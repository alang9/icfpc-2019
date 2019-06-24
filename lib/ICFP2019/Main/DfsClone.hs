{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
module ICFP2019.Main.DfsClone where

import qualified Data.Attoparsec.ByteString as AP
import qualified Data.ByteString.Char8 as C8
import Data.Foldable
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import System.Environment
import System.IO
import Control.Lens
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS

import ICFP2019.State
import ICFP2019.Action
import ICFP2019.AStar
import ICFP2019.Booster
import ICFP2019.DFS

import Debug.Trace

appendActions :: HM.HashMap Int (Seq Action) -> HM.HashMap Int Action -> HM.HashMap Int (Seq Action)
appendActions acts newActs = HM.unionWith (<>) acts (HM.map Seq.singleton newActs)

combineActions :: HM.HashMap Int (Seq Action) -> HM.HashMap Int (Seq Action) -> HM.HashMap Int (Seq Action)
combineActions = HM.unionWith (<>)

runSteps :: MineProblem -> FullState -> HM.HashMap WorkerId (Seq Action) ->
  HM.HashMap WorkerId [(Action, [Point])] -> -- Actions planned for each worker, and the points that they should wrap
  HM.HashMap Point (WorkerId, Int) -> -- points that are planned to be wrapped, and which worker plans to wrap it, and in which generation
  IO (HM.HashMap Int (Seq Action), Int)
runSteps prob !st actionsDone actionQueue plannedCoverage
  | allWrapped st = pure (actionsDone, st ^. fTimeSpent)
  | otherwise = do
      let (immediateActions, newQueue, newPlannedCoverage, finalSt') = sortedFoldl' (foo prob) (mempty, actionQueue, plannedCoverage, st) $ st ^. fWorkers
      let finalSt = tickTimeAll finalSt'
      -- let st' = if missingActions st immediateActions
      --           then error $ "bad greedy 5: " ++ show immediateActions
      --           else either (\exc -> error $ "oops 12 " ++ show exc) id $ stepAllWorkers' prob st immediateActions
      traceShowM ("f", HS.size $ finalSt ^. fUnwrapped, immediateActions, HM.map length newQueue, finalSt ^. fCollectedBoosters, HM.map (view wPosition) $ finalSt ^. fWorkers)
      runSteps prob finalSt (appendActions actionsDone immediateActions) newQueue newPlannedCoverage

missingActions :: FullState -> HM.HashMap WorkerId Action -> Bool
missingActions state actions =
  foldl' checkNonemptyActions False (HM.keys $ state ^. fWorkers)
  where
    checkNonemptyActions :: Bool -> Int -> Bool
    checkNonemptyActions prev i = case HM.lookup i actions of
      Nothing -> True
      Just _ -> prev

foo ::
  MineProblem ->
  (HM.HashMap WorkerId Action, HM.HashMap WorkerId [(Action, [Point])], PlannedCoverage, FullState) ->
  WorkerId ->
  a -> -- [(Action, [Point])] ->
  (HM.HashMap WorkerId Action, HM.HashMap WorkerId [(Action, [Point])], PlannedCoverage, FullState)
foo prob (actionAcc, queueAcc, plannedCoverage, fullState) workerId _ = case currentQueue of
  ((act, _):remQueue) ->
        (HM.insert workerId act actionAcc, HM.insert workerId remQueue queueAcc, plannedCoverage, applyAction "2" act)
  _ -> trace ("finding new work for: " ++ show workerId) $ case newPlan of
    ((act, _):remQueue) -> fp $ pp (HM.insert workerId act actionAcc, HM.insert workerId remQueue cleanedQueueAcc, invalidatedCoverage, applyAction "1" act) -- Maybe remove new invalidate guys from queue and coverage
    [] -> (HM.insert workerId DoNothing actionAcc, HM.insert workerId (replicate 200 (DoNothing, [])) queueAcc, cleanedCoverage, fullState) -- TODO: check cleanedCoverage == invalidatedCoverage
  where
    applyAction str act = either (\exc -> error $ show ("ntoehu: ", str, workerId, exc, act)) (maybe (error "omg") id) $
      stepSingleWorker prob fullState workerId act
    pp = case invalidated of
      [] -> id
      _ -> trace $ unwords [show workerId, " stole work from ", show invalidated]
    fp = traceShow ("found plan of length", length newPlan, newPlan, thisWorkerState, fullState ^. fCollectedBoosters, HM.map (view wPosition) $ fullState ^. fWorkers)
    currentQueue = maybe [] id $ HM.lookup workerId queueAcc
    invalidatedCoverage = foldl' (\cov wid -> HM.filter ((/= wid) . fst) cov) newPlannedCoverage invalidated
    cleanedCoverage = HM.filter ((/= workerId) . fst) plannedCoverage
    thisWorkerState = maybe (error "never") id $ fullState ^? fWorkers . ix workerId
    invalidated :: [WorkerId]
    (newPlannedCoverage, invalidated) = updateCoverage workerId (fullState ^. fTimeSpent) newPlan cleanedCoverage
    newPlan = maybe [] id $
        findPlanDfs prob cleanedCoverage $ makeOneWorker thisWorkerState fullState
    cleanedQueueAcc = foldl' (\q wid -> HM.delete wid q) queueAcc invalidated

updateCoverage :: WorkerId -> Int -> [(Action, [Point])] -> PlannedCoverage -> (PlannedCoverage, [WorkerId])
updateCoverage myId gen plan cov0 = foldl' go (cov0, []) $ concat $ zipWith (\g (_, pts) -> (,) g <$> pts) [gen+1..] plan
  where
    go :: (PlannedCoverage, [WorkerId]) -> (Int, Point) -> (PlannedCoverage, [WorkerId])
    go (cov, invalids) (thisGen, pt) = case HM.lookup pt cov of
      Nothing -> (HM.insert pt (myId, thisGen) cov, invalids)
      Just (otherId, _) -> (HM.insert pt (myId, thisGen) (HM.filter ((/=) otherId . fst) cov), otherId:invalids)

findPlan :: MineProblem -> PlannedCoverage -> OneWorkerState -> Maybe [(Action, [Point])]
findPlan = invalidatingBfs False 0

findPlanDfs :: MineProblem -> PlannedCoverage -> OneWorkerState -> Maybe [(Action, [Point])]
findPlanDfs prob cov initial = boundedDfs prob (boundedInvalidatingBfs 40 False 20 prob cov) 1 initial

boundedInvalidatingBfs :: Int -> Bool -> Int -> MineProblem -> PlannedCoverage -> OneWorkerState -> Maybe [(Action, [Point])]
boundedInvalidatingBfs stoppingDepth allowTurns respect prob cov initialSt = go stoppingDepth initialSt
  where
    go depth st = case invalidatingBfs allowTurns respect prob cov st of
      Nothing -> Nothing
      Just [] -> error "what"
      Just path
        | length path >= depth -> Just path
        | otherwise -> Just $ path ++ nextPaths
        where
          nextPaths = maybe [] id $ go (depth - length path) newSt
          newSt = foldl' (\st' (act, _) -> either (\exc -> error $ "boundedInvalidatingBfs: " ++ show exc) id (stepAndTick prob st' act)) st path

runStepsClone :: MineProblem -> FullState -> HM.HashMap Int (Seq Action) -> IO (HM.HashMap Int (Seq Action), FullState)
runStepsClone prob !st actionsDone
  | mysteriousLocations == [] = pure (actionsDone, st)
  | cloneLocations == [] && HM.lookupDefault 0 Clone (st ^. fCollectedBoosters) == 0 = pure (actionsDone, st)
  | HM.lookupDefault 0 Clone (st ^. fCollectedBoosters) == 0 = do
      traceShowM ("foo2", st ^. fTimeSpent)
      let actions = truncateActions $ bfsMultipleWorkers (bfsToExactPositions cloneLocations True prob) st
      let st' = if actionsAreMissing st actions
                then error "bad greedy 4"
                else either (error "oops 9") id $ stepUntilFinished prob st actions
      runStepsClone prob st' (combineActions actionsDone $ HM.map Seq.fromList actions)
  | otherwise = do
      traceShowM ("foo", st ^. fTimeSpent)
      let preActions = bfsMultipleWorkers
            (bfsToExactPositions (cloneLocations ++ mysteriousLocations) True prob) st
      let actions = flip HM.map preActions $ \case
            [DoNothing] -> []
            xs -> xs
      let actions2 = truncateActions $ snd $ sortedFoldl' go (HM.lookupDefault 0 Clone $ st ^. fCollectedBoosters, actions) (st ^. fWorkers)
      traceShowM ("foo4", actions, actions2, HM.lookup Clone $ st ^. fCollectedBoosters)
      let st' = if actionsAreMissing st actions2
                  then error $ "bad greedy 3: " ++ show (actions, actions2, cloneLocations, mysteriousLocations, st ^. fWorkers)
                  else either (error "oops 3") id $ stepUntilFinished prob st actions2
      traceShowM ("foo3", st' ^. fWorkers)
      runStepsClone prob st' (combineActions actionsDone $ HM.map Seq.fromList actions2)
  where
    go (clonesAvail, acc) key workerState =
      if clonesAvail > 0 && elem (workerState ^. wPosition) mysteriousLocations
        then (clonesAvail - 1, HM.insertWith (++) key [DoClone] acc)
        else (clonesAvail, HM.insertWith (++) key [DoNothing] acc)
    mysteriousLocations = HM.keys $ HM.filter (==Mysterious) $ st ^. fBoosters
    cloneLocations = HM.keys (HM.filter (==Clone) $ st ^. fBoosters)

main :: IO ()
main = do
  [desc, buy] <- getArgs
  descBs <- C8.readFile desc
  boosterBag <- readBoosterBag buy
  let Right (prob, state0) = AP.parseOnly initialParser descBs
  hSetBuffering stdout NoBuffering
  (actions1, state1) <- runStepsClone prob (buyBoosters boosterBag state0) HM.empty
  traceShowM ("actions1", actions1)
  (actionsDone, ts) <- runSteps prob state1 actions1 mempty mempty
  putStrLn $ serializeActions $ HM.map toList actionsDone
  traceShow ts $ putStrLn ""
