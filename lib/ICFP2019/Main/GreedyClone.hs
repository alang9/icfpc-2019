{-# LANGUAGE BangPatterns #-}
module ICFP2019.Main.GreedyClone where

import qualified Data.Attoparsec.ByteString as AP
import qualified Data.ByteString.Char8 as C8
import Data.List
import System.Environment
import System.IO
import Control.Lens
import qualified Data.HashMap.Lazy as HM

import ICFP2019.State
import ICFP2019.Action
import ICFP2019.AStar

import Debug.Trace

combineActions :: HM.HashMap Int [Action] -> HM.HashMap Int [Action] -> HM.HashMap Int [Action]
combineActions = HM.unionWith (++) 

runSteps :: MineProblem -> FullState -> HM.HashMap Int [Action] -> IO (HM.HashMap Int [Action], Int)
runSteps prob !st actionsDone
  | allWrapped st = pure (actionsDone, st ^. fTimeSpent)
  | otherwise = do
      let actions = truncateActions $ bfsMultipleWorkers (bfs True prob) st
      traceShowM ("f", actions)
      let st' = if actionsAreMissing st actions 
                then error $ "bad greedy 5: " ++ show actions
                else either (error "oops") id $ stepUntilFinished prob st actions 
      runSteps prob st' (combineActions actionsDone actions)

runStepsClone :: MineProblem -> FullState -> HM.HashMap Int [Action] -> IO (HM.HashMap Int [Action], FullState)
runStepsClone prob !st actionsDone
  | mysteriousLocations == [] = pure (actionsDone, st)
  | cloneLocations == [] && HM.lookupDefault 0 Clone (st ^. fCollectedBoosters) == 0 = pure (actionsDone, st)
  | HM.lookupDefault 0 Clone (st ^. fCollectedBoosters) == 0 = do
      traceShowM ("foo2", st ^. fTimeSpent)
      let actions = truncateActions $ bfsMultipleWorkers (bfsToExactPositions cloneLocations True prob) st
      let st' = if actionsAreMissing st actions
                then error "bad greedy 4"
                else either (error "oops") id $ stepUntilFinished prob st actions
      runStepsClone prob st' (combineActions actionsDone actions)
  | otherwise = do
      traceShowM ("foo", st ^. fTimeSpent)
      let actions = bfsMultipleWorkers
            (bfsToExactPositions (cloneLocations ++ mysteriousLocations) True prob) st
      let actions2 = truncateActions $ snd $ HM.foldlWithKey' go (HM.lookupDefault 0 Clone $ st ^. fCollectedBoosters, actions) (st ^. fWorkers)
      traceShowM ("foo", actions, actions2, HM.lookup Clone $ st ^. fCollectedBoosters)
      let st' = if actionsAreMissing st actions2
                  then error $ "bad greedy 3: " ++ show (actions, actions2, cloneLocations, mysteriousLocations, st ^. fWorkers)
                  else either (error "oops") id $ stepUntilFinished prob st actions2
      traceShowM ("foo3", st' ^. fWorkers)
      runStepsClone prob st' (combineActions actionsDone actions2)
  where
    go (clonesAvail, acc) key workerState =
      if clonesAvail > 0 && elem (workerState ^. wPosition) mysteriousLocations
        then (clonesAvail - 1, HM.insertWith (++) key [DoClone] acc)
        else (clonesAvail, HM.insertWith (++) key [DoNothing] acc)
    mysteriousLocations = HM.keys $ HM.filter (==Mysterious) $ st ^. fBoosters
    cloneLocations = HM.keys (HM.filter (==Clone) $ st ^. fBoosters)

main :: IO ()
main = do
  [desc] <- getArgs
  descBs <- C8.readFile desc
  let Right (prob, state0) = AP.parseOnly initialParser descBs
  hSetBuffering stdout NoBuffering
  (actions1, state1) <- runStepsClone prob state0 HM.empty
  traceShowM ("actions1", actions1)
  (actionsDone, timeSpent) <- runSteps prob state1 actions1
  putStrLn $ serializeActions actionsDone
  traceShow timeSpent $ putStrLn ""
