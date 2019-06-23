{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module ICFP2019.AStar where

import Control.Lens
import Control.Monad.Primitive
import Data.List
import qualified Data.HashSet as HS
import qualified Data.Graph.AStar
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import qualified Data.Vector as VB
import Linear hiding (trace)
import System.Random.MWC.Distributions
import System.Random.MWC
import qualified Data.HashMap.Lazy as HM
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashSet as HS
import Data.HashSet (HashSet)
import Data.List (foldl', foldl1')

import ICFP2019.State
import ICFP2019.Action

import Debug.Trace

-- aStar :: MineProblem -> MineState -> Point -> ([Action], MineState)
-- aStar prob state0 target = maybe ([], state0) id $ do
--   states <- Data.Graph.AStar.aStar (neighbours . snd) (\_ _ -> 1)
--     (heuristicDistance . snd)
--     (\(_, st) -> remainingTiles st < remainingTiles state0) (DoNothing, state0)
--   return (map fst states, snd $ last states)
--   where
--     neighbours st = HS.fromList $ (\act -> either (error "Impossible") ((,) act) $ step prob st act) <$> interestingActions prob st
--     dist1 (V2 x y) (V2 x' y') = abs (x - x') + abs (y - y')
--     -- TODO: Needs to support teleporters
--     -- TODO: Needs to account for rotation
--     heuristicDistance :: MineState -> Int
--     heuristicDistance st = d
--       where
--         d = minimum
--            [ dist1 (st ^. wwPosition + m) target
--            | m <- HS.toList $ st ^. wwManipulators
--            ]

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast [x] = Just x
safeLast (_ : rest) = safeLast rest

truncateActions :: HM.HashMap Int [Action] -> HM.HashMap Int [Action]
truncateActions actions =
  fmap (take minCount) actions
  where 
    minCount :: Int
    minCount = foldl1' min $ fmap length $ HM.elems actions 

bfsMultipleWorkers :: Bool -> MineProblem -> FullState -> HM.HashMap Int [Action]
bfsMultipleWorkers allowTurns prob state0 =
  snd $ HM.foldlWithKey' getPath (state0, HM.empty) workers 
  where 
    workers = state0 ^. fWorkers 
    getPath :: (FullState, HM.HashMap Int [Action]) -> Int -> WorkerState -> (FullState, HM.HashMap Int [Action])
    getPath (fullState, actions) workerIndex workerState = 
      case target of
        Nothing -> (fullState, actions)
        Just target -> (fullState & fUnwrapped %~ HS.delete target, HM.insert workerIndex newActions actions)
      where
        (newActions, target) = bfs allowTurns prob (makeOneWorker workerState fullState)
  

bfs :: Bool -> MineProblem -> OneWorkerState -> ([Action], Maybe Point)
bfs allowTurns prob state0 = maybe ([], Nothing) id $ do
  states <- Data.Graph.AStar.aStar (neighbours . snd)
    (\_ (_, ((newPos, newOrient), _, _)) -> V2 1 (negate $ length [ () | offset <- manips' newOrient, HS.member (newPos + offset) (state0 ^. unwrapped)]))
    (heuristicDistance . snd)
    (\(_, ((pos, orient), _, _)) -> or [HS.member (pos + offset) (state0 ^. unwrapped) | offset <- manips' orient])
    (DoNothing, ((state0 ^. wwPosition, state0 ^. wwOrientation), state0 ^. activeFastWheels, state0 ^. activeDrill))
  let actions = map fst states 
  let target = fmap (\(_action, ((finalPos,_),_,_)) -> finalPos) $ safeLast states
  return (actions, target)
  where
    manips = HS.toList $ state0 ^. wwManipulators
    manips' orient = case mod (orient - state0 ^. wwOrientation) 4 of
      0 -> manips0
      1 -> manips1
      2 -> manips2
      3 -> manips3
    rot (V2 x y) = V2 y (-x)
    (!manips0):(!manips1):(!manips2):(!manips3):_ = iterate (map rot) $ manips
    neighbours :: ((Point, Int), Int, Int) -> HS.HashSet (Action, ((Point, Int), Int, Int))
    neighbours ((pos, orient), fw, ad) = HS.fromList $
      [ if fw > 0 && (open prob state0 (pos + d + d) || (inMine prob (pos + d + d) && ad > 0))
          then (m, ((pos + d + d, orient), fw', ad'))
          else (m, ((pos + d, orient), fw', ad'))
      | (m, d) <- moves, open prob state0 (pos + d) || (inMine prob (pos + d) && ad > 0)
      ] ++
      if allowTurns
        then
          [ (m, ((pos, mod (orient + d) 4), fw', ad'))
          | (m, d) <- turns
          ]
        else []
      where
        fw' = max 0 $ fw - 1
        ad' = max 0 $ ad - 1
    moves =
      [ (MoveLeft, V2 (-1) 0)
      , (MoveRight, V2 1 0)
      , (MoveUp, V2 0 1)
      , (MoveDown, V2 0 (-1))
      ]
    turns =
      [ (TurnCW, 1)
      , (TurnCCW, -1)
      ]
    heuristicDistance :: a -> V2 Int
    heuristicDistance _ = V2 0 0

-- randomBfs :: forall m. (PrimMonad m) => Gen (PrimState m) -> MineProblem -> MineState -> m [Action]
-- randomBfs gen prob state0 = do
--   states <- Data.Graph.AStar.aStarM (neighbours . snd) (\_ _ -> pure 1)
--     (pure . heuristicDistance . snd)
--     (\(_, (pos, _, _)) -> pure $ or [HS.member (pos + offset) (state0 ^. unwrapped) | offset <- manips])
--     (pure (DoNothing, (state0 ^. wwPosition, state0 ^. activeFastWheels, state0 ^. activeDrill)))
--   -- traceShowM (map snd <$> states)
--   return $ maybe [] (map fst) states
--   where
--     manips = HS.toList $ state0 ^. wwManipulators
--     neighbours :: (Point, Int, Int) -> m (HS.HashSet (Action, (Point, Int, Int)))
--     neighbours (pos, fw, ad) = do
--       res <- uniformShuffle restricted gen
--       let fw' = max 0 $ fw - 1
--       let ad' = max 0 $ ad - 1
--       return $ HS.fromList
--         [ if fw > 0 && (open prob state0 (pos + d + d) || (inMine prob (pos + d + d) && ad > 0)) then (m, (pos + d + d, fw', ad')) else (m, (pos + d, fw', ad'))
--         | (m, d) <- VB.toList res, open prob state0 (pos + d) || (inMine prob (pos + d) && ad > 0)]
--     restricted = VB.fromList
--       [ (MoveLeft, V2 (-1) 0)
--       , (MoveRight, V2 1 0)
--       , (MoveUp, V2 0 1)
--       , (MoveDown, V2 0 (-1))
--       ]
--     heuristicDistance :: a -> Int
--     heuristicDistance _ = 0
-- 
-- boundedBfs :: forall m. (PrimMonad m) => Gen (PrimState m) -> Int -> MineProblem -> MineState -> m (Seq Action, MineState)
-- boundedBfs gen turns prob st
--   | allWrapped st || turns <= 0 = pure (mempty, st)
--   | otherwise = randomBfs gen prob st >>= \case
--       [] -> error "bounded bad greedy"
--       acts
--         | length acts >= turns -> do
--             let st' = foldl' (fmap (either (error "oops1") id) . step prob) st acts
--             return (Seq.fromList acts, st')
--         | otherwise -> do
--             let (st', acceptedActs) = foldl' (\(oldSt, acc) act -> case step prob oldSt act of Left ex -> traceShow ("error", ex) (oldSt, acc); Right newSt -> (newSt, act:acc) ) (st, []) acts
--             boundedBfs gen (turns - length acceptedActs) prob st' <&> _1 %~ (Seq.fromList (reverse acceptedActs) <>)
