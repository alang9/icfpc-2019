{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ICFP2019.DFS where

import Control.Lens
import qualified Data.HashSet as HS
import Data.List
import Data.Maybe
import Data.Ord
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Linear

import ICFP2019.Action
import ICFP2019.State

dfsOneStep :: MineProblem -> MineState -> [(Action, MineState)]
dfsOneStep prob state0 = mapMaybe go $ interestingActions prob state0
  where
    go act = case step prob state0 act of
      Left ex -> error $ "dfsOneStep: " ++ show ex
      Right state1 -> Just (act, state1)

boundedDfs :: forall m a. (Monad m, Ord a) => MineProblem -> (MineState -> m a) -> Int -> MineState -> m (Seq Action, MineState)
boundedDfs prob fitness maxDepth initial = do
  (acts, st, _) <- go maxDepth initial
  return (acts, st)
  where
  go :: Int -> MineState -> m (Seq Action, MineState, a)
  go !remDepth !st
    | allWrapped st = do
        !fit <- fitness st
        return (mempty, st, fit)
    | remDepth <= 0 = do
        !fit <- fitness st
        return (mempty, st, fit)
    | otherwise = case dfsOneStep prob st of
        [] -> do
          !fit <- fitness st
          return (mempty, st, fit)
        xs@(_:_) -> do
          xs' <- mapM (\(act, st') -> (_1 %~ (act Seq.:<|)) <$> go (remDepth - 1) st') xs
          return $ minimumBy (comparing $ \(_, _, score) -> score) xs'
  -- fitness' a = (\b -> (b, costHeuristic a)) <$> fitness a

costHeuristic :: MineState -> Int
costHeuristic state0 = case map go $ HS.toList $ missingTiles state0 of
    [] -> 0
    xs@(_:_) -> minimum xs
  where
    V2 x y = state0 ^. wwPosition
    go (V2 x' y') = abs (x - x') + abs (y - y')
