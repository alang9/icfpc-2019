{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ICFP2019.DFS where

import Control.Lens
import Control.Monad
import Control.Monad.Primitive
import qualified Data.HashSet as HS
import Data.List
import Data.Maybe
import Data.Ord
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Vector as VB
import Linear
import qualified System.Random.MWC.Distributions as MWC
import System.Random.MWC

import ICFP2019.Action
import ICFP2019.State

import Debug.Trace

dfsOneStep :: MineProblem -> MineState -> [(Action, MineState)]
dfsOneStep prob state0 = mapMaybe go $ interestingActions prob state0
  where
    go act = case step prob state0 act of
      Left ex -> error $ "dfsOneStep: " ++ show ex
      Right state1 -> Just (act, state1)

boundedDfs :: forall m a. (Monad m, Ord a, Show a) => MineProblem -> (MineState -> m a) -> Int -> MineState -> m (Seq Action, MineState, a)
boundedDfs prob fitness maxDepth initial = do
  fitnessInitial <- fitness initial
  go fitnessInitial maxDepth initial
  where
  go :: a -> Int -> MineState -> m (Seq Action, MineState, a)
  go initialFit !remDepth !st
    | allWrapped st = do
        !fit <- fitness st
        return (mempty, st, fit)
    | remDepth <= 0 = do
        !fit <- fitness st
        -- traceShowM ("foo", fit, initialFit)
        return (mempty, st, fit)
    | otherwise = case dfsOneStep prob st of
        [] -> do
          !fit <- fitness st
          return (mempty, st, fit)
        xs@(_:_) -> do
          xs' <- mapM (\(act, st') -> (_1 %~ (act Seq.:<|)) <$> go initialFit (remDepth - 1) st') xs
          return $ minimumBy (comparing $ \(_, _, score) -> score) xs'
  -- fitness' a = (\b -> (b, costHeuristic a)) <$> fitness a

randomBoundedDfs :: forall m a. (PrimMonad m, Ord a, Show a) => Gen (PrimState m) -> MineProblem -> (MineState -> m a) -> Int -> MineState -> m (Seq Action, MineState, a)
randomBoundedDfs gen prob fitness maxDepth initial = do
  fitnessInitial <- fitness initial
  go fitnessInitial maxDepth initial mempty
  where
  go :: a -> Int -> MineState -> Seq Action -> m (Seq Action, MineState, a)
  go initialFit !remDepth !st !acts
    | allWrapped st = do
        !fit <- fitness st
        return (acts, st, fit)
    | remDepth <= 0 = do
        -- traceShowM ("foo", initialFit, acts)
        !fit <- fitness st
        -- traceShowM ("foo2", fit, initialFit, acts)
        return (acts, st, fit)
    | otherwise = randomDfsOneStep gen prob st >>= \case
        [] -> do
          !fit <- fitness st
          return (mempty, st, fit)
        xs@(_:_) -> do
          -- when (remDepth == maxDepth) $ traceShowM ("top", map fst xs)
          xs' <- mapM (\(act, st') -> go initialFit (remDepth - 1) st' (acts Seq.:|> act)) xs
          return $ minimumBy (comparing $ \(_, _, score) -> score) xs'
  -- fitness' a = (\b -> (b, costHeuristic a)) <$> fitness a

randomDfsOneStep :: (PrimMonad m) => Gen (PrimState m) -> MineProblem -> MineState -> m [(Action, MineState)]
randomDfsOneStep gen prob state0 = VB.toList <$> MWC.uniformShuffle choices gen
  where
    choices = VB.fromList $ mapMaybe go $ interestingActions prob state0
    go act = case step prob state0 act of
      Left ex -> error $ "dfsOneStep: " ++ show ex
      Right state1 -> Just (act, state1)

costHeuristic :: MineState -> Int
costHeuristic state0 = case map go $ HS.toList $ missingTiles state0 of
    [] -> 0
    xs@(_:_) -> minimum xs
  where
    V2 x y = state0 ^. wwPosition
    go (V2 x' y') = abs (x - x') + abs (y - y')
