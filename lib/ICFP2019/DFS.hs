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

dfsOneStep :: MineProblem -> OneWorkerState -> [(Action, [Point], OneWorkerState)]
dfsOneStep prob state0 = mapMaybe go $ interestingActions prob state0
  where
    go act = case stepAndTick prob state0 act of
      Left ex -> error $ "dfsOneStep: " ++ show ex
      Right state1 -> Just (act, HS.toList (HS.difference (state0 ^. unwrapped) (state1 ^. unwrapped)), state1)

boundedDfs :: MineProblem -> (OneWorkerState -> Maybe [(Action, [Point])]) -> Int -> OneWorkerState -> Maybe [(Action, [Point])]
boundedDfs prob descend maxDepth initial =
    go maxDepth initial
  where
    fitness = sum . map (length . snd)
    initialFitness = maybe 0 fitness $ descend initial
    go remDepth st
      | remDepth <= 0 = descend st
      | otherwise =
            if fitness best > 0 then Just best else Nothing
          where
            best = maximumBy (comparing fitness) options
            neighbours = dfsOneStep prob st
            options :: [[(Action, [Point])]]
            options = map (\(act, covered, st') -> (:) (act, covered) $ maybe [] id $ go (remDepth - 1) st' ) neighbours
