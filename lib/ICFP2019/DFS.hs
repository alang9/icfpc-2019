module ICFP2019.DFS where

import Control.Arrow
import Data.List
import Data.Ord
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import ICFP2019.Action
import ICFP2019.State

dfsOneStep :: MineState -> [(Action, MineState)]
dfsOneStep state0 = map (\act -> (act, either (\ex -> error $ "dfsOneStep " ++ show ex) id $ step state0 act )) $ interestingActions state0

boundedDfs :: (MineState -> Double) -> Int -> MineState -> (Seq Action, MineState)
boundedDfs fitness maxDepth initial
  | allWrapped initial = (mempty, initial)
  | maxDepth <= 0 = (mempty, initial)
  | otherwise = case dfsOneStep initial of
      [] -> (mempty, initial)
      xs@(_:_) -> minimumBy (comparing $ \(acts, st) -> (fitness st, length acts) )$ map (\(act, st) -> first (act Seq.:<|) $ boundedDfs fitness (maxDepth - 1) st) xs
