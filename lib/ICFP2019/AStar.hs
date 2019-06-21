module ICFP2019.AStar where

import Control.Lens
import qualified Data.HashSet as HS
import qualified Data.Graph.AStar
import Linear

import ICFP2019.State
import ICFP2019.Action

aStar :: MineState -> Point -> ([Action], MineState)
aStar state0 target = maybe ([], state0) id $ do
  states <- Data.Graph.AStar.aStar (neighbours . snd) (\_ _ -> 1)
    (heuristicDistance . snd)
    (\(_, st) -> remainingTiles st < remainingTiles state0) (DoNothing, state0)
  return (map fst states, snd $ last states)
  where
    neighbours st = HS.fromList $ (\act -> either (error "Impossible") ((,) act) $ step st act) <$> interestingActions st
    dist1 (V2 x y) (V2 x' y') = abs (x - x') + abs (y - y')
    -- TODO: Needs to support teleporters
    -- TODO: Needs to account for rotation
    heuristicDistance :: MineState -> Int
    heuristicDistance st = d
      where
        d = minimum
           [ dist1 (st ^. wwPosition + m) target
           | m <- HS.toList $ st ^. wwManipulators
           ]
