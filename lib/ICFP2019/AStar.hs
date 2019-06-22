module ICFP2019.AStar where

import Control.Lens
import qualified Data.HashSet as HS
import qualified Data.Graph.AStar
import Linear

import ICFP2019.State
import ICFP2019.Action

aStar :: MineProblem -> MineState -> Point -> ([Action], MineState)
aStar prob state0 target = maybe ([], state0) id $ do
  states <- Data.Graph.AStar.aStar (neighbours . snd) (\_ _ -> 1)
    (heuristicDistance . snd)
    (\(_, st) -> remainingTiles st < remainingTiles state0) (DoNothing, state0)
  return (map fst states, snd $ last states)
  where
    neighbours st = HS.fromList $ (\act -> either (error "Impossible") ((,) act) $ step prob st act) <$> interestingActions prob st
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

bfs :: MineProblem -> MineState -> [Action]
bfs prob state0 = maybe [] id $ do
  states <- Data.Graph.AStar.aStar (neighbours . snd) (\_ _ -> 1)
    (heuristicDistance . snd)
    (\(_, pos) -> HS.member pos (state0 ^. unwrapped)) (DoNothing, state0 ^. wwPosition)
  return $ map fst states
  where
    neighbours :: Point -> HS.HashSet (Action, Point)
    neighbours pos = HS.fromList
      [ (m, pos + d) | (m, d) <- restricted, open prob state0 (pos + d)]
    restricted =
      [ (MoveLeft, V2 (-1) 0)
      , (MoveRight, V2 1 0)
      , (MoveUp, V2 0 1)
      , (MoveDown, V2 0 (-1))
      ]
    heuristicDistance :: a -> Int
    heuristicDistance _ = 0
