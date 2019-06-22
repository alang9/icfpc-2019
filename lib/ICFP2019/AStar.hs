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
import Linear
import System.Random.MWC.Distributions
import System.Random.MWC

import ICFP2019.State
import ICFP2019.Action

import Debug.Trace

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
    (\(_, (pos, _)) -> HS.member pos (state0 ^. unwrapped)) (DoNothing, (state0 ^. wwPosition, state0 ^. activeFastWheels))
  return $ map fst states
  where
    neighbours :: (Point, Int)-> HS.HashSet (Action, (Point, Int))
    neighbours (pos, fw) = ns
      where
        fw' = max 0 $ fw - 1
        ns = HS.fromList
           [ if fw > 0 && open prob state0 (pos + d + d) then (m, (pos + d + d, fw')) else (m, (pos + d, fw')) | (m, d) <- restricted, open prob state0 (pos + d)]
    restricted =
      [ (MoveLeft, V2 (-1) 0)
      , (MoveRight, V2 1 0)
      , (MoveUp, V2 0 1)
      , (MoveDown, V2 0 (-1))
      ]
    heuristicDistance :: a -> Int
    heuristicDistance _ = 0

randomBfs :: forall m. (PrimMonad m) => Gen (PrimState m) -> MineProblem -> MineState -> m [Action]
randomBfs gen prob state0 = do
  states <- Data.Graph.AStar.aStarM (neighbours . snd) (\_ _ -> pure 1)
    (pure . heuristicDistance . snd)
    (\(_, (pos, _)) -> pure $ or [HS.member (pos + offset) (state0 ^. unwrapped) | offset <- manips]) (pure (DoNothing, (state0 ^. wwPosition, state0 ^. activeFastWheels)))
  return $ maybe [] (map fst) states
  where
    manips = HS.toList $ state0 ^. wwManipulators
    neighbours :: (Point, Int) -> m (HS.HashSet (Action, (Point, Int)))
    neighbours (pos, fw) = do
      res <- uniformShuffle restricted gen
      let fw' = max 0 $ fw - 1
      return $ HS.fromList
        [ if fw > 0 && open prob state0 (pos + d + d) then (m, (pos + d + d, fw')) else (m, (pos + d, fw'))
        | (m, d) <- VB.toList res, open prob state0 (pos + d)] -- the fast wheels approximation is not quite right
    restricted = VB.fromList
      [ (MoveLeft, V2 (-1) 0)
      , (MoveRight, V2 1 0)
      , (MoveUp, V2 0 1)
      , (MoveDown, V2 0 (-1))
      ]
    heuristicDistance :: a -> Int
    heuristicDistance _ = 0

boundedBfs :: forall m. (PrimMonad m) => Gen (PrimState m) -> Int -> MineProblem -> MineState -> m (Seq Action, MineState)
boundedBfs gen turns prob st
  | allWrapped st || turns <= 0 = pure (mempty, st)
  | otherwise = randomBfs gen prob st >>= \case
      [] -> error "bounded bad greedy"
      acts -> do
        let acts' = take turns acts
        let st' = foldl' (fmap (either (error "oops") id) . step prob) st acts'
        boundedBfs gen (turns - length acts') prob st' <&> _1 %~ (Seq.fromList acts' <>)
