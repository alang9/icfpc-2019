{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module ICFP2019.AStar where

import Control.Lens
import Data.Hashable
import qualified Data.HashSet as HS
import qualified Data.Graph.AStar
import Data.List
import Linear hiding (trace)
import qualified Data.HashMap.Lazy as HM
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashSet as HS
import Data.HashSet (HashSet)
import Data.List (foldl', foldl1')
import Data.OrdPSQ (OrdPSQ)
import qualified Data.OrdPSQ as PSQ
import GHC.Generics (Generic)

import ICFP2019.State
import ICFP2019.Action
import qualified ICFP2019.LineOfSight as LineOfSight

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

sortedFoldl' f acc0 workers = foldl' (\acc (wid, ws) -> f acc wid ws) acc0 $ sort $ HM.toList workers

bfsMultipleWorkers :: (OneWorkerState -> ([Action], Maybe Point)) -> FullState -> HM.HashMap Int [Action]
bfsMultipleWorkers ourBfs state0 =
  snd $ sortedFoldl' getPath (state0, HM.empty) workers 
  where 
    workers = state0 ^. fWorkers 
    getPath :: (FullState, HM.HashMap Int [Action]) -> Int -> WorkerState -> (FullState, HM.HashMap Int [Action])
    getPath (fullState, actions) workerIndex workerState =
      case target of
        Nothing -> (fullState, HM.insert workerIndex [DoNothing] actions)
        Just target' -> (fullState & fUnwrapped %~ remove (HS.toList $ HS.map ((+) target') manips), HM.insert workerIndex newActions actions)
      where
        remove [] hs = hs
        remove (x:xs) hs = remove xs $ HS.delete x hs
        manips = maybe (error "bfsMultipleWorkers: impossible") id $ workers ^? ix workerIndex . wManipulators
        (newActions, target) = ourBfs (makeOneWorker workerState fullState)

bfsMultipleWorkers' :: (HS.HashSet Point -> OneWorkerState -> ([Action], Maybe Point)) -> FullState ->
    HM.HashMap Int [Action] -> HS.HashSet Point -> (HM.HashMap Int [Action], HS.HashSet Point)
bfsMultipleWorkers' ourBfs state0 actionQueue alreadyPlanned =
  sortedFoldl' getPath (actionQueue, alreadyPlanned) $ workers
  where
    workers = state0 ^. fWorkers 
    getPath :: (HM.HashMap Int [Action], HS.HashSet Point) -> Int -> WorkerState -> (HM.HashMap Int [Action], HS.HashSet Point)
    -- TODO: pass planned things to ourBfs
    getPath (actions, planned) workerIndex workerState
      | HM.lookupDefault [] workerIndex actions == [] =
          case target of
            Nothing -> (HM.insert workerIndex (replicate 100 DoNothing) actions, planned)
            Just target' -> (HM.insert workerIndex newActions actions, HS.union planned $ HS.map ((+) target') manips)
      | otherwise = (actions, planned)
      where
        remove [] hs = hs
        remove (x:xs) hs = remove xs $ HS.delete x hs
        manips = maybe (error "bfsMultipleWorkers: impossible") id $ workers ^? ix workerIndex . wManipulators
        (newActions, target) = ourBfs planned (makeOneWorker workerState state0)

bfsToExactPositions :: [Point] -> Bool -> MineProblem -> OneWorkerState -> ([Action], Maybe Point)
bfsToExactPositions targets allowTurns prob state0 = maybe ([], Nothing) id $ do
  states <- Data.Graph.AStar.aStar (neighbours . snd)
    (\_ (_, ((newPos, newOrient), _, _)) -> V2 1 (negate $ length [ () | offset <- manips' newOrient, HS.member (newPos + offset) (state0 ^. unwrapped)]))
    heuristicDistance
    (\(_, ((pos, _), _, _)) -> elem pos targets)
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
    heuristicDistance :: (Action, ((Point, Int), Int, Int)) -> V2 Int
    heuristicDistance (_, ((pos, _), _, _)) = V2 (minimum [l1 k pos | k <- targets]) 0
    l1 (V2 x y) (V2 x' y') = abs (x - x') + abs (y - y')

bfs :: Bool -> MineProblem -> HS.HashSet Point -> OneWorkerState -> ([Action], Maybe Point)
bfs allowTurns prob excluded state0 = maybe ([], Nothing) id $ do
  states <- Data.Graph.AStar.aStar (neighbours . snd)
    (\_ (_, ((newPos, newOrient), _, _)) ->
       V2 1 (negate $ length [ () | offset <- manips' newOrient, HS.member (newPos + offset) (state0 ^. unwrapped), not (HS.member (newPos + offset) excluded)]))
    (heuristicDistance . snd)
    (\(_, ((pos, orient), _, _)) ->
       or [HS.member (pos + offset) (state0 ^. unwrapped) && not (HS.member (pos + offset) excluded) | offset <- manips' orient])
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

type WorkerId = Int
type PlannedCoverage = HM.HashMap Point (WorkerId, Int)

data BfsState = BfsState
  { bsAction :: !Action
  , bsPos :: !Point
  , bsOrient :: !Int
  , bsActiveFastWheels :: !Int
  , bsActiveDrill :: !Int
  } deriving (Eq, Ord, Generic, Show)

instance Hashable BfsState

data AStarX a c v = AStarX { visited  :: !(HashSet a),
                         waiting  :: !(OrdPSQ a c v),
                         score    :: !(HashMap a c),
                         memoHeur :: !(HashMap a c),
                         cameFrom :: !(HashMap (a, v) (a, v)),
                         end      :: !(Maybe (a, v)) }
    deriving Show

aStarXInit start val = AStarX { visited  = mempty,
                          waiting  = PSQ.singleton start 0 val,
                          score    = HM.singleton start 0,
                          memoHeur = mempty,
                          cameFrom = mempty,
                          end      = Nothing }

runAStarX :: forall a c v. (Hashable a, Ord a, Ord c, Num c, Eq v, Hashable v)
         => ((a, v) -> HashSet (a, v))     -- adjacencies in graph
         -> ((a, v) -> (a, v) -> c) -- distance function
         -> (a -> c)      -- heuristic distance to goal
         -> ((a, v) -> Bool)   -- goal
         -> a             -- starting vertex
         -> v
         -> AStarX a c v    -- final state
runAStarX graph dist heur goal start val = aStar' (aStarXInit start val)
  where
        aStar' :: AStarX a c v -> AStarX a c v
        aStar' s
          = case PSQ.minView (waiting s) of
              Nothing            -> s
              Just (x, _,  v, w') ->
                if goal (x, v)
                  then s { end = Just (x, v) }
                  else aStar' $ foldl' (expand (x, v))
                                       (s { waiting = w',
                                            visited = HS.insert x (visited s)})
                                       [neigh | neigh <- HS.toList (graph (x, v)), not (HS.member (fst neigh) (visited s))]
        expand :: (a, v) -> AStarX a c v -> (a, v) -> AStarX a c v
        expand (xa, xv) s (ya, yv)
          = let v = score s HM.! xa + dist (xa, xv) (ya, yv)
            in case PSQ.lookup ya (waiting s) of
                 Nothing -> link (xa, xv) (ya, yv) v
                              (s { memoHeur
                                     = HM.insert ya (heur ya) (memoHeur s) })
                 Just _  -> if v < score s HM.! ya
                              then link (xa, xv) (ya, yv) v s
                              else s
        link :: (a, v) -> (a, v) -> c -> AStarX a c v -> AStarX a c v
        link (xa, xv) (ya, yv) v s
           = s { cameFrom = HM.insert (ya, yv) (xa, xv) (cameFrom s),
                 score    = HM.insert ya v (score s),
                 waiting  = PSQ.insert ya (v + memoHeur s HM.! ya) yv (waiting s) }

-- | This function computes an optimal (minimal distance) path through a graph in a best-first fashion,
-- starting from a given starting point.
aStarX :: (Hashable a, Ord a, Ord c, Num c, Eq v, Hashable v) =>
         ((a, v) -> HashSet (a, v))     -- ^ The graph we are searching through, given as a function from vertices
                          -- to their neighbours.
         -> ((a, v) -> (a, v) -> c) -- ^ Distance function between neighbouring vertices of the graph. This will
                          -- never be applied to vertices that are not neighbours, so may be undefined
                          -- on pairs that are not neighbours in the graph.
         -> (a -> c)      -- ^ Heuristic distance to the (nearest) goal. This should never overestimate the
                          -- distance, or else the path found may not be minimal.
         -> ((a, v) -> Bool)   -- ^ The goal, specified as a boolean predicate on vertices.
         -> a             -- ^ The vertex to start searching from.
         -> v
         -> Maybe [(a, v)]     -- ^ An optimal path, if any path exists. This excludes the starting vertex.
aStarX graph dist heur goal start val
    = let s = runAStarX graph dist heur goal start val
      in case end s of
            Nothing -> Nothing
            Just e  -> Just (reverse . takeWhile (not . (== (start, val))) . iterate (cameFrom s HM.!) $ e)

invalidatingBfs :: Bool -> Int -> MineProblem -> PlannedCoverage -> OneWorkerState -> Maybe [(Action, [Point])]
invalidatingBfs allowTurns respect prob plannedCov state0 = do
  states <- aStarX neighbours
    (\_ s2 -> V3 1 (if HM.member (bsPos $ fst s2) (state0 ^. boosters) then -1 else 0) (negate $ length $ newCover s2))
    heuristicDistance
    (\s -> length (newCover s) > 0)
    initialState
    (state0 ^. timeSpent)
  return $ map (\s -> (bsAction (fst s), newCover s)) states
  where
    newCover (BfsState {bsPos, bsOrient}, gen) =
      [ pos
      | offset <- manips' bsOrient
      , let pos = bsPos + offset
      , let line = map (+ bsPos) $ LineOfSight.lineOfSight offset
      , all (open prob state0) line
      , HS.member pos (state0 ^. unwrapped)
      , maybe True (\(_, gen') -> gen < gen' - respect) $ HM.lookup pos plannedCov
      ]
    initialState = BfsState DoNothing (state0 ^. wwPosition) (state0 ^. wwOrientation) (state0 ^. activeFastWheels) (state0 ^. activeDrill)
    manips = HS.toList $ state0 ^. wwManipulators
    manips' orient = case mod (orient - state0 ^. wwOrientation) 4 of
      0 -> manips0
      1 -> manips1
      2 -> manips2
      3 -> manips3
    rot (V2 x y) = V2 y (-x)
    (!manips0):(!manips1):(!manips2):(!manips3):_ = iterate (map rot) $ manips
    neighbours :: (BfsState, Int) -> HS.HashSet (BfsState, Int)
    neighbours (BfsState {..}, gen) = HS.fromList $
      [ if fw > 0 && (open prob state0 (bsPos + d + d) || (inMine prob (bsPos + d + d) && ad > 0))
          then (BfsState m (bsPos + d + d) bsOrient fw' ad', gen + 1)
          else (BfsState m (bsPos + d) bsOrient fw' ad', gen + 1)
      | (m, d) <- moves, open prob state0 (bsPos + d) || (inMine prob (bsPos + d) && ad > 0)
      ] ++
      if allowTurns
        then
          [ (BfsState m bsPos (mod (bsOrient + d) 4) fw' ad', gen + 1)
          | (m, d) <- turns
          ]
        else []
      where
        ad = bsActiveDrill
        fw = bsActiveFastWheels
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
    heuristicDistance :: BfsState -> V3 Int
    heuristicDistance _ = V3 0 0 0
