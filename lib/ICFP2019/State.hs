{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module ICFP2019.State where

import Control.Applicative
import Control.Lens
import qualified Data.Attoparsec.ByteString.Char8 as AP
import Data.Hashable
import qualified Data.HashMap.Lazy as HM
import Data.HashMap.Lazy (HashMap)
import Data.List (foldl')
import qualified Data.HashSet as HS
import Data.HashSet (HashSet)
import qualified Data.Vector.Unboxed as VU
import GHC.Generics (Generic)
import Linear hiding (trace)
import Data.List (sort)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Debug.Trace (trace, traceShow, traceShowM)

import qualified ICFP2019.Shape as Shape
import qualified ICFP2019.LineOfSight as LineOfSight
import ICFP2019.Shape (Shape)
import ICFP2019.Action
import ICFP2019.Booster

type Point = V2 Int

type RelPos = V2 Int

type TimeRemaining = Int

data MineProblem = MineProblem
  { _boundary :: !Shape
  } deriving (Show)

makeLenses ''MineProblem

-- INVARIANT: wrapped and blocked and unwrapped should be mutually exclusive, and their union should be `Shape.toHashSet boundary`
data OneWorkerState = OneWorkerState
  { _wwPosition :: !Point
  , _wwOrientation :: !Int
  , _wwManipulators :: !(HashSet RelPos)
  , _unwrapped :: !(HashSet Point)
  , _blocked :: !(HashSet Point)
  , _boosters :: !(HashMap Point Booster)
  , _collectedBoosters :: !(HashMap Booster Int)
  , _totalBoosters :: !Int
  , _activeFastWheels :: !TimeRemaining
  , _activeDrill :: !TimeRemaining
  , _timeSpent :: !Int
  , _beaconLocations :: !(HashSet Point)
  } deriving (Show, Eq, Ord, Generic)

instance Hashable OneWorkerState

makeLenses ''OneWorkerState

data WorkerState = WorkerState
  { _wPosition :: !Point
  , _wOrientation :: !Int
  , _wManipulators :: !(HashSet RelPos)
  , _wActiveFastWheels :: !TimeRemaining
  , _wActiveDrill :: !TimeRemaining
  } deriving (Show, Eq, Ord, Generic)

makeLenses ''WorkerState

data FullState = FullState
  { _fWorkers :: !(HashMap Int WorkerState)
  , _fUnwrapped :: !(HashSet Point)
  , _fBlocked :: !(HashSet Point)
  , _fBoosters :: !(HashMap Point Booster)
  , _fCollectedBoosters :: !(HashMap Booster Int)
  , _fTimeSpent :: !Int
  , _fBeaconLocations :: !(HashSet Point)
  , _fTotalBoosters :: !Int
  } deriving (Show, Eq, Ord, Generic)

makeLenses ''FullState

data ActionException
  = NoBooster
  | CantAttachManipulator
  | NoSpace
  | InvalidShift
  | InvalidCloneLocation
  deriving (Show)

makeOneWorker :: WorkerState -> FullState -> OneWorkerState 
makeOneWorker worker full =
  OneWorkerState
    { _wwPosition = worker ^. wPosition
    , _wwOrientation = worker ^. wOrientation
    , _wwManipulators = worker ^. wManipulators
    , _unwrapped = full ^. fUnwrapped
    , _blocked = full ^. fBlocked 
    , _boosters = full ^. fBoosters 
    , _collectedBoosters = full ^. fCollectedBoosters
    , _activeFastWheels = worker ^. wActiveFastWheels
    , _activeDrill = worker ^. wActiveDrill 
    , _timeSpent = full ^. fTimeSpent 
    , _beaconLocations = full ^. fBeaconLocations
    , _totalBoosters = full ^. fTotalBoosters
    }

selectWorker :: FullState -> Int -> Maybe OneWorkerState
selectWorker fullState workerIndex = fmap (\w -> makeOneWorker w fullState) $ workerstate
  where
    workerstate :: Maybe WorkerState
    workerstate = HM.lookup workerIndex (fullState ^. fWorkers)

updateFullState :: Int -> OneWorkerState -> FullState -> FullState
updateFullState workerIndex oneworkerstate fullstate =
  fullstate 
  & fUnwrapped .~ oneworkerstate ^. unwrapped
  & fBlocked .~ oneworkerstate ^. blocked
  & fBoosters .~ oneworkerstate ^. boosters
  & fCollectedBoosters .~ oneworkerstate ^. collectedBoosters
  & fTimeSpent .~ oneworkerstate ^. timeSpent
  & fBeaconLocations .~ oneworkerstate ^. beaconLocations
  & fWorkers %~ HM.insert workerIndex worker
  & fTotalBoosters .~ oneworkerstate ^. totalBoosters
  where 
    worker = WorkerState
      { _wPosition = oneworkerstate ^. wwPosition 
      , _wOrientation = oneworkerstate ^. wwOrientation 
      , _wManipulators = oneworkerstate ^. wwManipulators 
      , _wActiveFastWheels = oneworkerstate ^. activeFastWheels 
      , _wActiveDrill = oneworkerstate ^. activeDrill 
      }

updateFullStateWith :: (OneWorkerState -> OneWorkerState) -> FullState -> Int -> Maybe FullState
updateFullStateWith f fullstate workerIndex = case selectWorker fullstate workerIndex of
  Nothing -> Nothing
  Just result -> 
      Just $ updateFullState workerIndex (f result) fullstate
     
startingManip :: HashSet (V2 Int)
startingManip = HS.fromList [V2 0 0, V2 1 0, V2 1 1, V2 1 (-1)]

initialParser :: AP.Parser (MineProblem, FullState)
initialParser = do
    boundary0 <- shapeParser
    _ <- AP.char '#'
    wwPos <- pairParser
    _ <- AP.char '#'
    walls <- AP.sepBy shapeParser (AP.char ';')
    _ <- AP.char '#'
    boos <- AP.sepBy boosterParser (AP.char ';')
    let blok = foldMap Shape.toHashSet walls
    let wrap = mempty
    let mineProb = MineProblem boundary0
    let initialWorker = WorkerState
          { _wPosition = wwPos
          , _wOrientation = 0
          , _wManipulators = startingManip
          , _wActiveFastWheels = 0
          , _wActiveDrill = 0
          }
    let fullSt = FullState
          { _fWorkers = HM.singleton 0 initialWorker
          , _fUnwrapped = HS.difference (HS.difference (allTiles boundary0) wrap) blok
          , _fBlocked = blok
          , _fBoosters = HM.fromList boos
          , _fCollectedBoosters = mempty
          , _fTimeSpent = 0
          , _fBeaconLocations = mempty
          , _fTotalBoosters = 0
          }
    let initialSt = maybe (error "should be unreachable") id (updateFullStateWith (applyWrapped mineProb) fullSt 0) 
    -- traceShowM $ initialSt
    return (mineProb, initialSt)
  where
    allTiles sha = Shape.toHashSet sha
    boosterParser = do
      boosterCode <- AP.anyChar
      whichBooster <- maybe
        (fail $ "Unknown booster: " ++ show boosterCode)
        return
        (boosterCode ^? boosterFromCode)
      pt <- pairParser
      return (pt, whichBooster)
    shapeParser = do
      points <- AP.sepBy pairParser (AP.char ',')
      return $! Shape.fromPoints points
    pairParser = do
      _ <- AP.char '('
      x <- AP.signed AP.decimal
      _ <- AP.char ','
      y <- AP.signed AP.decimal
      _ <- AP.char ')'
      return $ V2 x y

buyBoosters :: BoosterBag -> FullState -> FullState
buyBoosters bb s = s
    & fCollectedBoosters %~ (unBoosterBag . (<> bb) . BoosterBag)

validNewManipulatorPositions :: OneWorkerState -> HashSet Point
validNewManipulatorPositions state0 =
    HS.filter (\newM -> maxRange (HS.insert newM (state0^.wwManipulators)) > oldRange) $
    HS.unions [HS.map ((+) dir) $ state0 ^. wwManipulators | dir <- dirs] `HS.difference` (state0 ^. wwManipulators)
  where
    oldRange :: Int
    oldRange = maxRange $ state0 ^. wwManipulators
    maxRange :: HashSet Point -> Int
    maxRange manip = max yRange xRange
      where
        xs = [m ^. _x | m <- HS.toList manip]
        ys = [m ^. _y | m <- HS.toList manip]
        xRange, yRange :: Int
        xRange = maximum xs - minimum xs
        yRange = maximum ys - minimum ys
    dirs = [V2 0 1, V2 0 (-1), V2 1 0, V2 (-1) 0]

step :: MineProblem -> OneWorkerState -> Action -> Either ActionException OneWorkerState
step prob preState0 act = case act of
  DoNothing -> pure $ state0
  MoveUp -> doMove (V2 0 1)
  MoveDown -> doMove (V2 0 (-1))
  MoveLeft -> doMove (V2 (-1) 0)
  MoveRight -> doMove (V2 1 0)
  TurnCW -> pure $ movePosition' (V2 0 0) prob $ state0
    & wwManipulators %~ HS.map (\(V2 x y) -> V2 y (-x))
    & wwOrientation %~ mod 4 . succ
  TurnCCW ->
    pure $ movePosition' (V2 0 0) prob $ state0
    & wwManipulators %~ HS.map (\(V2 x y) -> V2 (-y) x)
    & wwOrientation %~ mod 4 . pred
  AttachFastWheels -> if HM.lookupDefault 0 FastWheels (state0 ^. collectedBoosters) > 0
    then Right $ (state0
      & collectedBoosters %~ HM.adjust pred FastWheels)
      & activeFastWheels %~ (+) 51 -- 51 instead of 50 because we will advance the time after this move, but not use them
    else Left NoBooster
  AttachDrill -> if HM.lookupDefault 0 Drill (state0 ^. collectedBoosters) > 0
    then Right $ (state0
      & collectedBoosters %~ HM.adjust pred Drill)
      & activeDrill %~ (+) 31 -- 31 instead of 30 becauce we will advance the time after this move, but not use them
    else Left NoBooster
  AttachManipulator rp -> if HM.lookupDefault 0 Extension (state0 ^. collectedBoosters) > 0
    then
      if HS.member rp (validNewManipulatorPositions state0)
        then
          Right $ applyWrapped prob $ state0
            & wwManipulators %~ HS.insert rp
            & collectedBoosters %~ HM.adjust pred Extension
        else Left CantAttachManipulator
    else Left NoBooster
  Reset -> if HM.lookupDefault 0 Teleport (state0 ^. collectedBoosters) > 0
    then
      Right $ applyWrapped prob $ state0
            & beaconLocations %~ HS.insert (state0 ^. wwPosition)
            & collectedBoosters %~ HM.adjust pred Teleport
    else Left NoBooster
  Shift pos -> if HS.member pos (state0 ^. beaconLocations)
    then
      Right $ applyWrapped prob $ state0 & wwPosition .~ pos
    else Left InvalidShift
  where
    state0 = getBooster (preState0 ^. wwPosition) preState0
    doMove rp = maybe (Left NoSpace) Right $ do
      state1 <- movePosition rp prob state0
      if state0 ^. activeFastWheels > 0
        then do
          case movePosition rp prob state1 of
            Nothing -> return $ state1
            Just state2 -> return $ state2
        else return $ state1

stepAndTick :: MineProblem -> OneWorkerState -> Action -> Either ActionException OneWorkerState
stepAndTick prob state act = step prob state act
  <&> timeSpent %~ succ
  <&> activeDrill %~ max 0 . pred
  <&> activeFastWheels %~ max 0 . pred

passable :: MineProblem -> Point -> OneWorkerState -> Bool
passable prob pt state0 = inMine prob pt && (notWall || hasDrill)
  where
    hasDrill = state0 ^. activeDrill > 0
    notWall = not $ HS.member pt (view blocked state0)

inMine :: MineProblem -> Point -> Bool
inMine prob pt = pt `Shape.member` (prob ^. boundary)

open :: MineProblem -> OneWorkerState -> Point -> Bool
open prob state0 pt = inMine && notWall
  where
    inMine = pt `Shape.member` (prob ^. boundary)
    notWall = not $ HS.member pt (view blocked state0)

notWrapped :: OneWorkerState -> Point -> Bool
notWrapped state0 pt = HS.member pt (state0 ^. unwrapped)-- open prob state0 pt && not (HS.member pt (state0 ^. wrapped))

movePosition :: RelPos -> MineProblem -> OneWorkerState -> Maybe OneWorkerState
movePosition rp prob state0 =
  if passable prob pt state0
    then Just $ movePosition' rp prob state0
    else Nothing
  where
    pt = rp + state0 ^. wwPosition

movePosition' :: RelPos -> MineProblem -> OneWorkerState -> OneWorkerState
movePosition' rp prob state0 =
  -- getBooster pt $
      applyWrapped prob $ state0
            & wwPosition .~ pt
            & blocked %~ HS.delete pt
            & unwrapped %~ HS.insert pt
  where
    pt = rp + state0 ^. wwPosition

applyWrapped :: MineProblem -> OneWorkerState -> OneWorkerState
applyWrapped problem state0 = state0
  & unwrapped %~ (kill diff)
  where
    diff = HS.filter (notWrapped state0) targets
    targets = HS.filter (hasLineOfSightTo problem state0) $
        HS.map (+ state0 ^. wwPosition) $ state0 ^. wwManipulators

    kill victims uw = foldl' (flip HS.delete) uw victims

hasLineOfSightTo :: MineProblem -> OneWorkerState -> Point -> Bool
hasLineOfSightTo problem state target =
    let delta = target - state ^. wwPosition
        tiles = map (+ state ^. wwPosition) (LineOfSight.lineOfSight delta) in
    all (open problem state) tiles

allWrapped :: FullState -> Bool
allWrapped = HS.null . view fUnwrapped

remainingTiles :: OneWorkerState -> Int
remainingTiles state0 = HS.size (state0 ^. unwrapped)

numTiles :: MineProblem -> Int
numTiles = VU.length . VU.filter id . view (boundary . Shape.points)

missingTiles :: FullState -> HashSet Point
missingTiles = view fUnwrapped

getBooster :: Point -> OneWorkerState -> OneWorkerState
getBooster pt state0 = case HM.lookup pt $ state0 ^. boosters of
  Nothing -> state0
  Just Mysterious -> state0
  Just b -> state0
    & boosters %~ HM.delete pt
    & collectedBoosters %~ HM.insertWith (+) b 1
    & totalBoosters %~ (+) 1

tickTimeAll :: FullState -> FullState
tickTimeAll state0 = state0
  & fTimeSpent %~ (+) 1
  & fWorkers %~ HM.map decTimeRemaining
  where
    decTimeRemaining workerState = workerState 
      & wActiveFastWheels  %~ max 0 . subtract 1 
      & wActiveDrill %~ max 0 . subtract 1

interestingActions :: MineProblem -> OneWorkerState -> [Action]
interestingActions prob state0 = concat
  [ if passable prob (state0 ^. wwPosition + V2 (-1) 0) state0 then [MoveLeft] else []
  , if passable prob (state0 ^. wwPosition + V2 1 0) state0 then [MoveRight] else []
  , if passable prob (state0 ^. wwPosition + V2 0 (-1)) state0 then [MoveDown] else []
  , if passable prob (state0 ^. wwPosition + V2 0 1) state0 then [MoveUp] else []
  , [TurnCW]
  , [TurnCCW]
  , if maybe False (> 0) $ HM.lookup FastWheels (state0 ^. collectedBoosters) then [AttachFastWheels] else []
  , if maybe False (> 0) $ HM.lookup Drill (state0 ^. collectedBoosters) then [AttachDrill] else []
  , if maybe False (> 0) (HM.lookup Teleport (state0 ^. collectedBoosters))
        && (HM.lookup (state0 ^. wwPosition) (state0 ^. boosters) /= Just Mysterious)
      then [Reset]
      else []
  , if maybe False (> 0) $ HM.lookup Extension (state0 ^. collectedBoosters) then
      [AttachManipulator pt | pt <- HS.toList (validNewManipulatorPositions state0)] else []
  , [Shift loc | loc <- HS.toList (state0 ^. beaconLocations)]
  , case HM.lookup (state0 ^. wwPosition) (state0 ^. boosters) of
      Just Mysterious -> if maybe False (> 0) $ HM.lookup Clone (state0 ^. collectedBoosters) then [DoClone] else []
      _ -> []
--  , [DoNothing]
  ]

interestingActionsAll :: MineProblem -> FullState -> HashMap Int [Action]
interestingActionsAll prob state0 =
  HM.mapWithKey (\k v -> getActions k) (state0 ^. fWorkers)
  where
    getActions :: Int -> [Action]
    getActions i = case selectWorker state0 i of
      Nothing -> []
      Just oneWorkerState -> interestingActions prob oneWorkerState 

stepSingleWorker :: MineProblem -> FullState -> Int -> Action -> Either ActionException (Maybe FullState)
stepSingleWorker prob fullState workerIndex action = case selectWorker fullState workerIndex of
  Nothing -> Right Nothing 
  Just oneWorkerState -> case step prob oneWorkerState action of
    Left error -> Left error
    Right oneWorkerState1 -> Right $ Just $ (updateFullState workerIndex oneWorkerState1 fullState)

doClone :: MineProblem -> FullState -> Int -> Either ActionException (Maybe WorkerState)
doClone prob fullState workerIndex = case selectWorker fullState workerIndex of
  Nothing -> Right Nothing 
  Just oneWorkerState -> case HM.lookup (oneWorkerState ^. wwPosition) (oneWorkerState ^. boosters) of
    Just Mysterious -> 
      Right $ Just $ WorkerState
          { _wPosition = oneWorkerState ^. wwPosition
          , _wOrientation = oneWorkerState ^. wwOrientation
          , _wManipulators = startingManip
          , _wActiveFastWheels = 0
          , _wActiveDrill = 0
          }
    _ -> Left InvalidCloneLocation
  
addWorker :: FullState -> WorkerState -> FullState
addWorker state0 worker =
  state0 & fWorkers %~ HM.insert index worker
  where
    index = length (state0 ^. fWorkers)

stepAllWorkers :: 
  MineProblem -> 
  FullState -> 
  HashMap Int [Action] -> 
  Either ActionException (FullState, HashMap Int Action, HashMap Int [Action]) 
stepAllWorkers prob state0 actions = fmap (\(state1, newWorkers, performed, actions) -> 
  (tickTimeAll $ addWorkers newWorkers state1, performed, actions)) result
  where
    result :: Either ActionException (FullState, [WorkerState], HashMap Int Action, HashMap Int [Action])
    result = foldl' (\accum (workerIndex, actions) -> case accum of 
      Left error -> Left error
      Right (state0, newWorkers, perfed, remainingActions) -> case actions of
        all@(DoClone : rest) -> case doClone prob state0 workerIndex of
          Left error -> Left error
          Right (Just newWorker) -> Right (state0 & fCollectedBoosters %~ HM.adjust pred Clone, newWorker : newWorkers, HM.insert workerIndex DoClone perfed, HM.insert workerIndex rest remainingActions)
          Right Nothing -> Right (state0, newWorkers, perfed, HM.insert workerIndex all remainingActions)
        all@(action : rest) -> case stepSingleWorker prob state0 workerIndex action of
          Left error -> Left error
          Right Nothing -> Right (state0, newWorkers, perfed, HM.insert workerIndex all remainingActions)
          Right (Just state1) -> Right (state1, newWorkers, HM.insert workerIndex action perfed, HM.insert workerIndex rest remainingActions)
        [] -> Right (state0, newWorkers, perfed, remainingActions))
      (Right (state0, [], mempty, HM.empty))
      (sort $ HM.toList actions)
    addWorkers newWorkers state =
      foldl' addWorker state newWorkers 

stepAllWorkers' ::
  MineProblem ->
  FullState ->
  HashMap Int Action ->
  Either ActionException FullState
stepAllWorkers' prob state0 actions = fmap tickTimeAll result
  where
    result :: Either ActionException FullState
    result = foldl' (\accum (workerIndex, action) -> case accum of
      Left error -> Left error
      Right state0 -> case action of
        DoClone -> case doClone prob state0 workerIndex of
          Left error -> Left error
          Right (Just newWorker) -> Right $ state0
            & fCollectedBoosters %~ HM.adjust pred Clone
            & fWorkers %~ HM.insert index newWorker
            where
              index = length (state0 ^. fWorkers)
          Right Nothing -> Right state0
        action -> case stepSingleWorker prob state0 workerIndex action of
          Left error -> Left error
          Right Nothing -> Right state0
          Right (Just state1) -> Right state1)
      (Right state0)
      (sort $ HM.toList actions)

actionsAreMissing :: FullState -> HM.HashMap Int [Action] -> Bool
actionsAreMissing state actions =
  foldl' checkNonemptyActions False (HM.keys $ state ^. fWorkers)
  where
    checkNonemptyActions :: Bool -> Int -> Bool
    checkNonemptyActions prev i = case HM.lookup i actions of
      Nothing -> True  
      Just [] -> True
      Just _ -> prev
  
stepUntilFinished :: MineProblem -> FullState -> HM.HashMap Int [Action] -> Either ActionException FullState
stepUntilFinished prob state0 action0 = if actionsAreMissing state0 action0 then Right state0 else do
    (state1, _, action1) <- stepAllWorkers prob state0 action0 
    stepUntilFinished prob state1 action1

stepUntilFinished' :: MineProblem -> FullState -> HM.HashMap Int (Seq Action) -> HM.HashMap Int [Action] ->
  Either ActionException (FullState, HM.HashMap Int (Seq Action), HM.HashMap Int [Action])
stepUntilFinished' prob state0 performed action0 = if actionsAreMissing state0 action0 then return (state0, performed, action0) else do
    (state1, perfed, action1) <- stepAllWorkers prob state0 action0
    stepUntilFinished' prob state1 (HM.unionWith (<>) performed (HM.map Seq.singleton perfed)) action1
