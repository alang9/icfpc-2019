{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module ICFP2019.State where

import Control.Applicative
import Control.Lens
import qualified Data.Attoparsec.ByteString.Char8 as AP
import Data.Hashable
import qualified Data.HashMap.Lazy as HM
import Data.List (foldl')
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashSet as HS
import Data.HashSet (HashSet)
import qualified Data.Vector.Unboxed as VU
import GHC.Generics (Generic)
import Linear

import qualified ICFP2019.Shape as Shape
import ICFP2019.Shape (Shape)
import ICFP2019.Action

type Point = V2 Int

type RelPos = V2 Int

type TimeRemaining = Int

data Booster
  = Extension
  | FastWheels
  | Drill
  | Teleport
  | Mysterious
  | Clone
  deriving (Show, Eq, Ord, Generic)

instance Hashable Booster

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
      Just $ updateFullState workerIndex result fullstate
     
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
          }
    let initialSt = maybe fullSt id (updateFullStateWith applyWrapped fullSt 0) 
    return (mineProb, initialSt)
  where
    allTiles sha = Shape.toHashSet sha
    boosterParser = do
      whichBooster <- AP.char 'B' *> pure Extension
        <|> AP.char 'F' *> pure FastWheels
        <|> AP.char 'L' *> pure Drill
        <|> AP.char 'X' *> pure Mysterious
        <|> AP.char 'R' *> pure Teleport
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

validNewManipulatorPositions :: OneWorkerState -> HashSet Point
validNewManipulatorPositions state0 = HS.unions [HS.map ((+) dir) $ state0 ^. wwManipulators | dir <- dirs] `HS.difference` (state0 ^. wwManipulators)
  where
    dirs = [V2 0 1, V2 0 (-1), V2 1 0, V2 (-1) 0]

step :: MineProblem -> OneWorkerState -> Action -> Either ActionException OneWorkerState
step prob state0 act = case act of
  DoNothing -> pure $ state0
  MoveUp -> doMove (V2 0 1)
  MoveDown -> doMove (V2 0 (-1))
  MoveLeft -> doMove (V2 (-1) 0)
  MoveRight -> doMove (V2 1 0)
  TurnCW -> pure $ movePosition' (V2 0 0) $ state0
    & wwManipulators %~ HS.map (\(V2 x y) -> V2 y (-x))
    & wwOrientation %~ mod 4 . succ
  TurnCCW ->
    pure $ movePosition' (V2 0 0) $ state0
    & wwManipulators %~ HS.map (\(V2 x y) -> V2 (-y) x)
    & wwOrientation %~ mod 4 . pred
  AttachFastWheels -> if HM.lookupDefault 0 FastWheels (state0 ^. collectedBoosters) > 0
    then Right $ (state0
      & collectedBoosters %~ HM.adjust pred FastWheels)
      & activeFastWheels %~ (+) 50
    else Left NoBooster
  AttachDrill -> if HM.lookupDefault 0 Drill (state0 ^. collectedBoosters) > 0
    then Right $ (state0
      & collectedBoosters %~ HM.adjust pred Drill)
      & activeDrill %~ (+) 30
    else Left NoBooster
  AttachManipulator rp -> if HM.lookupDefault 0 Extension (state0 ^. collectedBoosters) > 0
    then
      if HS.member rp (validNewManipulatorPositions state0)
        then
          Right $ applyWrapped $ state0
            & wwManipulators %~ HS.insert rp
            & collectedBoosters %~ HM.adjust pred Extension
        else Left CantAttachManipulator
    else Left NoBooster
  Reset -> if HM.lookupDefault 0 Teleport (state0 ^. collectedBoosters) > 0
    then
      Right $ applyWrapped $ state0
            & beaconLocations %~ HS.insert (state0 ^. wwPosition)
            & collectedBoosters %~ HM.adjust pred Teleport
    else Left NoBooster
  Shift pos -> if HS.member pos (state0 ^. beaconLocations)
    then
      Right $ applyWrapped $ state0 & wwPosition .~ pos
    else Left InvalidShift
  where
    doMove rp = maybe (Left NoSpace) Right $ do
      state1 <- movePosition rp prob state0
      if state0 ^. activeFastWheels > 0
        then do
          case movePosition rp prob state1 of
            Nothing -> return $ state1
            Just state2 -> return $ state2
        else return $ state1

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
    then Just $ movePosition' rp state0
    else Nothing
  where
    pt = rp + state0 ^. wwPosition

movePosition' :: RelPos -> OneWorkerState -> OneWorkerState
movePosition' rp state0 =
  getBooster pt $
      applyWrapped $ state0
            & wwPosition .~ pt
            & blocked %~ HS.delete pt
            & unwrapped %~ HS.insert pt
  where
    pt = rp + state0 ^. wwPosition

applyWrapped :: OneWorkerState -> OneWorkerState
applyWrapped state0 = state0
  & unwrapped %~ (kill diff)
  where
    diff = HS.filter (notWrapped state0) $ HS.map (+ state0 ^. wwPosition) $ state0 ^. wwManipulators -- TODO: We need to check line of sight here
    kill victims uw = foldl' (flip HS.delete) uw victims

allWrapped :: FullState -> Bool
allWrapped = HS.null . view fUnwrapped

remainingTiles :: OneWorkerState -> Int
remainingTiles state0 = HS.size (state0 ^. unwrapped)

numTiles :: MineProblem -> Int
numTiles = VU.length . VU.filter id . view (boundary . Shape.points)

missingTiles :: OneWorkerState -> HashSet Point
missingTiles = view unwrapped

getBooster :: Point -> OneWorkerState -> OneWorkerState
getBooster pt state0 = case HM.lookup pt $ state0 ^. boosters of
  Nothing -> state0
  Just b -> state0
    & boosters %~ HM.delete pt
    & collectedBoosters %~ HM.insertWith (+) b 1

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
  , if maybe False (> 0) $ HM.lookup Teleport (state0 ^. collectedBoosters) then [Reset] else []
  , if maybe False (> 0) $ HM.lookup Extension (state0 ^. collectedBoosters) then [AttachManipulator pt | pt <- HS.toList (validNewManipulatorPositions state0), notWrapped state0 pt ] else []
  , [Shift loc | loc <- HS.toList (state0 ^. beaconLocations)]
  , case HM.lookup (state0 ^. wwPosition) (state0 ^. boosters) of
      Just Mysterious -> if maybe False (> 0) $ HM.lookup Clone (state0 ^. collectedBoosters) then [DoClone] else []
      _ -> []
  , [DoNothing]
  ]

interestingActionsAll :: MineProblem -> FullState -> HashMap Int [Action]
interestingActionsAll prob state0 =
  HM.mapWithKey (\k v -> getActions k) (state0 ^. fWorkers)
  where
    getActions :: Int -> [Action]
    getActions i = case selectWorker state0 i of
      Nothing -> []
      Just oneWorkerState -> interestingActions prob oneWorkerState 

stepSingleWorker :: MineProblem -> FullState -> Int -> Action -> Either ActionException FullState
stepSingleWorker prob fullState workerIndex action = case selectWorker fullState workerIndex of
  Nothing -> Right fullState
  Just oneWorkerState -> case step prob oneWorkerState action of
    Left error -> Left error
    Right oneWorkerState1 -> Right (updateFullState workerIndex oneWorkerState1 fullState)

doClone :: MineProblem -> FullState -> Int -> Either ActionException FullState
doClone prob fullState workerIndex = case selectWorker fullState workerIndex of
  Nothing -> Right fullState
  Just oneWorkerState -> case HM.lookup (oneWorkerState ^. wwPosition) (oneWorkerState ^. boosters) of
    Just Mysterious -> 
      Right (fullState & fWorkers %~ HM.insert 0 
        WorkerState
          { _wPosition = oneWorkerState ^. wwPosition
          , _wOrientation = oneWorkerState ^. wwOrientation
          , _wManipulators = startingManip
          , _wActiveFastWheels = 0
          , _wActiveDrill = 0
          })
    _ -> Left InvalidCloneLocation
  
stepAllWorkers :: 
  MineProblem -> 
  FullState -> 
  HashMap Int [Action] -> 
  Either ActionException (FullState, HashMap Int [Action]) 
stepAllWorkers prob state0 actions = fmap (\(state, actions) -> (tickTimeAll state, actions)) result
  where
    result = HM.foldlWithKey' (\accum workerIndex actions -> case accum of 
      Left error -> Left error
      Right (state0, remaining_actions) -> case actions of
        DoClone : _ -> Left InvalidShift
        action : rest -> case stepSingleWorker prob state0 workerIndex action of
          Left error -> Left error
          Right state1 -> Right (state1, HM.insert workerIndex rest remaining_actions))
      (Right (state0, HM.empty))
      actions
