{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module ICFP2019.State where

import Control.Applicative
import Control.Lens
import qualified Data.Attoparsec.ByteString.Char8 as AP
import Data.Hashable
import qualified Data.HashMap.Lazy as HM
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
  | Mysterious
  deriving (Show, Eq, Ord, Generic)

instance Hashable Booster

-- INVARIANT: wrapped and blocked and unwrapped should be mutually exclusive, and their union should be `Shape.toHashSet boundary`
data MineState = MineState
  { _wwPosition :: !Point
  , _wwManipulators :: !(HashSet RelPos)
  , _wrapped :: !(HashSet Point)
  , _unwrapped :: !(HashSet Point)
  , _blocked :: !(HashSet Point)
  , _boosters :: !(HashMap Point Booster)
  , _collectedBoosters :: !(HashMap Booster Int)
  , _activeFastWheels :: !TimeRemaining
  , _activeDrill :: !TimeRemaining
  , _timeSpent :: !Int
  , _boundary :: !Shape
  } deriving (Show, Eq, Ord, Generic)

instance Hashable MineState

makeLenses ''MineState

data ActionException
  = NoBooster
  | CantAttachManipulator
  deriving (Show)

initialParser :: AP.Parser MineState
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
    return $ applyWrapped $ MineState
      { _wwPosition = wwPos
      , _wwManipulators = startingManip
      , _wrapped = wrap
      , _unwrapped = HS.difference (HS.difference (allTiles boundary0) wrap) blok
      , _blocked = blok
      , _boundary = boundary0
      , _boosters = HM.fromList boos
      , _collectedBoosters = mempty
      , _activeFastWheels = 0
      , _activeDrill = 0
      , _timeSpent = 0
      }
  where
    allTiles sha = Shape.toHashSet sha

    startingManip = HS.fromList [V2 0 0, V2 1 0, V2 1 1, V2 1 (-1)]
    boosterParser = do
      whichBooster <- AP.char 'B' *> pure Extension
        <|> AP.char 'F' *> pure FastWheels
        <|> AP.char 'L' *> pure Drill
        <|> AP.char 'X' *> pure Mysterious
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

validNewManipulatorPositions :: MineState -> HashSet Point
validNewManipulatorPositions state0 = HS.unions [HS.map ((+) dir) $ state0 ^. wwManipulators | dir <- dirs] `HS.difference` (state0 ^. wwManipulators)
  where
    dirs = [V2 0 1, V2 0 (-1), V2 1 0, V2 (-1) 0]

step :: MineState -> Action -> Either ActionException MineState
step state0 act = case act of
  DoNothing -> pure $ tickTime state0
  MoveUp -> pure $ doMove (V2 0 1)
  MoveDown -> pure $ doMove (V2 0 (-1))
  MoveLeft -> pure $ doMove (V2 (-1) 0)
  MoveRight -> pure $ doMove (V2 1 0)
  TurnCW -> pure $ tickTime $ movePosition (V2 0 0) $ state0 & wwManipulators %~ HS.map (\(V2 x y) -> V2 y (-x))
  TurnCCW -> pure $ tickTime $ movePosition (V2 0 0) $ state0 & wwManipulators %~ HS.map (\(V2 x y) -> V2 (-y) x)
  AttachFastWheels -> if HM.lookupDefault 0 FastWheels (state0 ^. collectedBoosters) > 0
    then Right $ tickTime $ state0
      & activeFastWheels %~ max 50
      & collectedBoosters %~ HM.adjust pred FastWheels
    else Left NoBooster
  AttachDrill -> if HM.lookupDefault 0 Drill (state0 ^. collectedBoosters) > 0
    then Right $ tickTime $ state0
      & activeDrill %~ max 30
      & collectedBoosters %~ HM.adjust pred Drill
    else Left NoBooster
  AttachManipulator rp -> if HM.lookupDefault 0 Extension (state0 ^. collectedBoosters) > 0
    then
      if HS.member rp (validNewManipulatorPositions state0)
        then
          Right $ tickTime $ applyWrapped $ state0
            & wwManipulators %~ HS.insert rp
            & collectedBoosters %~ HM.adjust pred Extension
        else Left CantAttachManipulator
    else Left NoBooster
  where
    doMove rp =
      if state0 ^. activeFastWheels > 0
        then tickTime $ movePosition rp $ movePosition rp state0
        else tickTime $ movePosition rp state0

passable :: Point -> MineState -> Bool
passable pt state0 = inMine && (notWall || hasDrill)
  where
    hasDrill = state0 ^. activeDrill > 0
    inMine = pt `Shape.member` (state0 ^. boundary)
    notWall = not $ HS.member pt (view blocked state0)

open :: MineState -> Point -> Bool
open state0 pt = inMine && notWall
  where
    inMine = pt `Shape.member` (state0 ^. boundary)
    notWall = not $ HS.member pt (view blocked state0)

notWrapped :: MineState -> Point -> Bool
notWrapped state0 pt = open state0 pt && not (HS.member pt (state0 ^. wrapped))

movePosition :: RelPos -> MineState -> MineState
movePosition rp state0 =
  if passable pt state0
    then getBooster pt $
      applyWrapped $ state0
            & wwPosition .~ pt
            & blocked %~ HS.delete pt
    else state0
  where
    pt = rp + state0 ^. wwPosition

applyWrapped :: MineState -> MineState
applyWrapped state0 = state0
  & wrapped %~ HS.union diff
  & unwrapped %~ (\uw -> HS.difference uw diff)
  where
    diff = HS.filter (open state0) $ HS.map (+ state0 ^. wwPosition) $ state0 ^. wwManipulators -- TODO: We need to check line of sight here

allWrapped :: MineState -> Bool
allWrapped state0 = if HS.size (HS.intersection (state0 ^. blocked) (state0 ^. wrapped)) > 0
  then error "allWrapped: invariant broken"
  else case compare (HS.size (state0 ^. blocked) + HS.size (state0 ^. wrapped)) (numTiles state0) of
    LT -> False
    EQ -> True
    GT -> error $ "allWrapped: Should be impossible: " ++ show state0

remainingTiles :: MineState -> Int
remainingTiles state0 = numTiles state0 - HS.size (state0 ^. blocked) - HS.size (state0 ^. wrapped)

numTiles :: MineState -> Int
numTiles = VU.length . VU.filter id . view (boundary . Shape.points)

missingTiles :: MineState -> HashSet Point
missingTiles = view unwrapped

getBooster :: Point -> MineState -> MineState
getBooster pt state0 = case HM.lookup pt $ state0 ^. boosters of
  Nothing -> state0
  Just b -> state0
    & boosters %~ HM.delete pt
    & collectedBoosters %~ HM.insertWith (+) b 1

tickTime :: MineState -> MineState
tickTime state0 = state0
  & activeFastWheels %~ max 0 . subtract 1
  & activeDrill %~ max 0 . subtract 1
  & timeSpent %~ (+) 1

interestingActions :: MineState -> [Action]
interestingActions state0 = concat
  [ if passable (state0 ^. wwPosition + V2 (-1) 0) state0 then [MoveLeft] else []
  , if passable (state0 ^. wwPosition + V2 1 0) state0 then [MoveRight] else []
  , if passable (state0 ^. wwPosition + V2 0 (-1)) state0 then [MoveDown] else []
  , if passable (state0 ^. wwPosition + V2 0 1) state0 then [MoveUp] else []
  , [TurnCW]
  , [TurnCCW]
  , if maybe False (> 0) $ HM.lookup FastWheels (state0 ^. collectedBoosters) then [AttachFastWheels] else []
  , if maybe False (> 0) $ HM.lookup Drill (state0 ^. collectedBoosters) then [AttachDrill] else []
  , if maybe False (> 0) $ HM.lookup Extension (state0 ^. collectedBoosters) then [AttachManipulator pt | pt <- HS.toList (validNewManipulatorPositions state0), notWrapped state0 pt ] else []
  ]
