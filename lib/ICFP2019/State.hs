{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module ICFP2019.State where

import Control.Applicative
import Control.Lens
import Control.Monad
import qualified Data.Attoparsec.ByteString.Char8 as AP
import Data.Hashable
import qualified Data.HashMap.Lazy as HM
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashSet as HS
import Data.HashSet (HashSet)
import GHC.Generics (Generic)
import Linear

import ICFP2019.Action

type Point = V2 Int

type RelPos = V2 Int

type TimeRemaining = Int

data Booster
  = Extension
  | FastWheels
  | Drill
  | Mysterious
  deriving (Show, Eq, Generic)

instance Hashable Booster

data MineState = MineState
  { _wwPosition :: !Point
  , _wwManipulators :: !(HashSet RelPos)
  , _wrapped :: !(HashSet Point)
  , _blocked :: !(HashSet Point)
  , _boosters :: !(HashMap Point Booster)
  , _collectedBoosters :: !(HashMap Booster Int)
  , _activeFastWheels :: !TimeRemaining
  , _activeDrill :: !TimeRemaining
  , _timeSpent :: !Int
  , _corner0 :: !Point
  , _corner1 :: !Point
  } deriving (Show)

makeLenses ''MineState

data ActionException
  = NoBooster
  | CantAttachManipulator

initialParser :: AP.Parser MineState
initialParser = do
    (c0, c1) <- quadParser
    _ <- AP.char '#'
    wwPos <- pairParser
    _ <- AP.char '#'
    walls <- AP.sepBy quadParser (AP.char ';')
    _ <- AP.char '#'
    boos <- AP.sepBy boosterParser (AP.char ';')
    return $ MineState
      { _wwPosition = wwPos
      , _wwManipulators = HS.fromList [V2 0 0, V2 1 0, V2 1 1, V2 1 (-1)]
      , _wrapped = mempty
      , _blocked = HS.fromList [ V2 x y | (V2 mX0 mY0, V2 mX1 mY1) <- walls, x <- [mX0..mX1-1], y <- [mY0..mY1-1] ]
      , _corner0 = c0
      , _corner1 = c1
      , _boosters = HM.fromList boos
      , _collectedBoosters = mempty
      , _activeFastWheels = 0
      , _activeDrill = 0
      , _timeSpent = 0
      }
  where
    boosterParser = do
      whichBooster <- AP.char 'B' *> pure Extension
        <|> AP.char 'F' *> pure FastWheels
        <|> AP.char 'L' *> pure Drill
        <|> AP.char 'X' *> pure Mysterious
      pt <- pairParser
      return (pt, whichBooster)
    quadParser = do
      V2 mx my <- pairParser
      _ <- AP.char ','
      V2 dX my' <- pairParser
      _ <- AP.char ','
      V2 dX' dY' <- pairParser
      _ <- AP.char ','
      V2 mx' dY <- pairParser
      guard $ my == my'
      guard $ mx == mx'
      guard $ dX == dX'
      guard $ dY == dY'
      return (V2 mx my, V2 dX dY)
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
      & activeFastWheels %~ max 30
      & collectedBoosters %~ HM.adjust pred Drill
    else Left NoBooster
  AttachManipulator rp -> if HM.lookupDefault 0 Extension (state0 ^. collectedBoosters) > 0
    then
      if HS.member rp (validNewManipulatorPositions state0)
        then
          Right $ tickTime $ state0
            & wwManipulators %~ HS.insert rp
            & collectedBoosters %~ HM.adjust pred Extension
        else Left CantAttachManipulator
    else Left NoBooster
  where
    doMove rp =
      if state0 ^. activeFastWheels > 0
        then tickTime $ movePosition rp $ movePosition rp state0
        else tickTime $ movePosition rp state0

bound :: MineState -> Point -> Point
bound state0 (V2 x y) = V2 (max x0 $ min x1 x) (max y0 $ min y1 y)
  where
    V2 x0 y0 = state0 ^. corner0
    V2 x1 y1 = state0 ^. corner1

passable :: Point -> MineState -> Bool
passable pt state0 = inMine && (notWall || hasDrill)
  where
    hasDrill = state0 ^. activeDrill > 0
    inMine = x >= x0 && x < x1 && y >= y0 && y < y1
    notWall = not $ HS.member pt (view blocked state0)
    V2 x y = state0 ^. wwPosition
    V2 x0 y0 = state0 ^. corner0
    V2 x1 y1 = state0 ^. corner1

movePosition :: RelPos -> MineState -> MineState
movePosition rp state0 =
  if passable pt state0
    then getBooster pt $
      state0
        & wwPosition .~ pt
        & wrapped %~ (`HS.union` (HS.map (+ pt) $ state0 ^. wwManipulators)) -- TODO: We need to check line of sight here
        & blocked %~ HS.delete pt
    else state0
  where
    pt = rp + state0 ^. wwPosition

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
