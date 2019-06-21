module ICFP2019.State where

import Control.Applicative
import Control.Monad
import qualified Data.Attoparsec.ByteString.Char8 as AP
import qualified Data.HashMap.Lazy as HM
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashSet as HS
import Data.HashSet (HashSet)
import Linear

type Point = V2 Int

data Dir = Up | Down | Left | Right

data MineState = MineState
  { wwPosition :: !Point
  , wwManipulators :: !(HashSet Point)
  , wrapped :: !(HashSet Point)
  , blocked :: !(HashSet Point)
  , boosters :: !(HashMap Point Booster)
  , corner0 :: !Point
  , corder1 :: !Point
  } deriving (Show)

data Booster
  = Extension
  | FastWheels
  | Drill
  | Mysterious
  deriving (Show)

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
      { wwPosition = wwPos
      , wwManipulators = HS.fromList [V2 1 0, V2 1 1, V2 1 (-1)]
      , wrapped = mempty
      , blocked = HS.fromList [ V2 x y | (V2 mX0 mY0, V2 mX1 mY1) <- walls, x <- [mX0..mX1-1], y <- [mY0..mY1-1] ]
      , corner0 = c0
      , corder1 = c1
      , boosters = HM.fromList boos
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
