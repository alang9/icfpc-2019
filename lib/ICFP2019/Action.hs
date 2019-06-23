{-# LANGUAGE DeriveGeneric #-}

module ICFP2019.Action where

import qualified Data.Attoparsec.ByteString.Char8 as AP
import Data.Hashable
import GHC.Generics (Generic)
import Linear
import qualified Data.HashMap.Lazy as HM
import Data.List (foldl', sort, intercalate)
import Data.HashMap.Lazy (HashMap)

data Action
  = MoveUp
  | MoveDown
  | MoveLeft
  | MoveRight
  | DoNothing
  | TurnCW
  | TurnCCW
  | AttachManipulator (V2 Int)
  | AttachFastWheels
  | AttachDrill
  | Reset
  | Shift (V2 Int)
  | DoClone
  deriving (Show, Generic, Ord, Eq)

instance Hashable Action

parseAction :: AP.Parser Action
parseAction = AP.choice
  [ AP.char 'W' *> pure MoveUp
  , AP.char 'A' *> pure MoveLeft
  , AP.char 'S' *> pure MoveDown
  , AP.char 'D' *> pure MoveRight
  , AP.char 'Q' *> pure TurnCCW
  , AP.char 'E' *> pure TurnCW
  , AP.char 'C' *> pure DoClone
  , do
      _ <- AP.char 'B'
      _ <- AP.char '('
      dx <- AP.signed AP.decimal
      _ <- AP.char ','
      dy <- AP.signed AP.decimal
      _ <- AP.char ')'
      return $ AttachManipulator (V2 dx dy)
  , AP.char 'F' *> pure AttachFastWheels
  , AP.char 'L' *> pure AttachDrill
  , AP.char 'R' *> pure Reset
  , do
      _ <- AP.char 'T'
      _ <- AP.char '('
      x <- AP.signed AP.decimal
      _ <- AP.char ','
      y <- AP.signed AP.decimal
      _ <- AP.char ')'
      return $ Shift (V2 x y)
  ]

parseAllActions :: AP.Parser (HashMap Int [Action])
parseAllActions = 
  do 
    actions <- AP.sepBy (AP.many' parseAction) (AP.char '#')
    return $ foldl' (\accum (k,v) -> HM.insert k v accum) HM.empty (zip [0..] actions)

serialize :: Action -> String
serialize MoveUp = "W"
serialize MoveDown = "S"
serialize MoveLeft = "A"
serialize MoveRight = "D"
serialize DoNothing = "Z"
serialize TurnCW = "E"
serialize TurnCCW = "Q"
serialize (AttachManipulator (V2 dx dy)) = "B(" ++ show dx ++ "," ++ show dy ++ ")"
serialize AttachFastWheels = "F"
serialize AttachDrill = "L"
serialize Reset = "R"
serialize (Shift (V2 x y)) = "T(" ++ show x ++ "," ++ show y ++ ")"
serialize DoClone = "C"

serializeActions :: HM.HashMap Int [Action] -> String
serializeActions actions =
  intercalate "#" $ fmap snd $ sort $ HM.toList $ fmap (concatMap serialize) actions

