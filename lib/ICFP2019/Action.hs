{-# LANGUAGE DeriveGeneric #-}

module ICFP2019.Action where

import qualified Data.Attoparsec.ByteString.Char8 as AP
import Data.Hashable
import GHC.Generics (Generic)
import Linear

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
  ]

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
