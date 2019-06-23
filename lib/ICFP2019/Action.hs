{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module ICFP2019.Action where

import qualified Data.Attoparsec.ByteString.Char8 as AP
import Data.Hashable
import GHC.Generics (Generic)
import Linear
import qualified Data.HashMap.Lazy as HM
import Data.List (foldl', sort, intercalate)
import Data.HashMap.Lazy (HashMap)
import Data.Vector.Unboxed.Deriving (derivingUnbox)

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

derivingUnbox "Action"
    [t| Action -> (Int, V2 Int) |]
    [| \a -> case a of
              MoveUp -> (0, V2 0 0 )
              MoveDown -> (1,V2 0 0)
              MoveLeft -> (2,V2 0 0)
              MoveRight -> (3,V2 0 0)
              DoNothing -> (4,V2 0 0) 
              TurnCW -> (5,V2 0 0) 
              TurnCCW -> (6,V2 0 0) 
              AttachManipulator v -> (7, v)
              AttachFastWheels -> (8,V2 0 0)
              AttachDrill -> (9,V2 0 0) 
              Reset -> (10, V2 0 0) 
              Shift v -> (11, v)
              DoClone -> (12, V2 0 0) |]
    [| \(i,v) -> case i of
              0  ->  MoveUp
              1  ->  MoveDown
              2  ->  MoveLeft
              3  ->  MoveRight
              4  ->  DoNothing
              5  ->  TurnCW
              6  ->  TurnCCW
              7  ->  AttachManipulator v 
              8  ->  AttachFastWheels
              9  ->  AttachDrill
              10 ->  Reset
              11 ->  Shift v 
              other  ->  DoClone |]

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
  , AP.char 'Z' *> pure DoNothing
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

