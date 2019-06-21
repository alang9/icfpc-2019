module ICFP2019.Action where

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

