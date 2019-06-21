module ICFP2019.Action where

data Action
  = MoveUp
  | MoveDown
  | MoveLeft
  | MoveRight
  | DoNothing
  | TurnCW
  | TurnCCW
  | Attach (V2 Int)
  | FastWheels
  | Drill

