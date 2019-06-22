module ICFP2019.Main.Sim
    ( main
    ) where

import Control.Lens
import qualified Data.Attoparsec.ByteString as AP
import qualified Data.ByteString.Char8 as C8
import System.Environment
import Data.HashMap.Lazy as HM

import ICFP2019.State
import ICFP2019.Action

import Debug.Trace

printState :: FullState -> HM.HashMap Int [Action] -> IO ()
printState state actions = do
  print nextActions
  print workers
  where
    nextActions = HM.map (\l -> 
      case l of 
        [] -> Nothing
        hd : _ -> Just hd) actions
    workers = state ^. fWorkers

main :: IO ()
main = do
  [desc, sols] <- getArgs
  descBs <- C8.readFile desc
  let Right (prob, state0) = AP.parseOnly initialParser descBs
  actionBs <- C8.readFile sols
  let actions = case AP.parse parseAllActions actionBs of
        AP.Fail _ _ _ -> error "parsing failed" 
        AP.Partial _ -> error "unexpected partial"
        AP.Done remain actions -> actions 
  print $ state0 ^. fBoosters
  go prob 0 state0 actions
  where
    go :: MineProblem -> Int -> FullState -> HashMap Int [Action] -> IO ()
    go prob n state0 actions = do
      if allWrapped state0
        then do
          putStrLn $ "Completed after " ++ show n ++ " steps"
          putStrLn $ "Remaining actions: " ++ show actions
          putStrLn $ "Misssing tiles: " ++ show (missingTiles state0)
        else do
          printState state0 actions 
          case stepAllWorkers prob state0 actions of
              Left exc -> error $ "sim exception: " ++ show exc
              Right (state1, act1) -> do
                go prob (n + 1) state1 act1
