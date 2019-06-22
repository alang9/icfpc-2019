module ICFP2019.Main.Sim
    ( main
    ) where

import Control.Lens
import qualified Data.Attoparsec.ByteString as AP
import qualified Data.ByteString.Char8 as C8
import System.Environment

import ICFP2019.State
import ICFP2019.Action

import Debug.Trace

main :: IO ()
main = do
  [desc,sols] <- getArgs
  descBs <- C8.readFile desc
  let Right (prob, state0) = AP.parseOnly initialParser descBs
  actionBs <- C8.readFile sols
  parsedActions <- case AP.parse parseAllActions actionBs of
    f@(AP.Fail _ _ _) -> error "parsing failed" 
    AP.Partial _ -> error "unexpected partial"
    AP.Done remain actions -> actions 
  go prob 0 state0 parsedActions
  where
    go :: MineProblem -> Int -> FullState -> C8.ByteString -> IO ()
    go prob n state0 parsedActions = do
      if allWrapped state0
        then do
          putStrLn $ "Completed after " ++ show n ++ " steps"
          putStrLn $ "Remaining actions: " ++ show parsedActions
          putStrLn $ "Misssing tiles: " ++ show (missingTiles state0)
        else
          case stepAllWorkers prob state0 act of
              Left exc -> error $ "sim exception: " ++ show exc
              Right (state1, act1) -> do
                go prob (n + 1) state1 act1
