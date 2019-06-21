module ICFP2019.Main.Sim
    ( main
    ) where

import qualified Data.Attoparsec.ByteString as AP
import qualified Data.ByteString.Char8 as C8
import System.Environment

import ICFP2019.State
import ICFP2019.Action

main :: IO ()
main = do
  [desc,sols] <- getArgs
  descBs <- C8.readFile desc
  let Right (prob, state0) = AP.parseOnly initialParser descBs
  actionBs <- C8.readFile sols
  go prob 0 state0 actionBs
  where
    go :: MineProblem -> Int -> MineState -> C8.ByteString -> IO ()
    go prob n state0 actionBs = do
      if allWrapped state0
        then do
          putStrLn $ "Completed after " ++ show n ++ " steps"
          putStrLn $ "Remaining actions: " ++ show actionBs
          putStrLn $ "Misssing tiles: " ++ show (missingTiles state0)
        else
          case AP.parse parseAction actionBs of
            f@(AP.Fail _ _ _) -> error $ show (f, n, state0, actionBs, missingTiles state0)
            AP.Partial _ -> error "unexpected partial"
            AP.Done remain act -> case step prob state0 act of
              Left exc -> error $ "sim exception: " ++ show exc
              Right state1 -> go prob (n + 1) state1 remain
