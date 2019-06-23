{-# LANGUAGE BangPatterns #-}
module ICFP2019.Main.Greedy where

import qualified Data.Attoparsec.ByteString as AP
import qualified Data.ByteString.Char8 as C8
import Data.List
import System.Environment
import System.IO
import Control.Lens
import qualified Data.HashMap.Lazy as HM

import ICFP2019.State
import ICFP2019.Action
import ICFP2019.AStar

import Debug.Trace

combineActions :: HM.HashMap Int [Action] -> HM.HashMap Int [Action] -> HM.HashMap Int [Action]
combineActions = HM.unionWith (++) 

runSteps :: MineProblem -> FullState -> HM.HashMap Int [Action] -> IO (HM.HashMap Int [Action], Int)
runSteps prob !st actionsDone
  | allWrapped st = pure (actionsDone, st ^. fTimeSpent)
  | otherwise = 
      let actions = truncateActions $ bfsMultipleWorkers (bfs True prob mempty) st in 
          do
            let st' = if actionsAreMissing st actions 
                      then error "bad greedy" 
                      else either (error "oops") id $ stepUntilFinished prob st actions 
            runSteps prob st' (combineActions actionsDone actions)

main :: IO ()
main = do
  [desc] <- getArgs
  descBs <- C8.readFile desc
  let Right (prob, state0) = AP.parseOnly initialParser descBs
  hSetBuffering stdout NoBuffering
  (actionsDone, timeSpent) <- runSteps prob state0 HM.empty
  putStrLn $ serializeActions actionsDone
  traceShow timeSpent $ putStrLn ""
