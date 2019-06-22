{-# LANGUAGE BangPatterns #-}
module ICFP2019.Main.Greedy where

import qualified Data.Attoparsec.ByteString as AP
import qualified Data.ByteString.Char8 as C8
import Data.List
import System.Environment
import System.IO

import ICFP2019.State
import ICFP2019.Action
import ICFP2019.AStar

import Debug.Trace

main :: IO ()
main = do
  [desc] <- getArgs
  descBs <- C8.readFile desc
  let Right (prob, state0) = AP.parseOnly initialParser descBs
  hSetBuffering stdout NoBuffering
  go 0 prob state0
  putStrLn ""
  where
    go turns prob !st
      | allWrapped st = traceShowM ("turns", turns)
      | otherwise = case bfs True prob st of
          [] -> error "bad greedy"
          acts -> do
            putStr $ concat $ serialize <$> acts
            let st' = foldl' (fmap (either (error "oops") id) . step prob) st acts
            go (turns + length acts) prob st'
--   go state0 undefin
