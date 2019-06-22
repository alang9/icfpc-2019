module ICFP2019.Main.GreedyDfs where

import Control.Lens
import Control.Monad
import qualified Data.Attoparsec.ByteString as AP
import qualified Data.ByteString.Char8 as C8
import qualified Data.HashSet as HS
import Data.List
import qualified Data.Sequence as Seq
import System.Environment
import System.IO
import qualified System.Random.MWC as MWC

import ICFP2019.State
import ICFP2019.Action
import ICFP2019.DFS
import ICFP2019.AStar

import Debug.Trace

main :: IO ()
main = do
  [desc] <- getArgs
  descBs <- C8.readFile desc
  let Right (prob, state0) = AP.parseOnly initialParser descBs
  hSetBuffering stdout NoBuffering
  gen <- MWC.create
  go gen prob state0
  putStrLn ""
  where
    go gen prob st
      | allWrapped st = putStrLn ""
      | otherwise = do
          foo <- randomBoundedDfs gen prob (\st' -> do (acts, st'') <- boundedBfs gen 50 prob st'; return (remainingTiles st'', length acts, negate (HS.size (st' ^. beaconLocations)), negate $ sum (st' ^. collectedBoosters), remainingTiles st')) 1 st
          case foo of
            (xs@(x Seq.:<| _), st', finSco)
              | x /= DoNothing && view _2 finSco < remainingTiles st -> do
                  traceShowM $ (concat $ serialize <$> xs, remainingTiles st', finSco)
                  putStr $ serialize x
                  go gen prob (either (error "impossible") id $ step prob st x)
              | otherwise -> case bfs prob st of -- If doing greedy dfs doesn't give anything good, just do regular bfs
                  [] -> error "bad greedy"
                  acts -> do
                    let st' = foldl' (fmap (either (error "oops9") id) . step prob) st acts
                    forM_ acts $ \act -> putStr $ serialize act
                    traceShowM ("greedy", length acts)
                    go gen prob st'
--   go state0 undefin
