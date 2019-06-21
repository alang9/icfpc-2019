module ICFP2019.Main.Dfs where

import qualified Data.Attoparsec.ByteString as AP
import qualified Data.ByteString.Char8 as C8
import qualified Data.Sequence as Seq
import System.Environment
import System.IO

import ICFP2019.State
import ICFP2019.Action
import ICFP2019.DFS

main :: IO ()
main = do
  [desc] <- getArgs
  descBs <- C8.readFile desc
  let Right state0 = AP.parseOnly initialParser descBs
  go state0
  putStrLn ""
  where
    go st
      | allWrapped st = putStrLn ""
      | otherwise = do
          foo <- boundedDfs (\st -> pure (remainingTiles st, costHeuristic st)) 5 st
          case foo of
            (x Seq.:<| _, _) -> do
              putStr $ serialize x
              hFlush stdout
              go (either (error "impossible") id $ step st x)
--   go state0 undefin

