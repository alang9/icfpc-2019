module ICFP2019.Main.Dfs where

import qualified Data.Attoparsec.ByteString as AP
import qualified Data.ByteString.Char8 as C8
import qualified Data.Sequence as Seq
import System.Environment

import ICFP2019.State
import ICFP2019.Action
import ICFP2019.DFS

main :: IO ()
main = do
  [desc] <- getArgs
  descBs <- C8.readFile desc
  let Right state0 = AP.parseOnly initialParser descBs
  go state0
  where
    go st
      | allWrapped st = return ()
      | otherwise =
          case boundedDfs (fromIntegral . remainingTiles) 5 st of
            (x Seq.:<| _, _) -> do
              putStrLn $ serialize x
              go (either (error "impossible") id $ step st x)
--   go state0 undefin

