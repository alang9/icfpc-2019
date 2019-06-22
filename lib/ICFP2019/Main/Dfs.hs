module ICFP2019.Main.Dfs where

import qualified Data.Attoparsec.ByteString as AP
import qualified Data.ByteString.Char8 as C8
import qualified Data.Sequence as Seq
import System.Environment
import System.IO

import ICFP2019.State
import ICFP2019.Action
import ICFP2019.DFS

-- main :: IO ()
-- main = do
--   [desc] <- getArgs
--   descBs <- C8.readFile desc
--   let Right (prob, state0) = AP.parseOnly initialParser descBs
--   hSetBuffering stdout NoBuffering
--   go prob state0
--   putStrLn ""
--   where
--     go prob st
--       | allWrapped st = putStrLn ""
--       | otherwise = do
--           foo <- boundedDfs prob (\st -> pure (remainingTiles st, costHeuristic st)) 7 st
--           case foo of
--             (x Seq.:<| _, _, _) -> do
--               putStr $ serialize x
--               go prob (either (error "impossible") id $ step prob st x)
-- --   go state0 undefin
-- 
