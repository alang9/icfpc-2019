module ICFP2019.Main.Greedy where

import qualified Data.Attoparsec.ByteString as AP
import qualified Data.ByteString.Char8 as C8
import qualified Data.HashSet as HS
import System.Environment
import System.IO

import ICFP2019.State
import ICFP2019.Action
import ICFP2019.AStar

main :: IO ()
main = do
  [desc] <- getArgs
  descBs <- C8.readFile desc
  let Right (prob, state0) = AP.parseOnly initialParser descBs
  hSetBuffering stdout NoBuffering
  go prob state0
  putStrLn ""
  where
    go prob st = case HS.toList (missingTiles st) of
      [] -> return ()
      (pt:_) -> case aStar prob st pt of
        ([], _) -> error "bad greedy"
        (acts, st') -> do
          putStr $ concat $ serialize <$> acts
          go prob st'
--   go state0 undefin
