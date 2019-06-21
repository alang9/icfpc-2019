module ICFP2019.Main.Greedy where

import qualified Data.Attoparsec.ByteString as AP
import qualified Data.ByteString.Char8 as C8
import qualified Data.HashSet as HS
import System.Environment

import ICFP2019.State
import ICFP2019.Action
import ICFP2019.AStar

main :: IO ()
main = do
  [desc] <- getArgs
  descBs <- C8.readFile desc
  let Right state0 = AP.parseOnly initialParser descBs
  go state0
  putStrLn ""
  where
    go st = case HS.toList (missingTiles st) of
      [] -> return ()
      (pt:_) -> case aStar st pt of
        ([], _) -> error "bad greedy"
        (acts, st') -> do
          putStr $ concat $ serialize <$> acts
          go st'
--   go state0 undefin
