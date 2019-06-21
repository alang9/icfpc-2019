module ICFP2019.Main.V0
    ( main
    ) where

import qualified Data.Attoparsec.ByteString as AP
import qualified Data.ByteString.Char8 as C8
import System.Environment

import ICFP2019.State

main :: IO ()
main = do
  [x] <- getArgs
  bs <- C8.readFile x
  let state = AP.parse initialParser bs
  print state
