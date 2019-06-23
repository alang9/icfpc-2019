module ICFP2019.Main.Buyer where

import           Control.Monad       (forM_)
import qualified Data.HashMap.Strict as HMS
import           System.Environment  (getArgs)
import           System.FilePath     ((</>))
import qualified System.IO           as IO
import           Text.Printf         (printf)
import           Text.Read           (readMaybe)

import           ICFP2019.Booster

main :: IO ()
main = do
    args <- getArgs
    case args of
        [balance, path] | Just bal <- readMaybe balance ->
            writeMassBuy path $ buyStrategy bal 300
        _ ->
            IO.hPutStrLn IO.stderr $ "Usage: buyer <balance> <out dir>"

-- Booster bag per problem.
type MassBuy = HMS.HashMap Int BoosterBag

initialMassBuy :: Int -> MassBuy
initialMassBuy numProblems =
    HMS.fromList [(n, mempty) | n <- [1 .. numProblems]]

buyStrategy :: Int -> Int -> MassBuy
buyStrategy balance numProblems =
    go (initialMassBuy numProblems) balance (cycle [1 .. numProblems])
  where
    go acc _ [] = acc
    go acc bal (n : ns)
        | bal >= boosterCost Clone =
            let acc' = HMS.insertWith (<>) n (singletonBoosterBag Clone) acc in
            go acc' (bal - boosterCost Clone) ns
        | otherwise = acc

writeMassBuy :: FilePath -> MassBuy -> IO ()
writeMassBuy dir massBuy = do
    forM_ (HMS.toList massBuy) $ \(prob, bag) ->
        writeBoosterBag (dir </> printf "prob-%03d.buy" prob) bag
