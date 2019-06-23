module Main where

import qualified ICFP2019.LineOfSight.Tests
import qualified Test.Tasty                 as Tasty

main :: IO ()
main = Tasty.defaultMain $ Tasty.testGroup "icfp2019"
    [ ICFP2019.LineOfSight.Tests.tests
    ]
