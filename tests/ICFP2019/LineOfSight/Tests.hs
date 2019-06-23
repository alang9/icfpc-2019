module ICFP2019.LineOfSight.Tests where

import           Data.Ratio            (denominator, numerator, (%))
import           ICFP2019.LineOfSight
import           Linear                (V2 (..))
import qualified Test.Tasty            as Tasty
import qualified Test.Tasty.QuickCheck as QC

tests :: Tasty.TestTree
tests = Tasty.testGroup "ICFP2019.LineOfSight.Tests"
    [ QC.testProperty "prop_lineOfSight_01" prop_lineOfSight_01
    ]

newtype Target = Target (V2 Int) deriving (Show)

instance QC.Arbitrary Target where
    arbitrary = fmap Target $
        V2 <$> QC.choose (-100, 100) <*> QC.choose (-100, 100)

newtype Clamp = Clamp Rational deriving (Show)

instance QC.Arbitrary Clamp where
    arbitrary = fmap Clamp $ do
        let denom = fromIntegral (1000 :: Int) :: Integer
        num <- QC.choose (0, denom)
        return (num % denom)

prop_lineOfSight_01 :: Target -> Clamp -> Bool
prop_lineOfSight_01 (Target target@(V2 tx ty)) (Clamp clamp) =
    let tiles = lineOfSight target

        -- Find a point that is on the line.
        testX = (toRational tx * clamp) + (1 % 2)
        testY = (toRational ty * clamp) + (1 % 2)

        -- Find the tile this should belong to.
        tile = V2 (floor testX) (floor testY) in

    tile == V2 0 0 || tile == target || tile `elem` tiles ||
        -- We picked a point on a corner.
        (isIntegral testX && isIntegral testY)

  where
    isIntegral r = numerator r `mod` denominator r == 0
