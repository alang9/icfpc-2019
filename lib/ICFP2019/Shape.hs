{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
module ICFP2019.Shape
    ( Shape, width, height, points
    , fromTuples
    , fromPoints

    , pointsInBoundingBox
    , member

    , toHashSet
    , toAscii
    ) where

import           Control.Lens
import           Control.Lens.TH             (makeLenses)
import           Control.Monad               (foldM_, forM_)
import           Control.Monad.ST            (runST)
import qualified Data.HashSet                as HS
import           Data.Maybe                  (fromMaybe)
import           Data.Vector.Instances       ()
import qualified Data.Vector.Unboxed         as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import           GHC.Generics                (Generic)
import           Linear                      (V2 (..), _x, _y)
import           Prelude                     hiding (lines)

data Shape = Shape
    { _bottomLeft :: !(V2 Int)
    , _width      :: !Int
    , _height     :: !Int
    , _points     :: !(VU.Vector Bool)
    } deriving (Show, Eq, Ord, Generic)

$(makeLenses ''Shape)

fromTuples :: [(Int, Int)] -> Shape
fromTuples = fromPoints . map (uncurry V2)

fromPoints :: [V2 Int] -> Shape
fromPoints points0 = runST $ do

    -- Draw only horizontal lines.
    horilmv <- VUM.replicate ((w + 1) * (h + 1)) False
    forM_ lines $ \line ->
        case line of
            HoriLine _ _ _ -> forM_ (pointsOnLine line) $ \p ->
                VUM.write horilmv (idx p) True
            VertLine _ _ _ -> return ()

    horilv <- VU.unsafeFreeze horilmv

    -- Vertical scanline fill
    vertmsv <- VUM.new (w * h)
    forM_ [0 .. w - 1] $ \x -> foldM_
        (\fill y -> do
            let fill' = if horilv VU.! (y * w + x) then not fill else fill
            VUM.write vertmsv (y * w + x) fill'
            return fill')
        False
        [0 .. h - 1]
    vertsv <- VU.unsafeFreeze vertmsv

    return Shape
        { _bottomLeft = bl
        , _width      = w
        , _height     = h
        , _points     = vertsv
        }

  where
    minX = fromMaybe 0 $ minimumOf (traverse . _x) points0
    minY = fromMaybe 0 $ minimumOf (traverse . _y) points0
    maxX = fromMaybe 0 $ maximumOf (traverse . _x) points0
    maxY = fromMaybe 0 $ maximumOf (traverse . _y) points0

    bl = V2 minX minY

    w = maxX - minX
    h = maxY - minY

    lines = linesFromPoints points0

    idx :: V2 Int -> Int
    idx (V2 x y) = (y - minY) * w + (x - minX)

pointsInBoundingBox :: Shape -> [V2 Int]
pointsInBoundingBox shape =
    [ p
    | y <- [0 .. h - 1]
    , x <- [0 .. w - 1]
    , let p = (shape ^. bottomLeft) + V2 x y
    ]
  where
    w = shape ^. width
    h = shape ^. height

member :: V2 Int -> Shape -> Bool
member p shape
    | x < 0 || x >= shape ^. width  = False
    | y < 0 || y >= shape ^. height = False
    | otherwise                     =
        (shape ^. points) VU.! (y * (shape ^. width) + x)
  where
    V2 x y = p - shape ^. bottomLeft

data Line
    = HoriLine !Int !Int !Int  -- X1, Y, X2
    | VertLine !Int !Int !Int  -- X, Y1, Y2
    deriving (Show)

pointsOnLine :: Line -> [V2 Int]
pointsOnLine = \case
    HoriLine x0 y x1 -> [V2 x y | x <- [x0 .. x1 - 1]]
    VertLine x y0 y1 -> [V2 x y | y <- [y0 .. y1 - 1]]

linesFromPoints :: [V2 Int] -> [Line]
linesFromPoints points0 =
    map toLine $ zipWith (,) points0 (drop 1 points0 ++ points0)
  where
    toLine l@(V2 x0 y0, V2 x1 y1)
        | y0 == y1  = HoriLine (min x0 x1) y0 (max x0 x1)
        | x0 == x1  = VertLine x0 (min y0 y1) (max y0 y1)
        | otherwise = error $ "Non rectilinear line: " ++ show l

toHashSet :: Shape -> HS.HashSet (V2 Int)
toHashSet shape = HS.fromList
    [ p
    | p <- pointsInBoundingBox shape
    , member p shape
    ]

toAscii :: Shape -> String
toAscii shape = unlines $ do
    y <- [h - 1, h - 2 .. 0]
    return $ do
        x <- [0 .. w - 1]
        return $ if member (bl + V2 x y) shape then '#' else '.'
  where
    bl = shape ^. bottomLeft
    w  = shape ^. width
    h  = shape ^. height
