{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
module ICFP2019.Shape where
    {-
    ( Shape, width, height, points
    , fromTuples
    , fromPoints

    , pointsInBoundingBox
    , member

    , toHashSet
    , toAscii

    , pointSetToOutline
    ) where
    -}

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

-- | Convert a hash set of points back to a list of edges.
pointSetToOutline :: HS.HashSet (V2 Int) -> [V2 Int]
pointSetToOutline set =
    startPos : takeWhile (/= startPos) (walkInRight startPos)
  where
    walkOutUp :: V2 Int -> [V2 Int]
    walkOutUp p
        | p `HS.member` set         = p : walkInRight p
        | not (l p `HS.member` set) = p : walkOutLeft p
        | otherwise                 = walkOutUp (u p)

    walkInRight :: V2 Int -> [V2 Int]
    walkInRight p
        | d p `HS.member` set     = p : walkInDown p
        | not (p `HS.member` set) = p : walkOutUp p
        | otherwise               = walkInRight (r p)

    walkOutLeft :: V2 Int -> [V2 Int]
    walkOutLeft p
        | l p `HS.member` set           = p : walkOutUp p
        | not (d (l p) `HS.member` set) = p : walkInDown p
        | otherwise                     = walkOutLeft (l p)

    walkInDown :: V2 Int -> [V2 Int]
    walkInDown p
        | l (d p) `HS.member` set   = p : walkOutLeft p
        | not (d p `HS.member` set) = p : walkInRight p
        | otherwise                 = walkInDown (d p)

    -- | Find a corner to start at by just going left as far as possible and then
    -- down as far as possible.
    startPos :: V2 Int
    startPos = case HS.toList set of
        []       -> error $ "startPos: empty shape"
        (p0 : _) -> goLeft p0
      where
        goLeft p
            | l p `HS.member` set = goLeft (l p)
            | d p `HS.member` set = goDown (d p)
            | otherwise           = p

        goDown p
            | d p `HS.member` set = goDown (d p)
            | otherwise           = goLeft p

    u, r, d, l :: V2 Int -> V2 Int
    u p = p + V2 0 1
    r p = p + V2 1 0
    d p = p - V2 0 1
    l p = p - V2 1 0

_test_shape_prob001 = fromTuples
    [(0,0),(6,0),(6,1),(8,1),(8,2),(6,2),(6,3),(0,3)]

_test_shape_prob011 = fromTuples
    [(12,19),(14,19),(14,18),(13,18),(13,17),(14,17),(14,16),(15,16),(15,19),(16,19),(16,15),(13,15),(13,14),(14,14),(14,12),(13,12),(13,13),(12,13),(12,12),(11,12),(11,11),(12,11),(12,10),(13,10),(13,11),(14,11),(14,10),(15,10),(15,11),(16,11),(16,10),(17,10),(17,11),(18,11),(18,12),(15,12),(15,14),(16,14),(16,13),(17,13),(17,14),(18,14),(18,13),(19,13),(19,14),(20,14),(20,15),(17,15),(17,16),(19,16),(19,18),(17,18),(17,19),(20,19),(20,16),(21,16),(21,19),(22,19),(22,15),(23,15),(23,17),(24,17),(24,18),(23,18),(23,19),(25,19),(25,15),(26,15),(26,14),(25,14),(25,13),(26,13),(26,11),(27,11),(27,9),(25,9),(25,12),(24,12),(24,9),(23,9),(23,10),(22,10),(22,9),(20,9),(20,10),(19,10),(19,9),(18,9),(18,6),(19,6),(19,5),(20,5),(20,6),(21,6),(21,7),(27,7),(27,6),(24,6),(24,5),(25,5),(25,4),(26,4),(26,5),(27,5),(27,4),(28,4),(28,11),(29,11),(29,14),(27,14),(27,15),(28,15),(28,16),(29,16),(29,17),(28,17),(28,18),(26,18),(26,19),(27,19),(27,20),(28,20),(28,21),(27,21),(27,22),(26,22),(26,25),(27,25),(27,24),(28,24),(28,25),(29,25),(29,26),(28,26),(28,27),(27,27),(27,26),(26,26),(26,29),(23,29),(23,28),(22,28),(22,27),(23,27),(23,26),(22,26),(22,24),(21,24),(21,23),(22,23),(22,22),(18,22),(18,23),(19,23),(19,24),(18,24),(18,25),(17,25),(17,26),(19,26),(19,25),(20,25),(20,26),(21,26),(21,27),(19,27),(19,28),(18,28),(18,27),(17,27),(17,28),(16,28),(16,27),(15,27),(15,26),(16,26),(16,25),(14,25),(14,29),(13,29),(13,27),(12,27),(12,26),(13,26),(13,25),(12,25),(12,24),(11,24),(11,27),(10,27),(10,24),(9,24),(9,29),(5,29),(5,26),(8,26),(8,24),(6,24),(6,25),(5,25),(5,24),(4,24),(4,28),(3,28),(3,27),(1,27),(1,25),(3,25),(3,24),(0,24),(0,21),(3,21),(3,20),(2,20),(2,19),(3,19),(3,18),(6,18),(6,21),(8,21),(8,20),(7,20),(7,19),(8,19),(8,18),(7,18),(7,17),(8,17),(8,16),(7,16),(7,15),(8,15),(8,14),(6,14),(6,16),(4,16),(4,14),(3,14),(3,13),(2,13),(2,18),(1,18),(1,13),(0,13),(0,10),(3,10),(3,9),(4,9),(4,8),(5,8),(5,9),(8,9),(8,7),(7,7),(7,8),(6,8),(6,7),(2,7),(2,8),(1,8),(1,7),(0,7),(0,4),(3,4),(3,2),(8,2),(8,1),(12,1),(12,0),(13,0),(13,1),(15,1),(15,0),(16,0),(16,1),(17,1),(17,0),(18,0),(18,1),(19,1),(19,2),(22,2),(22,3),(19,3),(19,4),(17,4),(17,8),(15,8),(15,9),(14,9),(14,8),(13,8),(13,4),(10,4),(10,5),(12,5),(12,7),(10,7),(10,15),(11,15),(11,16),(10,16),(10,21),(12,21)]

_test_shape_prob_weird = fromPoints $ pointSetToOutline $ HS.fromList
    [ V2 x y
    | x <- [0 .. 4]
    , y <- [0 .. 4]
    , x /= 2 || y == 4
    ]
