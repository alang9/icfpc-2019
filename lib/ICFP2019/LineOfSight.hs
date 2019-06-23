{-# LANGUAGE BangPatterns #-}
module ICFP2019.LineOfSight where

import           Linear (V2 (..))

--------------------------------------------------------------------------------

-- Check what tiles we need to test to see if a point is visible from the
-- origin.  Does not include the starting point (robot) not the ending point
-- (target we want to wrap).

lineOfSight :: V2 Int -> [V2 Int]
lineOfSight =
    safeInit . drop 1 .
    uniques .
    map halve .
    plotLine (V2 1 1) .
    (+ one) . double
  where
    halve  (V2 x y) = V2 (x `div` 2) (y `div` 2)
    double (V2 x y) = V2 (x * 2) (y * 2)
    one = V2 1 1

--------------------------------------------------------------------------------

-- More or less straight from wikipedia.

plotLine :: V2 Int -> V2 Int -> [V2 Int]
plotLine (V2 x0 y0) (V2 x1 y1)
    | abs (y1 - y0) < abs (x1 - x0) =
        if x0 > x1
            then plotLineX (V2 x1 y1) (V2 x0 y0)
            else plotLineX (V2 x0 y0) (V2 x1 y1)
    | otherwise =
        if y0 > y1
            then plotLineY (V2 x1 y1) (V2 x0 y0)
            else plotLineY (V2 x0 y0) (V2 x1 y1)

plotLineX :: V2 Int -> V2 Int -> [V2 Int]
plotLineX (V2 x0 y0) (V2 x1 y1) =

    let !dx   = x1 - x0
        !dy   = abs (y1 - y0)
        !ysig = if y1 - y0 < 0 then -1 else 1

        go x _ | x >= dx = []
        go x y =
            let l = y
                r = y + dy in
            [V2 x (ysig * (l `div` dx))] ++
            [V2 x (ysig * (r `div` dx)) | r `mod` dx /= 0] ++
            go (x + 1) r in

    map (+ (V2 x0 (if ysig > 0 then y0 else y0 - 1))) $ uniques $ go 0 0

plotLineY :: V2 Int -> V2 Int -> [V2 Int]
plotLineY (V2 x0 y0) (V2 x1 y1) =

    let !dx   = abs (x1 - x0)
        !dy   = y1 - y0
        !xsig = if x1 - x0 < 0 then -1 else 1

        go _ y | y >= dy = []
        go x y =
            let d = x
                u = x + dx in
            [V2 (xsig * (d `div` dy)) y] ++
            [V2 (xsig * (u `div` dy)) y | u `mod` dy /= 0] ++
            go u (y + 1) in

    map (+ (V2 (if xsig > 0 then x0 else x0 - 1) y0)) $ uniques $ go 0 0


--------------------------------------------------------------------------------

safeInit :: [a] -> [a]
safeInit (x : _ : []) = x : []
safeInit (x : y : zs) = x : safeInit (y : zs)
safeInit _            = []

uniques :: Eq a => [a] -> [a]
uniques [] = []
uniques (x : xs) = x : go x xs
  where
    go _ []         = []
    go y (z : zs)
        | y == z    = go y zs
        | otherwise = z : go z zs
