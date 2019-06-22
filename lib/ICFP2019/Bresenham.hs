{-# LANGUAGE BangPatterns #-}
module ICFP2019.Bresenham where

import           Linear (V2 (..))

--------------------------------------------------------------------------------

-- Check what tiles we need to test to see if a point is visible from the
-- origin.

visible :: V2 Int -> [V2 Int]
visible = init . drop 1 . bresenham (V2 0 0)

--------------------------------------------------------------------------------

-- More or less straight from wikipedia.

bresenham :: V2 Int -> V2 Int -> [V2 Int]
bresenham (V2 x0 y0) (V2 x1 y1)
    | abs (y1 - y0) < abs (x1 - x0) =
        if x0 > x1
            then plotLineLow (V2 x1 y1) (V2 x0 y0)
            else plotLineLow (V2 x0 y0) (V2 x1 y1)
    | otherwise =
        if y0 > y1
            then plotLineHigh (V2 x1 y1) (V2 x0 y0)
            else plotLineHigh (V2 x0 y0) (V2 x1 y1)

plotLineLow :: V2 Int -> V2 Int -> [V2 Int]
plotLineLow (V2 x0 y0) (V2 x1 y1) =

    let !dx  = x1 - x0
        !dy0 = y1 - y0
        !dy  = if dy0 < 0 then -dy0 else dy0
        !yi  = if dy0 < 0 then -1 else 1

        -- Does a line go nicely through corners?
        smooth = (dx `mod` dy == 0) && (odd $ dx `div` dy) in

    let go !d !x !y
            | x > x1    = []
            | d > 0     =
                let y' = y + yi
                    d' = d - 2 * dx in
                [V2 x y] ++
                [V2 x y' | not smooth && x + 1 <= x1] ++
                go (d' + 2 * dy) (x + 1) y'
            | otherwise =
                V2 x y : go (d + 2 * dy) (x + 1) y in

    go (2 * dy - dx) x0 y0

plotLineHigh :: V2 Int -> V2 Int -> [V2 Int]
plotLineHigh (V2 x0 y0) (V2 x1 y1) =

    let !dx0 = x1 - x0
        !dy  = y1 - y0
        !dx  = if dx0 < 0 then -dx0 else dx0
        !xi  = if dx0 < 0 then -1 else 1

        -- Does a line go nicely through corners?
        smooth = (dy `mod` dx == 0) && (odd $ dy `div` dx) in

    let go !d !x !y
            | y > y1    = []
            | d > 0     =
                let x' = x + xi
                    d' = d - 2 * dy in
                [V2 x y] ++
                [V2 x' y | not smooth && y + 1 <= y1] ++
                go (d' + 2 * dx) x' (y + 1)
            | otherwise =
                V2 x y : go (d + 2 * dx) x (y + 1) in

    go (2 * dx - dy) x0 y0
