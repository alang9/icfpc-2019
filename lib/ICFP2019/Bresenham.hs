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
        !yi  = if dy0 < 0 then -1 else 1 in

    let go !d !x !y
            | x > x1    = []
            | otherwise = V2 x y :
                let y' = if d > 0 then y + yi else y
                    d' = if d > 0 then d - 2 * dx else d in
                go (d' + 2 * dy) (x + 1) y' in

    go (2 * dy - dx) x0 y0

plotLineHigh :: V2 Int -> V2 Int -> [V2 Int]
plotLineHigh (V2 x0 y0) (V2 x1 y1) =

    let !dx0 = x1 - x0
        !dy  = y1 - y0
        !dx  = if dx0 < 0 then -dx0 else dx0
        !xi  = if dx0 < 0 then -1 else 1 in

    let go !d !x !y
            | y > y1    = []
            | otherwise = V2 x y :
                let x'  = if d > 0 then x + xi else x
                    d'  = if d > 0 then d - 2 * dy else d in
                go (d' + 2 * dx) x' (y + 1) in

    go (2 * dx - dy) x0 y0
