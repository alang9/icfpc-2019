{-# LANGUAGE RecordWildCards #-}

module ICFP2019.CoinPuzzle where

import Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString.Char8 as AP
import qualified Data.ByteString.Char8 as C8
import Data.List
import Data.Graph.AStar
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Linear hiding (trace)

import Debug.Trace

data Puzzle = Puzzle
  { bNum :: !Int
  , eNum :: !Int
  , tSize :: !Int
  , vMin :: !Int
  , vMax :: !Int
  , mNum :: !Int
  , fNum :: !Int
  , dNum :: !Int
  , rNum :: !Int
  , cNum :: !Int
  , xNum :: !Int
  , iSqs :: ![Point]
  , oSqs :: ![Point]
  }

type Point = V2 Int

puzzleParser :: Parser Puzzle
puzzleParser = do
  bNum <- AP.decimal
  _ <- AP.char ','
  eNum <- AP.decimal
  _ <- AP.char ','
  tSize <- AP.decimal
  _ <- AP.char ','
  vMin <- AP.decimal
  _ <- AP.char ','
  vMax <- AP.decimal
  _ <- AP.char ','
  mNum <- AP.decimal
  _ <- AP.char ','
  fNum <- AP.decimal
  _ <- AP.char ','
  dNum <- AP.decimal
  _ <- AP.char ','
  rNum <- AP.decimal
  _ <- AP.char ','
  cNum <- AP.decimal
  _ <- AP.char ','
  xNum <- AP.decimal
  _ <- AP.char '#'
  iSqs <- AP.sepBy pointParser (AP.char ',')
  _ <- AP.char '#'
  oSqs <- AP.sepBy pointParser (AP.char ',')
  return Puzzle {..}
  where
    pointParser = do
      _ <- AP.char '('
      x <- AP.decimal
      _ <- AP.char ','
      y <- AP.decimal
      _ <- AP.char ')'
      return $ V2 x y

findArea :: Puzzle -> HashSet Point
findArea Puzzle {..} = trace pp finalArea
  where
    pp = unlines
      [ [ case () of
            _ | elem (V2 x y) oSqs -> 'o'
              | elem (V2 x y) iSqs -> '.'
              | HS.member (V2 x y) finalArea -> '#'
              | otherwise -> ' '
        | x <- [0..tSize - 1]]
      | y <- [0..tSize-1]]
    (finalArea, _) = foldl' carve (initialArea, initialTarget) oSqs
    initialArea = HS.fromList [ V2 x y | x <- [0..tSize - 1], y <- [0..tSize - 1]]
    initialTarget = HS.fromList $
      [ V2 x y | x <- [0..tSize - 1], y <- [0, tSize - 1]] ++
      [ V2 x y | x <- [0, tSize - 1], y <- [0..tSize - 1]]
    forbidden = HS.fromList iSqs
    carve (area, target) pt = (newArea, newTarget)
      where
        newArea = foldl' (\a pt -> HS.delete pt a ) area path
        newTarget = target `HS.union` HS.fromList [ p + d | p <- path, d <- neigh]
        neigh = [V2 0 1, V2 0 (-1), V2 1 0, V2 (-1) 0]
        path :: [Point]
        path = maybe (error "findArea: fail") id $
          aStar
            (\p -> HS.fromList $ filter (\p' -> not $ HS.member p' forbidden) $ (+) p <$> neigh)
            (\_ _ -> 1)
            (const 0)
            (\p -> HS.member p target)
            pt

puzzle1Bs :: C8.ByteString
puzzle1Bs = C8.pack "2,1,200,400,1200,6,10,5,1,3,4#(147,109),(123,95),(28,120),(105,50),(106,123),(131,82),(153,10),(63,96),(116,188),(112,62),(121,56),(151,140),(142,55),(63,184),(47,170),(90,144),(39,117),(82,159),(116,92),(48,128),(171,119),(96,97),(169,158),(106,156),(87,175),(100,37),(42,62),(29,195),(97,145),(181,162),(170,64),(95,101),(75,106),(99,87),(162,129),(188,10),(177,79),(117,116),(17,92),(176,129),(16,108),(111,88),(34,142),(44,146),(147,18),(107,109),(173,12),(59,190),(102,129),(56,102)#(61,157),(143,168),(15,169),(184,195),(156,102),(81,73),(133,179),(2,148),(187,193),(175,101),(23,83),(131,1),(85,29),(115,168),(64,55),(52,133),(105,148),(80,14),(115,34),(1,82),(46,52),(42,138),(184,194),(135,139),(178,81),(0,50),(128,35),(140,7),(69,149),(181,193),(167,67),(119,151),(151,188),(197,2),(148,80),(191,94),(16,195),(61,28),(156,176),(196,47),(81,188),(60,177),(87,80),(171,100),(18,40),(128,25),(1,22),(116,130),(170,170),(123,19)"

main :: IO ()
main = do
    print $ findArea puzzle1
  where
    Right puzzle1 = AP.parseOnly puzzleParser puzzle1Bs
