{-# LANGUAGE RecordWildCards #-}

module ICFP2019.Main.CoinPuzzle where

import Data.Attoparsec.ByteString
import System.Environment (getArgs)
import qualified Data.Attoparsec.ByteString.Char8 as AP
import qualified Data.ByteString.Char8 as C8
import Control.Monad.Primitive
import Data.List
import Data.Graph.AStar
import qualified Data.HashSet as HS
import Linear hiding (trace)
import System.Random.MWC
import qualified System.IO as IO
import qualified ICFP2019.Shape as Shape
import qualified ICFP2019.State as State

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
  } deriving (Show)

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

data PartialSolution = PartialSolution
    -- Filled spaces inside the solution
    { psInside   :: !(HS.HashSet Point)
    -- Targets outside the solution that we can carve paths to.
    , psOutside  :: !(HS.HashSet Point)
    -- Must stay inside.
    , psRequired :: !(HS.HashSet Point)
    } deriving (Show)

partialSolutionToAscii :: Puzzle -> PartialSolution -> String
partialSolutionToAscii Puzzle {..} PartialSolution {..} = unlines
  [ [ case () of
        _ | elem (V2 x y) oSqs -> 'o'
          | elem (V2 x y) iSqs -> '.'
          | HS.member (V2 x y) psInside -> '#'
          | otherwise -> ' '
    | x <- [0..tSize - 1]]
  | y <- [tSize-1,tSize-2..0]]

initialCarves :: Puzzle -> PartialSolution
initialCarves Puzzle {..} = finalPartialSolution
  where
    initialPartialSolution = PartialSolution
        { psInside = HS.fromList
            [V2 x y | x <- [0..tSize - 1], y <- [0..tSize - 1]]
        , psOutside = HS.fromList $
            [V2 x y | x <- [0..tSize - 1], y <- [0, tSize - 1]] ++
            [V2 x y | x <- [0, tSize - 1], y <- [0..tSize - 1]]
        , psRequired = HS.fromList iSqs
        }

    finalPartialSolution = foldl' carve initialPartialSolution oSqs

keepCarving
    :: PrimMonad m
    => Gen (PrimState m) -> Puzzle -> PartialSolution -> m PartialSolution
keepCarving gen puzzle@Puzzle {..} ps
    | vertices < vMin = carveRandomPoint gen puzzle ps >>= keepCarving gen puzzle
    | otherwise       = return ps
  where
    vertices = length $ Shape.pointSetToOutline (psInside ps)

carveRandomPoint
    :: PrimMonad m
    => Gen (PrimState m) -> Puzzle -> PartialSolution -> m PartialSolution
carveRandomPoint gen puzzle@Puzzle {..} ps@PartialSolution {..} = do
    pt <- V2 <$> uniformR (0, tSize - 1) gen <*> uniformR (0, tSize - 1) gen
    if pt `HS.member` psInside && not (pt `HS.member` psRequired)
        then return $! carve ps pt
        else carveRandomPoint gen puzzle ps

carve :: PartialSolution -> Point -> PartialSolution
carve PartialSolution {..} pt = PartialSolution newInside newOutside psRequired
  where
    newInside = foldl' (\a p -> HS.delete p a ) psInside path
    newOutside = psOutside `HS.union` HS.fromList [p + d | p <- path, d <- neigh]
    neigh = [V2 0 1, V2 0 (-1), V2 1 0, V2 (-1) 0]
    path :: [Point]
    path = maybe (error "findArea: fail") (pt :) $
      aStar
        (\p -> HS.fromList $ filter (\p' -> not $ HS.member p' psRequired) $ (+) p <$> neigh)
        (\_ _ -> 1 :: Int)
        (const 0)
        (\p -> HS.member p psOutside)
        pt

data TaskDescription = TaskDescription
    { tdMap       :: [V2 Int]
    , tdStart     :: V2 Int
    , tdObstacles :: [[V2 Int]]
    , tdBoosters  :: [(State.Booster, V2 Int)]
    }

partialSolutionToTaskDescription
    :: Puzzle -> PartialSolution -> TaskDescription
partialSolutionToTaskDescription Puzzle {..} ps = TaskDescription
    { tdMap       = Shape.pointSetToOutline (psInside ps)
    , tdStart     = start
    , tdObstacles = []
    , tdBoosters  = zip boosterCodes boosterLocations
    }
  where
    (start, boosterLocations) = case HS.toList (psInside ps) of
        []      -> error "partialSolutionToTaskDescription: empty inside"
        (s : b) -> (s, b)

    boosterCodes =
        replicate mNum State.Extension ++
        replicate fNum State.FastWheels ++
        replicate dNum State.Drill ++
        replicate rNum State.Teleport ++
        replicate xNum State.Mysterious ++
        replicate cNum State.Clone

unparseTaskDescription :: TaskDescription -> String
unparseTaskDescription TaskDescription {..} =
    unparseMap tdMap ++ "#" ++
    unparsePoint tdStart ++ "#" ++
    intercalate ";" (map unparseMap tdObstacles) ++ "#" ++
    intercalate ";" (map unparseBooster tdBoosters)
  where
    unparseMap = intercalate "," . map unparsePoint

    unparsePoint (V2 x y) = "(" ++ show x ++ "," ++ show y ++ ")"

    unparseBooster (c, p) = unparseBoosterCode c ++ unparsePoint p

    unparseBoosterCode State.Extension  = "B"
    unparseBoosterCode State.FastWheels = "F"
    unparseBoosterCode State.Drill      = "L"
    unparseBoosterCode State.Mysterious = "X"
    unparseBoosterCode State.Teleport   = "R"
    unparseBoosterCode State.Clone      = "C"

main :: IO ()
main = do
    (arg : _) <- getArgs
    content   <- C8.readFile arg
    puzzle <- case AP.parseOnly puzzleParser content of
        Left err -> fail $ "Puzzle parse fail: " ++ show err
        Right x  -> return x
    gen <- createSystemRandom
    sol <- keepCarving gen puzzle (initialCarves puzzle)

    IO.hPutStrLn IO.stderr $ partialSolutionToAscii puzzle sol
    putStr $ unparseTaskDescription $
        partialSolutionToTaskDescription puzzle sol
