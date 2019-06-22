{-# LANGUAGE RecordWildCards #-}

module ICFP2019.Main.CoinPuzzle where

import Data.Attoparsec.ByteString
import System.Environment (getArgs)
import qualified Data.Attoparsec.ByteString.Char8 as AP
import qualified Data.ByteString.Char8 as C8
import Control.Monad.Primitive
import Data.List
import Data.Graph.AStar
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Linear hiding (trace)
import System.Random.MWC
import qualified ICFP2019.Shape as Shape
import qualified ICFP2019.State as State

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
    newInside = foldl' (\a pt -> HS.delete pt a ) psInside path
    newOutside = psOutside `HS.union` HS.fromList [ p + d | p <- path, d <- neigh]
    neigh = [V2 0 1, V2 0 (-1), V2 1 0, V2 (-1) 0]
    path :: [Point]
    path = maybe (error "findArea: fail") id $
      aStar
        (\p -> HS.fromList $ filter (\p' -> not $ HS.member p' psRequired) $ (+) p <$> neigh)
        (\_ _ -> 1)
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

    -- TODO: cNum -> Clone
    boosterCodes =
        replicate mNum State.Extension ++
        replicate fNum State.FastWheels ++
        replicate dNum State.Drill ++
        replicate rNum State.Teleport ++
        replicate xNum State.Mysterious

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


puzzle1Bs :: C8.ByteString
puzzle1Bs = C8.pack "2,1,200,400,1200,6,10,5,1,3,4#(147,109),(123,95),(28,120),(105,50),(106,123),(131,82),(153,10),(63,96),(116,188),(112,62),(121,56),(151,140),(142,55),(63,184),(47,170),(90,144),(39,117),(82,159),(116,92),(48,128),(171,119),(96,97),(169,158),(106,156),(87,175),(100,37),(42,62),(29,195),(97,145),(181,162),(170,64),(95,101),(75,106),(99,87),(162,129),(188,10),(177,79),(117,116),(17,92),(176,129),(16,108),(111,88),(34,142),(44,146),(147,18),(107,109),(173,12),(59,190),(102,129),(56,102)#(61,157),(143,168),(15,169),(184,195),(156,102),(81,73),(133,179),(2,148),(187,193),(175,101),(23,83),(131,1),(85,29),(115,168),(64,55),(52,133),(105,148),(80,14),(115,34),(1,82),(46,52),(42,138),(184,194),(135,139),(178,81),(0,50),(128,35),(140,7),(69,149),(181,193),(167,67),(119,151),(151,188),(197,2),(148,80),(191,94),(16,195),(61,28),(156,176),(196,47),(81,188),(60,177),(87,80),(171,100),(18,40),(128,25),(1,22),(116,130),(170,170),(123,19)"

main :: IO ()
main = do
    (arg : _) <- getArgs
    content   <- C8.readFile arg
    puzzle <- case AP.parseOnly puzzleParser content of
        Left err -> fail $ "Puzzle parse fail: " ++ show err
        Right x  -> return x
    gen <- createSystemRandom
    sol <- keepCarving gen puzzle (initialCarves puzzle)
    putStr $ unparseTaskDescription $
        partialSolutionToTaskDescription puzzle sol
