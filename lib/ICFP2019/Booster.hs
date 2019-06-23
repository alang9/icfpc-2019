{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase    #-}
module ICFP2019.Booster where

import           Control.Lens        (preview, review)
import           Control.Lens.Prism  (Prism', prism')
import           Data.Char           (isSpace)
import           Data.Hashable       (Hashable)
import qualified Data.HashMap.Strict as HMS
import           GHC.Generics        (Generic)

data Booster
  = Extension
  | FastWheels
  | Drill
  | Teleport
  | Mysterious
  | Clone
  deriving (Show, Eq, Ord, Generic)

instance Hashable Booster

boosterFromCode :: Prism' Char Booster
boosterFromCode = prism' toCode fromCode
  where
    fromCode = \case
        'B' -> Just Extension
        'F' -> Just FastWheels
        'L' -> Just Drill
        'X' -> Just Mysterious
        'R' -> Just Teleport
        'C' -> Just Clone
        _   -> Nothing

    toCode = \case
        Extension  -> 'B'
        FastWheels -> 'F'
        Drill      -> 'L'
        Mysterious -> 'X'
        Teleport   -> 'R'
        Clone      -> 'C'

newtype BoosterBag = BoosterBag {unBoosterBag :: HMS.HashMap Booster Int}

instance Semigroup BoosterBag where
    BoosterBag x <> BoosterBag y = BoosterBag (HMS.unionWith (+) x y)

instance Monoid BoosterBag where
    mempty  = BoosterBag HMS.empty
    mappend = (<>)

parseBoosterBag :: String -> Maybe BoosterBag
parseBoosterBag =
    fmap (BoosterBag . HMS.fromListWith (+) . flip zip (repeat 1)) .
    mapM (preview boosterFromCode) .
    filter (not . isSpace)

unparseBoosterBag :: BoosterBag -> String
unparseBoosterBag =
    concat .
    map (\(b, c) -> replicate c (review boosterFromCode b)) .
    HMS.toList . unBoosterBag

readBoosterBag :: FilePath -> IO BoosterBag
readBoosterBag path = do
    content <- readFile path
    case parseBoosterBag content of
        Nothing -> fail $ "Error parsing booster bag " ++ show path
        Just bb -> return bb

writeBoosterBag :: FilePath -> BoosterBag -> IO ()
writeBoosterBag path = writeFile path . unparseBoosterBag
