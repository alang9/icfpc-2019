{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Util
  ( opaqueId
  , at0, at
  , rep
  , iter
  , cross
  , bucket
  , histogram
  , buildGraphlike
  , mergesBy
  , foldBalanced
  , mergeBy
  , maximumIndex
  , lastOccurrenceTable
  , isLeapYear
  , toDecimal
  , fromDecimal
  , dp1
  , dp1r
  , dp2
  , dp3
  , bsToVector
  , i2d
  , (^^^)
  , forceU
  , forceList
  , g_unsafeSplitAt
  , g_mapAccumL
  , inplacePrescanl
  , pow1, mpow, pow
  , pack32, unpack32
  , whenM
  , replicate2
  , generate2
  , Int2Tuple(..)
  , i2t_fst
  , i2t_snd
  , Int3Tuple(..)
  , i3t_fst
  , i3t_snd
  , i3t_trd
  , i3tToTuple
  , STInt
  , toSTInt
  , fromSTInt
  , catchAnswer
  , throwAnswer
  , bitScanForward
  , bsfTable
  , bitScanReverse
  , log2Ceil
  , roundDown
  , roundUp
  , cdiv
  , is_lookupLE
  , is_lookupGE
  , is_lookupLT
  , is_lookupGT
  , s_lookupLE
  , s_lookupGE
  , s_lookupLT
  , s_lookupGT
  , im_lookupLE
  , im_lookupGE
  , im_lookupLT
  , im_lookupGT
  , m_lookupLE
  , m_lookupGE
  , m_lookupLT
  , m_lookupGT
  ) where

import Control.Applicative
import qualified Control.Exception as Ex
import qualified Control.Monad.Primitive as Primitive
import Control.Monad
import Control.Monad.ST
import Data.Bits
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Unsafe as BS
import Data.Char
import Data.Function
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import qualified Data.Set as S
import Data.Typeable
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Data.Word
import GHC.Exts (State#, Int#, Int(..))
import GHC.ST (ST(..))
import Prelude

opaqueId :: a -> a
opaqueId = id
{-# NOINLINE opaqueId #-}

at0 :: (Num a, G.Vector v a) => v a -> Int -> a
at0 v i = at 0 v i
{-# INLINE at0 #-}

at :: (G.Vector v a) => a -> v a -> Int -> a
at dflt v i = fromMaybe dflt $ v G.!? i
{-# INLINE at #-}

rep :: (Monad m) => Int -> (Int -> m ()) -> m ()
rep n f = go 0
  where
    go k
      | k >= n = return ()
      | otherwise = do
          f k
          go $ k + 1
{-# INLINE rep #-}

iter :: Int -> (a -> a) -> a -> a
iter n0 f = go n0
  where
    go n !x
      | n <= 0 = x
      | otherwise = go (n-1) (f x)
{-# INLINE iter #-}

cross
  :: (G.Vector v a, G.Vector v b, G.Vector v (a, b))
  => v a -> v b -> v (a, b)
cross v0 v1 = G.generate (len0 * len1) $ \ij ->
  let
    !(i, j) = quotRem ij len1
    !x = G.unsafeIndex v0 i
    !y = G.unsafeIndex v1 j
  in (x, y)
  where
    !len0 = G.length v0
    !len1 = G.length v1
{-# INLINE cross #-}

histogram :: Int -> U.Vector Int -> U.Vector Int
histogram sz vec = U.accumulate (+) (U.replicate sz 0) $ U.map (,1) vec

bucket
  :: forall a v
  .  (G.Vector v a, G.Vector v (Int, a))
  => Int
  -> v (Int, a)
  -> V.Vector (v a)
bucket !nbuckets !vec = buildGraphlike nbuckets (G.length vec) trav
  where
    trav :: Monad f => (Int -> a -> f ()) -> f ()
    trav f = G.forM_ vec $ \(from, to) -> f from to
    {-# INLINE trav #-}
{-# INLINE bucket #-}

buildGraphlike
  :: (G.Vector outv outedge)
  => Int
  -> Int
  -> (forall m. (Monad m) => (Int -> outedge -> m ()) -> m ())
  -> V.Vector (outv outedge)
buildGraphlike !nv !nEdges traverseEdges_ = runST $ do
  !counter <- UM.replicate nv 0
  let incr v _ = UM.unsafeWrite counter v . (+1) =<< UM.read counter v
  traverseEdges_ incr
  inplacePrescanl (+) 0 counter
  !mpool <- GM.new nEdges
  let
    add a oe = do
      i <- UM.unsafeRead counter a
      GM.unsafeWrite mpool i oe
      UM.unsafeWrite counter a $ i + 1
  traverseEdges_ add
  pool <- G.unsafeFreeze mpool
  V.generateM nv $ \i -> do
    begin <- if i == 0 then return 0 else UM.unsafeRead counter (i - 1)
    end <- UM.unsafeRead counter i
    return $! G.unsafeSlice begin (end - begin) pool
{-# INLINE buildGraphlike #-}

mergesBy :: (G.Vector v [a]) => (a -> a -> Ordering) -> v [a] -> [a]
mergesBy cmp lists = fromMaybe [] $ foldBalanced (mergeBy cmp) lists
{-# INLINE mergesBy #-}

foldBalanced :: (G.Vector v a) => (a -> a -> a) -> v a -> Maybe a
foldBalanced f vec0
  | G.null vec0 = Nothing
  | otherwise = Just $! go vec0
  where
    go vec
      | G.length vec <= 1 = G.head vec
      | otherwise = let
          !(x, y) = G.splitAt (G.length vec `shiftR` 1) vec
          in f (go x) (go y)
{-# INLINE foldBalanced #-}

mergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeBy cmp = go
  where
    go [] ys = ys
    go xs [] = xs
    go xxs@(x:xs) yys@(y:ys) = case cmp x y of
      LT -> x : go xs yys
      EQ -> x : y : go xs ys
      GT -> y : go xxs ys
{-# INLINE mergeBy #-}

maximumIndex :: (G.Vector v (Int, a), G.Vector v a, Ord a) => v a -> Maybe Int
maximumIndex vec
  | G.null vec = Nothing
  | otherwise = Just $! fst $ G.maximumBy (compare `on` snd)
      $ G.indexed vec

-- | Returns a vector whose @i@-th element is the index of the last
-- ocurrence of @i@ in the input vector, or @-1@ if there is no such ocurrence.
lastOccurrenceTable
  :: Int -- ^ The size of the returned vector
  -> U.Vector Int
  -> U.Vector Int
lastOccurrenceTable nv
  = U.accumulate (flip const) (U.replicate nv (-1))
  . U.map (uncurry $ flip (,))
  . U.indexed

isLeapYear :: Int -> Bool
isLeapYear y = m 4 `except` (m 100 `except` m 400)
  where
    m x = mod y x == 0
    a `except` b = a && not b

toDecimal :: Int -> [Int]
toDecimal = map digitToInt . show

fromDecimal :: [Int] -> Int
fromDecimal xs0 = foldl' step 0 xs0
  where
    step !a !d
      | 0 <= d && d < 10 = a * 10 + d
      | otherwise = error $
        "fromDecimal: bad digit: " ++ show xs0

dp1
  :: (G.Vector v a)
  => a
  -> Int
  -> ((Int -> a) -> Int -> a)
  -> v a
dp1 dflt n f = G.constructN n $ \vec ->
  let
    !i = G.length vec
    g k
      | k < 0 = dflt
      | k >= i = error $ "dp[" ++ show i ++ "]: out of bounds: " ++ show k
      | otherwise = vec `G.unsafeIndex` k
  in f g i
{-# INLINE dp1 #-}

dp1r
  :: (G.Vector v a)
  => Int
  -> ((Int -> a) -> Int -> a)
  -> v a
dp1r n f = G.constructrN n $ \vec ->
  let
    !i = n - G.length vec - 1
    g k
      | k <= i || k >= n = error $ "dp[" ++ show i ++ "]: out of bounds: " ++ show k
      | otherwise = vec `G.unsafeIndex` (k - i - 1)
  in f g i
{-# INLINE dp1r #-}

dp2
  :: (G.Vector v a)
  => a
  -> Int
  -> Int
  -> ((Int -> Int -> a) -> Int -> Int -> a)
  -> V.Vector (v a)
dp2 dflt m n f = V.constructN m $ \outer ->
  let
    !i = V.length outer
  in G.constructN n $ \inner ->
  let
    !j = G.length inner
    g x y
      | x < 0 || y < 0 = dflt
      | (x, y) >= (i, j) = error $ "dp[" ++ show (i, j) ++ "]: out of bounds: " ++ show (x, y)
      | x == i = inner `G.unsafeIndex` y
      | otherwise = outer `V.unsafeIndex` x `G.unsafeIndex` y
  in f g i j
{-# INLINE dp2 #-}

dp3
  :: (G.Vector v a)
  => a
  -> Int
  -> Int
  -> Int
  -> ((Int -> Int -> Int -> a) -> Int -> Int -> Int -> a)
  -> V.Vector (V.Vector (v a))
dp3 dflt n0 n1 n2 f = V.constructN n0 $ \v0 ->
  let
    !i0 = V.length v0
  in V.constructN n1 $ \v1 ->
  let
    !i1 = V.length v1
  in G.constructN n2 $ \v2 ->
  let
    !i2 = G.length v2
    g x0 x1 x2
      | x0 < 0 || x1 < 0 || x2 < 0 = dflt
      | (x0, x1, x2) >= (i0, i1, i2) = error $
          "dp[" ++ show (i0, i1, i2) ++ "]: out of bounds: "
            ++ show (x0, x1, x2)
      | (x0, x1) == (i0, i1) = v2 `G.unsafeIndex` x2
      | x0 == i0 = v1 `V.unsafeIndex` x1 `G.unsafeIndex` x2
      | otherwise = v0 `V.unsafeIndex` x0 `V.unsafeIndex` x1 `G.unsafeIndex` x2
  in f g i0 i1 i2
{-# INLINE dp3 #-}

bsToVector :: BS.ByteString -> U.Vector Char
bsToVector str = U.generate (BS.length str) $ \i ->
  -- TODO: toEnum is slow
  toEnum $ fromIntegral $ BS.unsafeIndex str i

i2d :: (Integral a) => a -> Double
i2d = fromIntegral

(^^^) :: (Num a) => a -> Int -> a
(^^^) = (^)

forceList :: [a] -> [a]
forceList = foldr (\x xs -> x `seq` (x : xs)) []

forceU :: (U.Unbox a) => a -> a
forceU x = G.elemseq (vec x) x x
  where
    vec :: a -> U.Vector a
    vec = undefined
{-# INLINE forceU #-}

g_unsafeSplitAt :: (G.Vector v a) => Int -> v a -> (v a, v a)
g_unsafeSplitAt k vec = (G.unsafeTake k vec, G.unsafeDrop k vec)
{-# INLINE g_unsafeSplitAt #-}

g_mapAccumL
  :: (G.Vector v a, G.Vector v1 b)
  => (acc -> a -> (acc, b))
  -> acc
  -> v a
  -> v1 b
g_mapAccumL f !initial !v = G.unfoldrN (G.length v) f' (0, initial)
  where
    f' (!i, acc)
      | i == G.length v = Nothing
      | otherwise = let
          !(Box x) = G.unsafeIndexM v i
          !(acc', out) = f acc x
          !i' = i + 1
          in Just (out, (i', acc'))

data Box a = Box a
  deriving (Functor)

instance Applicative Box where
  pure = return
  (<*>) = ap

instance Monad Box where
  return = Box
  Box a >>= f = f a

inplacePrescanl
  :: (Primitive.PrimMonad m, GM.MVector v a)
  => (a -> a -> a)
  -> a
  -> v (Primitive.PrimState m) a
  -> m ()
inplacePrescanl f x0 !mv = go 0 x0
  where
    go !i !_
      | i >= GM.length mv = return ()
    go !i !x = do
      old <- GM.unsafeRead mv i
      GM.unsafeWrite mv i x
      go (i+1) $ f x old
{-# INLINE inplacePrescanl #-}

pow1 :: (a -> a -> a) -> Int -> a -> a
pow1 = pow (error "pow1 <= 0")
{-# INLINE pow1 #-}

mpow :: (Monoid m) => Int -> m -> m
mpow = pow mempty mappend

pow :: a -> (a -> a -> a) -> Int -> a -> a
pow one mul s0
  | s0 <= 0 = const one
  | otherwise = go s0
  where
    go 1 s = s
    go n s = if n .&. 1 == 0 then h2 else mul s h2
      where
        !h2 = mul h h
        h = go (n `shiftR` 1) s
{-# INLINE pow #-}

pack32 :: (Int, Int) -> Int
pack32 (a, b) = (a `shiftL` 32) .|. b

unpack32 :: Int -> (Int, Int)
unpack32 a = (a `shiftR` 32, a .&. 0xffffffff)

whenM :: (Monad m) => m Bool -> m () -> m ()
whenM cond act = do
  c <- cond
  when c act

------------------------------------------------------------------------------
-- 2D vectors

replicate2 :: (G.Vector v a) => Int -> Int -> a -> V.Vector (v a)
replicate2 m n x = V.replicate m inner
  where
    !inner = G.replicate n x

generate2 :: (G.Vector v a) => Int -> Int -> (Int -> Int -> a) -> V.Vector (v a)
generate2 m n f = runST $
  V.generateM m $ \i ->
    return $! G.generate n $ \j -> f i j

------------------------------------------------------------------------------
-- Unpacking tuples

data Int2Tuple = Int2Tuple {-# UNPACK #-} !Int {-# UNPACK #-} !Int

i2t_fst :: Int2Tuple -> Int
i2t_fst (Int2Tuple a _) = a

i2t_snd :: Int2Tuple -> Int
i2t_snd (Int2Tuple _ a) = a

data Int3Tuple = Int3Tuple {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int

i3t_fst :: Int3Tuple -> Int
i3t_fst (Int3Tuple a _ _) = a

i3t_snd :: Int3Tuple -> Int
i3t_snd (Int3Tuple _ a _) = a

i3t_trd :: Int3Tuple -> Int
i3t_trd (Int3Tuple _ _ a) = a

i3tToTuple :: Int3Tuple -> (Int, Int, Int)
i3tToTuple (Int3Tuple a b c) = (a, b, c)

------------------------------------------------------------------------------
-- Unpacking ST

newtype STInt s = STInt (State# s -> (# State# s, Int# #))

toSTInt :: ST s Int -> STInt s
toSTInt (ST f) = STInt $ \s -> let
  !(# s1, I# x #) = f s
  in (# s1, x #)
{-# INLINE toSTInt #-}

fromSTInt :: STInt s -> ST s Int
fromSTInt (STInt f) = ST $ \s -> let
  !(# s1, x #) = f s
  in (# s1, I# x #)
{-# INLINE fromSTInt #-}

------------------------------------------------------------------------------
-- Throwing an answer

catchAnswer :: Typeable a => IO a -> IO a
catchAnswer = Ex.handle $ \(AnswerException val) ->  case cast val of
  Just val' -> return val'
  Nothing -> fail $ "catchAnswer: unexpected answer type: "
      ++ show (typeOf val)

throwAnswer :: Typeable a => a -> IO b
throwAnswer = Ex.throwIO . AnswerException

data AnswerException = forall a. Typeable a => AnswerException a
  deriving (Typeable)

instance Show AnswerException where
  show _ = "<AnswerException>"

instance Ex.Exception AnswerException

------------------------------------------------------------------------------
-- Bit scan forward

-- | Returns the position of the lowest 1 in @w@. If @w@ is 0, returns @0@.
bitScanForward :: Word -> Int
bitScanForward w = bsfTable U.! bsfHash (w .&. (-w))

bsfTable :: U.Vector Int
bsfTable = U.create $ do
  mv <- UM.new 64
  U.forM_ (U.generate 64 id) $ \i ->
    UM.write mv (bsfHash $ 1 `shiftL` i) i
  return mv

bsfHash :: Word -> Int
bsfHash w = fromIntegral $ (w * 0x03f79d71b4cb0a89) `shiftR` 58

------------------------------------------------------------------------------
-- Bit scan reverse

-- | Returns the position of the highest 1 in @w@. If @w@ is 0, returns @0@.
bitScanReverse :: Word -> Int
bitScanReverse w0 = snd $ f 1 $ f 2 $ f 4 $ f 8 $ f 16 $ f 32 (w0, 0)
  where
    f k (!w, !acc)
      | w .&. mask /= 0 = (w `shiftR` k, acc + k)
      | otherwise = (w, acc)
      where
        mask = complement 0 `shiftL` k
    {-# INLINE f #-}

log2Ceil :: Word -> Int
log2Ceil 0 = -1
log2Ceil 1 = 0
log2Ceil w = bitScanReverse (w - 1) + 1

_prop_bitScanReverse :: Word -> Bool
_prop_bitScanReverse w = bitScanReverse w == naive w
  where
    naive x = maybe 0 (63-) $ findIndex id $ map (testBit x) [63, 62..0]

------------------------------------------------------------------------------
-- integer rounding

roundDown :: Int -> Int -> Int
roundDown m n = m * div n m

roundUp :: Int -> Int -> Int
roundUp m n = m * cdiv n m

infixl 7 `cdiv`

cdiv :: Int -> Int -> Int
cdiv a b = div (a + b - 1) b

------------------------------------------------------------------------------
-- containers-compat

is_lookupLE :: Int -> IS.IntSet -> Maybe Int
is_lookupLE = setlike_lookupLE IS.splitMember IS.maxView

is_lookupGE :: Int -> IS.IntSet -> Maybe Int
is_lookupGE = setlike_lookupGE IS.splitMember IS.minView

is_lookupLT :: Int -> IS.IntSet -> Maybe Int
is_lookupLT = setlike_lookupLT IS.split IS.maxView

is_lookupGT :: Int -> IS.IntSet -> Maybe Int
is_lookupGT = setlike_lookupGT IS.split IS.minView

s_lookupLE :: (Ord a) => a -> S.Set a -> Maybe a
s_lookupLE = setlike_lookupLE S.splitMember S.maxView

s_lookupGE :: (Ord a) => a -> S.Set a -> Maybe a
s_lookupGE = setlike_lookupGE S.splitMember S.minView

s_lookupLT :: (Ord a) => a -> S.Set a -> Maybe a
s_lookupLT = setlike_lookupLT S.split S.maxView

s_lookupGT :: (Ord a) => a -> S.Set a -> Maybe a
s_lookupGT = setlike_lookupGT S.split S.minView

im_lookupLE :: Int -> IM.IntMap a -> Maybe (Int, a)
im_lookupLE = maplike_lookupLE IM.splitLookup IM.maxViewWithKey

im_lookupGE :: Int -> IM.IntMap a -> Maybe (Int, a)
im_lookupGE = maplike_lookupGE IM.splitLookup IM.minViewWithKey

im_lookupLT :: Int -> IM.IntMap a -> Maybe (Int, a)
im_lookupLT = maplike_lookupLT IM.split IM.maxViewWithKey

im_lookupGT :: Int -> IM.IntMap a -> Maybe (Int, a)
im_lookupGT = maplike_lookupGT IM.split IM.minViewWithKey

m_lookupLE :: (Ord k) => k -> M.Map k a -> Maybe (k, a)
m_lookupLE = maplike_lookupLE M.splitLookup M.maxViewWithKey

m_lookupGE :: (Ord k) => k -> M.Map k a -> Maybe (k, a)
m_lookupGE = maplike_lookupGE M.splitLookup M.minViewWithKey

m_lookupLT :: (Ord k) => k -> M.Map k a -> Maybe (k, a)
m_lookupLT = maplike_lookupLT M.split M.maxViewWithKey

m_lookupGT :: (Ord k) => k -> M.Map k a -> Maybe (k, a)
m_lookupGT = maplike_lookupGT M.split M.minViewWithKey

setlike_lookupLE
  :: (a -> set -> (set, Bool, set))
  -> (set -> Maybe (a, set))
  -> a
  -> set
  -> Maybe a
setlike_lookupLE splitMember maxView k s = case splitMember k s of
  (_, True, _) -> Just k
  (lo, _, _) -> fst <$> maxView lo
{-# INLINE setlike_lookupLE #-}

setlike_lookupGE
  :: (a -> set -> (set, Bool, set))
  -> (set -> Maybe (a, set))
  -> a
  -> set
  -> Maybe a
setlike_lookupGE splitMember minView k s = case splitMember k s of
  (_, True, _) -> Just k
  (_, _, hi) -> fst <$> minView hi
{-# INLINE setlike_lookupGE #-}

setlike_lookupLT
  :: (a -> set -> (set, set))
  -> (set -> Maybe (a, set))
  -> a
  -> set
  -> Maybe a
setlike_lookupLT split maxView k s = fmap fst $ maxView $ fst $ split k s
{-# INLINE setlike_lookupLT #-}

setlike_lookupGT
  :: (a -> set -> (set, set))
  -> (set -> Maybe (a, set))
  -> a
  -> set
  -> Maybe a
setlike_lookupGT split minView k s = fmap fst $ minView $ snd $ split k s
{-# INLINE setlike_lookupGT #-}

maplike_lookupLE
  :: (k -> map -> (map, Maybe a, map))
  -> (map -> Maybe ((k, a), map))
  -> k
  -> map
  -> Maybe (k, a)
maplike_lookupLE splitLookup maxViewWithKey k s = case splitLookup k s of
  (_, Just v, _) -> Just (k, v)
  (lo, _, _) -> fst <$> maxViewWithKey lo
{-# INLINE maplike_lookupLE #-}

maplike_lookupGE
  :: (k -> map -> (map, Maybe a, map))
  -> (map -> Maybe ((k, a), map))
  -> k
  -> map
  -> Maybe (k, a)
maplike_lookupGE splitLookup minViewWithKey k s = case splitLookup k s of
  (_, Just v, _) -> Just (k, v)
  (_, _, hi) -> fst <$> minViewWithKey hi
{-# INLINE maplike_lookupGE #-}

maplike_lookupLT
  :: (k -> map -> (map, map))
  -> (map -> Maybe ((k, a), map))
  -> k
  -> map
  -> Maybe (k, a)
maplike_lookupLT split maxViewWithKey k s
  = fmap fst $ maxViewWithKey $ fst $ split k s
{-# INLINE maplike_lookupLT #-}

maplike_lookupGT
  :: (k -> map -> (map, map))
  -> (map -> Maybe ((k, a), map))
  -> k
  -> map
  -> Maybe (k, a)
maplike_lookupGT split minViewWithKey k s
  = fmap fst $ minViewWithKey $ snd $ split k s
{-# INLINE maplike_lookupGT #-}
