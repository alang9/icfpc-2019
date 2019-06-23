{-# LANGUAGE BangPatterns, ViewPatterns, OverloadedStrings #-}
{-# LANGUAGE UnboxedTuples, MagicHash #-}
{-# LANGUAGE TupleSections #-}
module MHeap
  ( MHeap
  , newMH
  , clearMH
  , insertMH
  , deleteMH
  , dumpMH
  , sizeMH
  , visualizeMH
  , heapsortKV
  , heapsort
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Data.Function
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Data.Bits
import Prelude

import Debug.Trace
import MBA
import Util

data MHeap s k a = MHeap
  { mhSize :: {-# UNPACK #-} !(MBA s)
  , mhKeys :: !(UM.MVector s k)
  , mhVals :: !(UM.MVector s a)
  } 

-- | Create an empty heap of capacity @cap@.
newMH :: (UM.Unbox k, UM.Unbox a) => Int -> ST s (MHeap s k a)
newMH cap = MHeap
  <$> do
    r <- newMBA 8
    writeIntMBA r 0 0
    return r
  <*>  UM.new cap
  <*>  UM.new cap

clearMH :: MHeap s k a -> ST s ()
clearMH MHeap{mhSize=szV} = writeIntMBA szV 0 0

insertMH :: (UM.Unbox k, Ord k, UM.Unbox a) => MHeap s k a -> k -> a -> ST s ()
insertMH MHeap{mhSize=szV, mhVals=vals, mhKeys=keys} !key !val = do
  sz <- readIntMBA szV 0
  let !sz' = sz + 1
  --trace ("insert: sz'=" ++ show sz') $ return ()
  when (sz' > UM.length vals) $ overflowErrorMH $! UM.length vals
  writeIntMBA szV 0 sz'
  loop sz
  where
    loop 0 = do
      UM.unsafeWrite keys 0 key
      UM.unsafeWrite vals 0 val
    loop pos = do
      parent <- UM.unsafeRead keys pos'
      if parent <= key
        then do
          UM.unsafeWrite keys pos key
          UM.unsafeWrite vals pos val
        else do
          UM.unsafeWrite keys pos parent
          UM.unsafeWrite vals pos =<< UM.unsafeRead vals pos'
          loop pos'
      where
        !pos' = (pos - 1) `shiftR` 1
{-# INLINE insertMH #-}
{-# SCC insertMH #-}

sizeMH :: MHeap s k a -> ST s Int
sizeMH MHeap{mhSize=mhSize} = readIntMBA mhSize 0

overflowErrorMH :: Int -> ST s ()
overflowErrorMH s = fail $ "insertMH: overflow (cap=" ++ show s ++ ")"
{-# NOINLINE overflowErrorMH #-}

deleteMH :: (UM.Unbox k, Ord k, UM.Unbox a) => MHeap s k a -> ST s (Maybe (k, a))
deleteMH MHeap{mhSize=szV, mhVals=vals, mhKeys=keys} = do
  sz <- readIntMBA szV 0
  let !sz' = sz - 1
  if sz' < 0
    then return Nothing
    else do
      outKey <- UM.unsafeRead keys 0
      outVal <- UM.unsafeRead vals 0
      writeIntMBA szV 0 sz'
      key <- UM.unsafeRead keys sz'
      val <- UM.unsafeRead vals sz'
      loop sz' 0 key val
      return $ Just (outKey, outVal)
  where
    loop !sz !pos !(forceU -> key) !(forceU -> val)
      | lch >= sz = do
        UM.unsafeWrite keys pos key
        UM.unsafeWrite vals pos val
      | otherwise = do
        lkey <- UM.unsafeRead keys lch
        (!ch, forceU -> !ckey, forceU -> !cval) <-
          if rch >= sz
            then do
              lval <- UM.unsafeRead vals lch
              return (lch, lkey, lval)
            else do
              rkey <- UM.unsafeRead keys rch
              if lkey < rkey
                then do
                  lval <- UM.unsafeRead vals lch
                  return (lch, lkey, lval)
                else do
                  rval <- UM.unsafeRead vals rch
                  return (rch, rkey, rval)
        if key < ckey
          then do
            UM.unsafeWrite keys pos key
            UM.unsafeWrite vals pos val
          else do
            UM.unsafeWrite keys pos ckey
            UM.unsafeWrite vals pos cval
            loop sz ch key val
      where
        !lch = 2 * pos + 1
        !rch = lch + 1
{-# INLINE deleteMH #-}
{-# SCC deleteMH #-}

dumpMH :: (UM.Unbox a, Show a) => String -> MHeap s k a -> ST s ()
dumpMH label mh = do
  str <- visualizeMH mh
  trace (label ++ ":\n" ++ str) $ return ()

visualizeMH :: (UM.Unbox a, Show a) => MHeap s k a -> ST s String
visualizeMH MHeap{mhSize=szV, mhVals=vals} = do
  sz <- readIntMBA szV 0
  ($ "") <$> go sz 0 0
  where
    go !sz !level !pos
      | pos >= sz = return id
      | otherwise = do
        val <- UM.unsafeRead vals pos
        left <- go sz (level + 1) (pos * 2 + 1)
        right <- go sz (level + 1) (pos * 2 + 2)
        return $ (replicate level ' '++) . shows val . ('\n':) . left . right

heapsortKV :: (UM.Unbox k, Ord k, UM.Unbox a) => U.Vector (k, a) -> U.Vector (k, a)
heapsortKV xs = U.create $ do
  mv <- UM.new $ U.length xs
  h <- newMH $ U.length xs
  U.forM_ xs $ \(k, v) -> insertMH h k v
  flip fix 0 $ \loop !i -> do
    r <- deleteMH h
    case r of
      Nothing -> return ()
      Just (k, v) -> do
        UM.write mv i (k, v)
        loop $! i + 1
  return mv

heapsort :: (UM.Unbox a, Ord a) => U.Vector a -> U.Vector a
heapsort = U.map fst . heapsortKV . U.map (,())
