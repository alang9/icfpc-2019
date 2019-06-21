{-# LANGUAGE BangPatterns, ViewPatterns, OverloadedStrings #-}
{-# LANGUAGE UnboxedTuples, MagicHash #-}
module MBA
  ( MBA
  , newMBA
  , readIntMBA
  , writeIntMBA
  , writeWord16MBA
  , writeWord32MBA
  , unsafeFreezeMBA
  , BA
  , indexWordBA
  , indexWord16BA
  , indexWord32BA
  ) where

import GHC.ST (ST(..))
import GHC.Exts (Int(..), MutableByteArray#, newByteArray#, readIntArray#)
import GHC.Exts (writeIntArray#, writeWord16Array#, writeWord32Array#)
import GHC.Exts (ByteArray#, unsafeFreezeByteArray#, indexWordArray#, Word(..))
import GHC.Exts (indexWord16Array#, indexWord32Array#)
import GHC.Word (Word16(..), Word32(..))

-- | Untyped byte array.
data MBA s = MBA (MutableByteArray# s)

newMBA :: Int -> ST s (MBA s)
newMBA (I# bytes) = ST $ \s -> let
  !(# s1, mba #) = newByteArray# bytes s
  in (# s1, MBA mba #)

readIntMBA :: MBA s -> Int -> ST s Int
readIntMBA (MBA mba) (I# ofs) = ST $ \s -> let
  !(# s1, val #) = readIntArray# mba ofs s
  in (# s1, I# val #)
{-# INLINE readIntMBA #-}

writeIntMBA :: MBA s -> Int -> Int -> ST s ()
writeIntMBA (MBA mba) (I# ofs) (I# val) = ST $ \s -> let
  !s1 = writeIntArray# mba ofs val s
  in (# s1, () #)
{-# INLINE writeIntMBA #-}

writeWord16MBA :: MBA s -> Int -> Word16 -> ST s ()
writeWord16MBA (MBA mba) (I# ofs) (W16# val) = ST $ \s -> let
  !s1 = writeWord16Array# mba ofs val s
  in (# s1, () #)
{-# INLINE writeWord16MBA #-}

writeWord32MBA :: MBA s -> Int -> Word32 -> ST s ()
writeWord32MBA (MBA mba) (I# ofs) (W32# val) = ST $ \s -> let
  !s1 = writeWord32Array# mba ofs val s
  in (# s1, () #)
{-# INLINE writeWord32MBA #-}

data BA = BA ByteArray#

unsafeFreezeMBA :: MBA s -> ST s BA
unsafeFreezeMBA (MBA mba) = ST $ \s -> let
  !(# s1, ba #) = unsafeFreezeByteArray# mba s
  in (# s1, BA ba #)

indexWordBA :: BA -> Int -> Word
indexWordBA (BA ba) (I# k) = W# (indexWordArray# ba k)
{-# INLINE indexWordBA #-}

indexWord16BA :: BA -> Int -> Word16
indexWord16BA (BA ba) (I# k) = W16# (indexWord16Array# ba k)
{-# INLINE indexWord16BA #-}

indexWord32BA :: BA -> Int -> Word32
indexWord32BA (BA ba) (I# k) = W32# (indexWord32Array# ba k)
{-# INLINE indexWord32BA #-}
