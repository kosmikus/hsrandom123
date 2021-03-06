{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns, TypeFamilies, EmptyDataDecls, MultiParamTypeClasses, MagicHash, UnboxedTuples, FlexibleContexts, ScopedTypeVariables, FlexibleInstances #-}

#if MIN_VERSION_base(4,6,0)

{-# LANGUAGE DataKinds #-}

#endif

#include "MachDeps.h"

module System.Random.Philox (
  module System.Random.Philox, module System.Random.HsRandom123.Utils
  ) where

import Control.Monad.Primitive
import Control.Monad.ST
import Data.Bits
import Data.Int
import Data.Primitive.MutVar
import Data.Word
import GHC.Exts
import GHC.Word
import Prelude hiding (init)
import System.Random.HsRandom123.Utils

class PhiloxW w where
  mulhilo :: W w -> W w -> A N2 w

#if WORD_SIZE_IN_BITS < 64
#if MIN_VERSION_base(4,6,0)

instance PhiloxW W32 where
  mulhilo (W32# x) (W32# y) =
    case timesWord2# x y of
      (# lo, hi #) -> A2x32 (W32# lo) (W32# hi)

#else

lomask32 :: Word32
lomask32 = 0x00000000FFFFFFFF

instance PhiloxW W32 where
  mulhilo a b =
    let !lo  = a * b
        !ahi = shiftR a 16
        !bhi = shiftR b 16
        !alo = a .&. lomask32
        !blo = b .&. lomask32

        !ahbl = ahi * blo
        !albh = alo * bhi

        !ahbl_albh = (ahbl .&. lomask32) + (albh .&. lomask32)
        !hi = ahi * bhi + shiftR ahbl 16 + shiftR albh 16 + shiftR ahbl_albh 16 +
              (if shiftL lo 16 < ahbl_albh .&. lomask32 then 1 else 0)
     in A2x32 lo hi

#endif
#else

instance PhiloxW W32 where
  mulhilo x y = split32 (fromIntegral x * fromIntegral y)
  {-# INLINE mulhilo #-}

split32 :: Word64 -> A N2 W32
split32 x = A2x32 (fromIntegral x) (fromIntegral (shiftR x 32))
{-# INLINE split32 #-}

#endif

#if MIN_VERSION_base(4,6,0) && WORD_SIZE_IN_BITS == 64

instance PhiloxW W64 where
  mulhilo (W64# x) (W64# y) =
    case timesWord2# x y of
      (# lo, hi #) -> A2x64 (W64# lo) (W64# hi)

#else

lomask64 :: Word64
lomask64 = 0x00000000FFFFFFFF

instance PhiloxW W64 where
  mulhilo a b =
    let !lo  = a * b
        !ahi = shiftR a 32
        !bhi = shiftR b 32
        !alo = a .&. lomask64
        !blo = b .&. lomask64

        !ahbl = ahi * blo
        !albh = alo * bhi

        !ahbl_albh = (ahbl .&. lomask64) + (albh .&. lomask64)
        !hi = ahi * bhi + shiftR ahbl 32 + shiftR albh 32 + shiftR ahbl_albh 32 +
              (if shiftL lo 32 < ahbl_albh .&. lomask64 then 1 else 0)
     in A2x64 lo hi

#endif

type PhiloxCtr n w = A n w
type PhiloxKey n w = A (Hlv n) w

class (PhiloxW w, PhiloxN n, WithA n w, WithA (Hlv n) w, ToDoubles n w, ToWord32s n w, Dbl (Hlv n) ~ n) => Philox n w where
  multipliers :: A (Hlv n) w
  bumpers     :: A (Hlv n) w
  philoxRound :: PhiloxCtr n w -> PhiloxKey n w -> PhiloxCtr n w
  bumpKey     :: PhiloxKey n w -> PhiloxKey n w
  stream      :: PhiloxCtr n w -> PhiloxKey n w -> [W w]
  ustream     :: PhiloxCtr n w -> PhiloxKey n w -> [Double]

instance Philox N2 W64 where
  multipliers = A1x64 0xD2B74407B1CE6E93
  {-# INLINE multipliers #-}
  bumpers     = A1x64 0x9E3779B97F4A7C15
  {-# INLINE bumpers #-}
  philoxRound = pR
  {-# INLINE philoxRound #-}
  bumpKey     = bk
  {-# INLINE bumpKey #-}
  stream ctr key = go (init ctr key :: PhiloxData N2 W64)
    where
      go pd = case step pd of
                (r, npd) -> withA r $ \ a0 a1 -> a0 : a1 : go npd
  {-# INLINE stream #-}
  ustream ctr key = go (init ctr key :: PhiloxData N2 W64)
    where
      go pd = case step pd of
                (r, npd) -> withA r $ \ a0 a1 -> wordToDouble a0 : wordToDouble a1 : go npd
  {-# INLINE ustream #-}

instance Philox N4 W64 where
  multipliers = A2x64 0xD2E7470EE14C6C93 0xCA5A826395121157
  {-# INLINE multipliers #-}
  bumpers     = A2x64 0x9E3779B97F4A7C15 0xBB67AE8584CAA73B
  {-# INLINE bumpers #-}
  philoxRound = pR
  {-# INLINE philoxRound #-}
  bumpKey     = bk
  {-# INLINE bumpKey #-}
  stream ctr key = go (init ctr key :: PhiloxData N4 W64)
    where
      go pd = case step pd of
                (r, npd) -> withA r $ \ a0 a1 a2 a3 -> a0 : a1 : a2 : a3 : go npd
  {-# INLINE stream #-}
  ustream ctr key = go (init ctr key :: PhiloxData N4 W64)
    where
      go pd = case step pd of
                (r, npd) -> withA r $ \ a0 a1 a2 a3 -> wordToDouble a0 : wordToDouble a1 : wordToDouble a2 : wordToDouble a3 : go npd
  {-# INLINE ustream #-}

instance Philox N2 W32 where
  multipliers = A1x32 0xD256D193
  {-# INLINE multipliers #-}
  bumpers     = A1x32 0x9E3779B9
  {-# INLINE bumpers #-}
  philoxRound = pR
  {-# INLINE philoxRound #-}
  bumpKey     = bk
  {-# INLINE bumpKey #-}
  stream ctr key = go (init ctr key :: PhiloxData N2 W32)
    where
      go pd = case step pd of
                (r, npd) -> withA r $ \ a0 a1 -> a0 : a1 : go npd
  {-# INLINE stream #-}
  ustream ctr key = go (init ctr key :: PhiloxData N2 W32)
    where
      go pd = case step pd of
                (r, npd) -> withA r $ \ a0 a1 -> wordsToDouble a0 a1 : go npd
  {-# INLINE ustream #-}

instance Philox N4 W32 where
  multipliers = A2x32 0xD2511F53 0xCD9E8D57
  {-# INLINE multipliers #-}
  bumpers     = A2x32 0x9E3779B9 0xBB67AE85
  {-# INLINE bumpers #-}
  philoxRound = pR
  {-# INLINE philoxRound #-}
  bumpKey     = bk
  {-# INLINE bumpKey #-}
  stream ctr key = go (init ctr key :: PhiloxData N4 W32)
    where
      go pd = case step pd of
                (r, npd) -> withA r $ \ a0 a1 a2 a3 -> a0 : a1 : a2 : a3 : go npd
  {-# INLINE stream #-}
  ustream ctr key = go (init ctr key :: PhiloxData N4 W32)
    where
      go pd = case step pd of
                (r, npd) -> withA r $ \ a0 a1 a2 a3 -> wordsToDouble a0 a1 : wordsToDouble a2 a3 : go npd
  {-# INLINE ustream #-}

class Counter n => PhiloxN n where
  bk   :: forall w . (Philox n w) => A (Hlv n) w -> A (Hlv n) w
  pR   :: forall w . (Philox n w) => A n w -> A (Hlv n) w -> A n w

instance PhiloxN N2 where
  bk (key :: A N1 w) =
    withA key $ \ key0 ->
    withA (bumpers :: A N1 w) $ \ w0 ->
    mkA (key0 + w0)
  {-# INLINE bk #-}
  pR (ctr :: A N2 w) key =
    withA ctr $ \ ctr0 ctr1 ->
    withA key $ \ key0 ->
    withA (multipliers :: A N1 w) $ \ m0 ->
    withA (mulhilo m0 ctr0 :: A N2 w) $ \ lo0 hi0 ->
    mkA (hi0 `xor` ctr1 `xor` key0) lo0
  {-# INLINE pR #-}

instance PhiloxN N4 where
  bk (key :: A N2 w) =
    withA key $ \ key0 key1 ->
    withA (bumpers :: A N2 w) $ \ w0 w1 ->
    mkA (key0 + w0) (key1 + w1)
  {-# INLINE bk #-}
  pR (ctr :: A N4 w) key =
    withA ctr $ \ ctr0 ctr1 ctr2 ctr3 ->
    withA key $ \ key0 key1 ->
    withA (multipliers :: A N2 w) $ \ m0 m1 ->
    withA (mulhilo m0 ctr0 :: A N2 w) $ \ lo0 hi0 ->
    withA (mulhilo m1 ctr2 :: A N2 w) $ \ lo1 hi1 ->
    mkA (hi1 `xor` ctr1 `xor` key0) lo1
                              (hi0 `xor` ctr3 `xor` key1) lo0
  {-# INLINE pR #-}

-- | The state of a Philox RNG.
data PhiloxData n w = P !(PhiloxCtr n w) !(PhiloxKey n w)

-- | Initialize a Philox RNG with a counter and a key.
init :: PhiloxCtr n w -> PhiloxKey n w -> PhiloxData n w
init = P
{-# INLINE init #-}

-- | Increment the counter.
inc :: (Philox n w) => PhiloxData n w -> PhiloxData n w
inc (P ctr key) = P (incr ctr) key
{-# INLINE inc #-}

class (Philox n w) => Cycle r n w where
  rounds :: Rounds r -> PhiloxData n w -> PhiloxData n w

instance Philox n w => Cycle Z n w where
  rounds _ p = p
  {-# INLINE rounds #-}

instance (Philox n w, Cycle r n w) => Cycle (S r) n w where
  rounds _ p = philoxCycle (rounds (Rounds :: Rounds r) p)
  {-# INLINE rounds #-}

rounds1 :: forall r n w . (Philox n w, Cycle r n w) => Rounds (S r) -> PhiloxData n w -> PhiloxCtr n w
rounds1 _ pd = case rounds (Rounds :: Rounds r) pd of
                 P ctr key -> philoxRound ctr key
{-# INLINE rounds1 #-}

philoxCycle :: (Philox n w) => PhiloxData n w -> PhiloxData n w
philoxCycle (P ctr key) = P (philoxRound ctr key) (bumpKey key)
{-# INLINE philoxCycle #-}

-- | Get the current random data.
get :: (Philox n w) => PhiloxData n w -> A n w
get pd = rounds1 (Rounds :: Rounds R10) pd
{-# INLINE get #-}

-- | Get the current random data and increment the counter.
step :: (Philox n w) => PhiloxData n w -> (A n w, PhiloxData n w)
step rng = (get rng, inc rng)
{-# INLINE step #-}

{-
data PhiloxState s n w = PS {-# UNPACK #-} !(MutVar s (A n w))
                            {-# UNPACK #-} !(MutVar s Int)
                            {-# UNPACK #-} !(MutVar s (PhiloxData n w))
-}

data PhiloxState s n w = PS {-# UNPACK #-} !(MutVar s DoubleList) {-# UNPACK #-} !(MutVar s (PhiloxData n w))

type PhiloxStateIO   = PhiloxState (PrimState IO)
type PhiloxStateST s = PhiloxState (PrimState (ST s))

new :: (PrimMonad m, Philox n w) => A n w -> A (Hlv n) w -> m (PhiloxState (PrimState m) n w)
new ctr key = do
  svd <- newMutVar DoubleNil
  ph  <- newMutVar (init ctr key)
  return (PS svd ph)

uniformDouble :: (PrimMonad m, Philox n w) => PhiloxState (PrimState m) n w -> m Double
uniformDouble (PS svd ph) = do
  ds <- readMutVar svd
  case ds of
    (DoubleCons x xs) -> do
                writeMutVar svd xs
                return x
    DoubleNil         -> do
                rng <- readMutVar ph
                toDoublesL (get rng) $ \ r rs -> do
                    writeMutVar ph $! inc rng
                    writeMutVar svd rs
                    return r
{-# INLINE uniformDouble #-}

data PhiloxStateWord32 s n w = PSW {-# UNPACK #-} !(MutVar s Word32List) {-# UNPACK #-} !(MutVar s (PhiloxData n w))

type PhiloxStateWord32IO   = PhiloxStateWord32 (PrimState IO)
type PhiloxStateWord32ST s = PhiloxStateWord32 (PrimState (ST s))

newWord32 :: (PrimMonad m, Philox n w) => A n w -> A (Hlv n) w -> m (PhiloxStateWord32 (PrimState m) n w)
newWord32 ctr key = do
  svd <- newMutVar Word32Nil
  ph  <- newMutVar (init ctr key)
  return (PSW svd ph)

uniform1 :: (PrimMonad m, Philox n w) => (Word32 -> a) -> PhiloxStateWord32 (PrimState m) n w -> m a
uniform1 f (PSW svd ph) = do
  ds <- readMutVar svd
  case ds of
    (Word32Cons x xs) -> do
                writeMutVar svd xs
                return (f x)
    Word32Nil -> do
                rng <- readMutVar ph
                toWord32sL (get rng) $ \ x xs -> do
                  writeMutVar ph $! inc rng
                  writeMutVar svd xs
                  return (f x)

uniform2 :: (PrimMonad m, Philox n w) => (Word32 -> Word32 -> a) -> PhiloxStateWord32 (PrimState m) n w -> m a
uniform2 f (PSW svd ph) = do
  ds <- readMutVar svd
  case ds of
    (Word32Cons x0 (Word32Cons x1 xs)) -> do
                writeMutVar svd xs
                return (f x0 x1)
    (Word32Cons x0 Word32Nil) -> do
                rng <- readMutVar ph
                toWord32sL (get rng) $ \ x1 xs -> do
                    writeMutVar ph $! inc rng
                    writeMutVar svd xs
                    return (f x0 x1)
    Word32Nil -> do
                rng <- readMutVar ph
                toWord32sL (get rng) $ \ x0 xs ->
                  case xs of
                    (Word32Cons x1 ys) -> do
                      writeMutVar ph $! inc rng
                      writeMutVar svd ys
                      return (f x0 x1)
                    Word32Nil -> do
                      let !rng' = inc rng
                      toWord32sL (get rng') $ \ x1 ys -> do
                        writeMutVar ph $! inc rng'
                        writeMutVar svd ys
                        return (f x0 x1)
{-# INLINE uniform2 #-}

class Variate a where
  uniform :: (Philox n w, PrimMonad m) => PhiloxStateWord32 (PrimState m) n w -> m a

instance Variate Int8 where
  uniform = uniform1 fromIntegral
  {-# INLINE uniform #-}

instance Variate Int16 where
  uniform = uniform1 fromIntegral
  {-# INLINE uniform #-}

instance Variate Int32 where
  uniform = uniform1 fromIntegral
  {-# INLINE uniform #-}

instance Variate Int64 where
  uniform = uniform2 wordsTo64Bit
  {-# INLINE uniform #-}

instance Variate Word8 where
  uniform = uniform1 fromIntegral
  {-# INLINE uniform #-}

instance Variate Word16 where
  uniform = uniform1 fromIntegral
  {-# INLINE uniform #-}

instance Variate Word32 where
  uniform = uniform1 fromIntegral
  {-# INLINE uniform #-}

instance Variate Word64 where
  uniform = uniform2 wordsTo64Bit
  {-# INLINE uniform #-}

instance Variate Double where
  uniform = uniform2 wordsToDouble
  {-# INLINE uniform #-}

instance Variate Int where
#if WORD_SIZE_IN_BITS < 64
  uniform = uniform1 fromIntegral
#else
  uniform = uniform2 wordsTo64Bit
#endif
  {-# INLINE uniform  #-}

instance Variate Word where
#if WORD_SIZE_IN_BITS < 64
  uniform = uniform1 fromIntegral
#else
  uniform = uniform2 wordsTo64Bit
#endif
  {-# INLINE uniform  #-}

newtype PhiloxStateRaw s n w = PSR (MutVar s (PhiloxData n w))

type PhiloxStateRawIO   = PhiloxStateRaw (PrimState IO)
type PhiloxStateRawST s = PhiloxStateRaw (PrimState (ST s))

newRaw :: (PrimMonad m, Philox n w) => A n w -> A (Hlv n) w -> m (PhiloxStateRaw (PrimState m) n w)
newRaw ctr key = do
  ph <- newMutVar (init ctr key)
  return (PSR ph)

nextRaw :: (PrimMonad m, Philox n w) => PhiloxStateRaw (PrimState m) n w -> m (A n w)
nextRaw (PSR ph) = do
  rng <- readMutVar ph
  writeMutVar ph $! inc rng
  return (get rng)
{-# INLINE nextRaw #-}
