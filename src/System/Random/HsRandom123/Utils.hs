{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns, TypeFamilies, EmptyDataDecls, MultiParamTypeClasses, MagicHash, UnboxedTuples, FlexibleContexts, ScopedTypeVariables, FlexibleInstances #-}

#if MIN_VERSION_base(4,6,0)

{-# LANGUAGE DataKinds #-}

#endif
module System.Random.HsRandom123.Utils where

import Control.DeepSeq
import Data.Bits
import Data.Int
import Data.Word

#if MIN_VERSION_base(4,6,0)

-- | Quantities of words that appear grouped together.
data Nx = N1 | N2 | N4

-- | Division by two for 'Nx' quantities.
type family Hlv (n :: Nx) :: Nx
-- | Multiplication by two for 'Nx' quantities.
type family Dbl (n :: Nx) :: Nx

-- | Word sizes. The 'WN' constructor represents native word size for the platform.
data Wd = WN | W8 | W32 | W64

-- | Peano naturals.
data Nat = Z | S Nat

-- | Modulo 8 on peano naturals.
type family Mod8 (n :: Nat) :: Nat
-- | Division by 4 on peano naturals.
type family Div4 (n :: Nat) :: Nat

-- | The type 'A n w' represents 'n' words of width 'w'.
data family A (n :: Nx) (w :: Wd) :: *
newtype Rot n (w :: Wd) = Rot (A (Hlv n) W8)
type family Cont (n :: Nx) (w :: Wd) (r :: *) :: *

-- | The type 'W w' represents a word of width 'w'.
type family W (w :: Wd) :: *
-- | Inverse of 'W'.
type family UnW (w :: *) :: Wd

data Rounds (r :: Nat) = Rounds

#else

-- kind Nx

-- | Represents a group of 1.
data N1
-- | Represents a group of 2.
data N2
-- | Represents a group of 4.
data N4

-- | Division by two for 'N1', 'N2', 'N4', ...
type family Hlv n :: *
-- | Multiplication by two for 'N1', 'N2', 'N4', ...
type family Dbl n :: *

-- kind Wd

-- | Represents native word width.
data WN
-- | Represents a width of 8 bits.
data W8
-- | Represents a width of 32 bits.
data W32
-- | Represents a width of 64 bits.
data W64

-- kind Nat

-- | Represents zero in the Peano naturals.
data Z
-- | Represents successor in the Peano naturals.
data S n

-- | Modulo 8 on Peano naturals.
type family Mod8 n :: *
-- | Division by 4 on Peano naturals.
type family Div4 n :: *

-- | The datatype 'A n w' represents 'n' words of width 'w'.
data family A n w :: *
newtype Rot n w = Rot (A (Hlv n) W8)
type family Cont n w r :: *

-- | The datatype 'W w' represents a word of width 'w'.
type family W w :: *
-- | Inverse of 'W'.
type family UnW w :: *

data Rounds r = Rounds

#endif

type family UnCont (t :: *) :: *

type instance Hlv N2 = N1
type instance Hlv N4 = N2

type instance Dbl N1 = N2
type instance Dbl N2 = N4

newtype instance A N1 WN  = A1x   Word
newtype instance A N1 W8  = A1x8  Word8
newtype instance A N1 W32 = A1x32 Word32
newtype instance A N1 W64 = A1x64 Word64
data instance A N2 WN  = A2x   {-# UNPACK #-} !Word   {-# UNPACK #-} !Word
data instance A N2 W8  = A2x8  {-# UNPACK #-} !Word8  {-# UNPACK #-} !Word8
data instance A N2 W32 = A2x32 {-# UNPACK #-} !Word32 {-# UNPACK #-} !Word32
data instance A N2 W64 = A2x64 {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64
data instance A N4 WN  = A4x   {-# UNPACK #-} !Word   {-# UNPACK #-} !Word   {-# UNPACK #-} !Word   {-# UNPACK #-} !Word
data instance A N4 W8  = A4x8  {-# UNPACK #-} !Word8  {-# UNPACK #-} !Word8  {-# UNPACK #-} !Word8  {-# UNPACK #-} !Word8
data instance A N4 W32 = A4x32 {-# UNPACK #-} !Word32 {-# UNPACK #-} !Word32 {-# UNPACK #-} !Word32 {-# UNPACK #-} !Word32
data instance A N4 W64 = A4x64 {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64

instance NFData (A N1 WN)
instance NFData (A N1 W8)
instance NFData (A N1 W32)
instance NFData (A N1 W64)
instance NFData (A N2 WN)
instance NFData (A N2 W8)
instance NFData (A N2 W32)
instance NFData (A N2 W64)
instance NFData (A N4 WN)
instance NFData (A N4 W8)
instance NFData (A N4 W32)
instance NFData (A N4 W64)

type instance Cont N1 w r = W w -> r
type instance Cont N2 w r = W w -> W w -> r
type instance Cont N4 w r = W w -> W w -> W w -> W w -> r

type instance UnCont (A n w)  = A n w
type instance UnCont (a -> r) = UnCont r

class (Bits (W w), Num (W w), Eq (W w)) => WithA n w where
  withA :: A n w -> Cont n w r -> r
  mkA   :: (UnCont r ~ A n w, Cont n w (A n w) ~ r) => r

instance WithA N1 WN where
  withA (A1x x0) k = k x0
  {-# INLINE withA #-}
  mkA              = A1x
  {-# INLINE mkA #-}

instance WithA N1 W8  where
  withA (A1x8 x0) k = k x0
  {-# INLINE withA #-}
  mkA               = A1x8
  {-# INLINE mkA #-}

instance WithA N1 W32 where
  withA (A1x32 x0) k = k x0
  {-# INLINE withA #-}
  mkA                = A1x32
  {-# INLINE mkA #-}

instance WithA N1 W64 where
  withA (A1x64 x0) k = k x0
  {-# INLINE withA #-}
  mkA                = A1x64
  {-# INLINE mkA #-}

instance WithA N2 WN where
  withA (A2x x0 x1) k = k x0 x1
  {-# INLINE withA #-}
  mkA                 = A2x
  {-# INLINE mkA #-}

instance WithA N2 W8 where
  withA (A2x8 x0 x1) k = k x0 x1
  {-# INLINE withA #-}
  mkA                  = A2x8
  {-# INLINE mkA #-}

instance WithA N2 W32 where
  withA (A2x32 x0 x1) k = k x0 x1
  {-# INLINE withA #-}
  mkA                   = A2x32
  {-# INLINE mkA #-}

instance WithA N2 W64 where
  withA (A2x64 x0 x1) k = k x0 x1
  {-# INLINE withA #-}
  mkA                   = A2x64
  {-# INLINE mkA #-}

instance WithA N4 WN where
  withA (A4x x0 x1 x2 x3) k = k x0 x1 x2 x3
  {-# INLINE withA #-}
  mkA                       = A4x
  {-# INLINE mkA #-}

instance WithA N4 W8 where
  withA (A4x8 x0 x1 x2 x3) k = k x0 x1 x2 x3
  {-# INLINE withA #-}
  mkA                        = A4x8
  {-# INLINE mkA #-}

instance WithA N4 W32 where
  withA (A4x32 x0 x1 x2 x3) k = k x0 x1 x2 x3
  {-# INLINE withA #-}
  mkA                         = A4x32
  {-# INLINE mkA #-}

instance WithA N4 W64 where
  withA (A4x64 x0 x1 x2 x3) k = k x0 x1 x2 x3
  {-# INLINE withA #-}
  mkA                         = A4x64
  {-# INLINE mkA #-}

withRot :: (WithA (Hlv n) W8) => Rot n w -> Cont (Hlv n) W8 r -> r
withRot (Rot a) = withA a

data DoubleList =
    DoubleNil
  | DoubleCons {-# UNPACK #-} !Double DoubleList

data Word32List =
    Word32Nil
  | Word32Cons {-# UNPACK #-} !Word32 Word32List

class ToDoubles n w where
  toDoubles  :: A n w -> (Double -> [Double] -> r) -> r
  toDoublesL :: A n w -> (Double -> DoubleList -> r) -> r

class ToWord32s n w where
  toWord32s  :: A n w -> (Word32 -> [Word32] -> r) -> r
  toWord32sL :: A n w -> (Word32 -> Word32List -> r) -> r

instance ToDoubles N2 W64 where
  toDoubles (A2x64 w0 w1) k = k d0 [d1]
    where
      !d0 = wordToDouble w0
      !d1 = wordToDouble w1 
  {-# INLINE toDoubles #-}
  toDoublesL (A2x64 w0 w1) k = k d0 (DoubleCons d1 DoubleNil)
    where
      !d0 = wordToDouble w0
      !d1 = wordToDouble w1 
  {-# INLINE toDoublesL #-}

instance ToWord32s N2 W64 where
  toWord32s (A2x64 lw0 lw1) k = k w0 [w1, w2, w3]
    where
      !(# w0, w1 #) = word64ToWords32 lw0
      !(# w2, w3 #) = word64ToWords32 lw1
  {-# INLINE toWord32s #-}
  toWord32sL (A2x64 lw0 lw1) k = k w0 (Word32Cons w1 (Word32Cons w2 (Word32Cons w3 Word32Nil)))
    where
      !(# w0, w1 #) = word64ToWords32 lw0
      !(# w2, w3 #) = word64ToWords32 lw1
  {-# INLINE toWord32sL #-}

instance ToDoubles N4 W64 where
  toDoubles (A4x64 w0 w1 w2 w3) k = k d0 [d1, d2, d3]
    where
      !d0 = wordToDouble w0
      !d1 = wordToDouble w1
      !d2 = wordToDouble w2
      !d3 = wordToDouble w3
  {-# INLINE toDoubles #-}
  toDoublesL (A4x64 w0 w1 w2 w3) k = k d0 (DoubleCons d1 (DoubleCons d2 (DoubleCons d3 DoubleNil)))
    where
      !d0 = wordToDouble w0
      !d1 = wordToDouble w1
      !d2 = wordToDouble w2
      !d3 = wordToDouble w3
  {-# INLINE toDoublesL #-}

instance ToWord32s N4 W64 where
  toWord32s (A4x64 lw0 lw1 lw2 lw3) k = k w0 [w1, w2, w3, w4, w5, w6, w7]
    where
      !(# w0, w1 #) = word64ToWords32 lw0
      !(# w2, w3 #) = word64ToWords32 lw1
      !(# w4, w5 #) = word64ToWords32 lw2
      !(# w6, w7 #) = word64ToWords32 lw3
  {-# INLINE toWord32s #-}
  toWord32sL (A4x64 lw0 lw1 lw2 lw3) k = k w0 (Word32Cons w1 (Word32Cons w2 (Word32Cons w3 (Word32Cons w4 (Word32Cons w5 (Word32Cons w6 (Word32Cons w7 Word32Nil)))))))
    where
      !(# w0, w1 #) = word64ToWords32 lw0
      !(# w2, w3 #) = word64ToWords32 lw1
      !(# w4, w5 #) = word64ToWords32 lw2
      !(# w6, w7 #) = word64ToWords32 lw3
  {-# INLINE toWord32sL #-}

instance ToDoubles N2 W32 where
  toDoubles (A2x32 w0 w1) k = (k $! wordsToDouble w0 w1) []
  {-# INLINE toDoubles #-}
  toDoublesL (A2x32 w0 w1) k = (k $! wordsToDouble w0 w1) DoubleNil
  {-# INLINE toDoublesL #-}

instance ToWord32s N2 W32 where
  toWord32s (A2x32 w0 w1) k = k w0 [w1]
  {-# INLINE toWord32s #-}
  toWord32sL (A2x32 w0 w1) k = k w0 (Word32Cons w1 Word32Nil)
  {-# INLINE toWord32sL #-}

instance ToDoubles N4 W32 where
  toDoubles (A4x32 w0 w1 w2 w3) k = k d0 [d1]
    where
      !d0 = wordsToDouble w0 w1
      !d1 = wordsToDouble w2 w3
  {-# INLINE toDoubles #-}
  toDoublesL (A4x32 w0 w1 w2 w3) k = k d0 (DoubleCons d1 DoubleNil)
    where
      !d0 = wordsToDouble w0 w1
      !d1 = wordsToDouble w2 w3
  {-# INLINE toDoublesL #-}

instance ToWord32s N4 W32 where
  toWord32s (A4x32 w0 w1 w2 w3) k = k w0 [w1, w2, w3]
  {-# INLINE toWord32s #-}
  toWord32sL (A4x32 w0 w1 w2 w3) k = k w0 (Word32Cons w1 (Word32Cons w2 (Word32Cons w3 Word32Nil)))
  {-# INLINE toWord32sL #-}

class Counter n where
  zero :: forall w . (WithA n w) => A n w
  incr :: forall w . (WithA n w) => A n w -> A n w

instance Counter N1 where
  zero = mkA 0
  {-# INLINE zero #-}
  incr (c :: A N1 w) =
    withA c $ \ c0 -> mkA (c0 + 1)
  {-# INLINE incr #-}

instance Counter N2 where
  zero = mkA 0 0
  {-# INLINE zero #-}
  incr (c :: A N2 w) =
    withA c $ \ c0 c1 ->
    case c1 + 1 of
      0 -> mkA (c0 + 1) 0
      n -> mkA c0 n
  {-# INLINE incr #-}

instance Counter N4 where
  zero = mkA 0 0 0 0
  {-# INLINE zero #-}
  incr (c :: A N4 w) =
    withA c $ \ c0 c1 c2 c3 ->
    case c3 + 1 of
      0 -> case c2 + 1 of
             0 -> case c1 + 1 of
                    0 -> mkA (c0 + 1) 0 0 0
                    n -> mkA c0 n 0 0
             n -> mkA c0 c1 n 0
      n -> mkA c0 c1 c2 n
  {-# INLINE incr #-}

type instance W WN  = Word
type instance W W8  = Word8
type instance W W32 = Word32
type instance W W64 = Word64

type instance UnW Word   = WN
type instance UnW Word8  = W8
type instance UnW Word32 = W32
type instance UnW Word64 = W64

-- type-level integer literals
type R0 = Z
type R1 = S Z
type R2 = S R1
type R3 = S R2
type R4 = S R3
type R5 = S R4
type R6 = S R5
type R7 = S R6
type R8 = S R7
type R9 = S R8
type R10 = S R9
type R11 = S R10
type R12 = S R11
type R13 = S R12
type R14 = S R13
type R15 = S R14
type R16 = S R15
type R17 = S R16
type R18 = S R17
type R19 = S R18
type R20 = S R19
type R21 = S R20
type R22 = S R21
type R23 = S R22
type R24 = S R23
type R25 = S R24
type R26 = S R25
type R27 = S R26
type R28 = S R27
type R29 = S R28
type R30 = S R29
type R31 = S R30
type R32 = S R31
type S4 n = S (S (S (S n)))
type S8 n = S4 (S4 n)

type instance Mod8 R0 = R0
type instance Mod8 R1 = R1
type instance Mod8 R2 = R2
type instance Mod8 R3 = R3
type instance Mod8 R4 = R4
type instance Mod8 R5 = R5
type instance Mod8 R6 = R6
type instance Mod8 R7 = R7
type instance Mod8 (S8 n) = Mod8 n

type instance Div4 R0 = R0
type instance Div4 R1 = R0
type instance Div4 R2 = R0
type instance Div4 R3 = R0
type instance Div4 (S4 n) = S (Div4 n)

streamToDouble32 :: [Word32] -> [Double]
streamToDouble32 (w1 : w2 : ws) = wordsToDouble w1 w2 : streamToDouble32 ws

streamToDouble64 :: [Word64] -> [Double]
streamToDouble64 (w : ws) = wordToDouble w : streamToDouble64 ws

word64ToWords32 :: Word64 -> (# Word32, Word32 #)
word64ToWords32 lw = (# fromIntegral (lw `shiftR` 32), fromIntegral lw #)
{-# INLINE word64ToWords32 #-}

-- from bos
wordsToDouble :: Word32 -> Word32 -> Double
wordsToDouble x y  = (fromIntegral u * m_inv_32 + (0.5 + m_inv_53) +
                     fromIntegral (v .&. 0xFFFFF) * m_inv_52)
    where m_inv_52 = 2.220446049250313080847263336181640625e-16
          m_inv_53 = 1.1102230246251565404236316680908203125e-16
          m_inv_32 = 2.3283064365386962890625e-10
          u        = fromIntegral x :: Int32
          v        = fromIntegral y :: Int32
{-# INLINE wordsToDouble #-}

wordToDouble :: Word64 -> Double
wordToDouble w = wordsToDouble (fromIntegral w) (fromIntegral (shiftR w 32))
{-# INLINE wordToDouble #-}

wordsTo64Bit :: (Integral a) => Word32 -> Word32 -> a
wordsTo64Bit x y =
    fromIntegral ((fromIntegral x `shiftL` 32) .|. fromIntegral y :: Word64)
{-# INLINE wordsTo64Bit #-}

wordToBool :: Word32 -> Bool
wordToBool i = (i .&. 1) /= 0
{-# INLINE wordToBool #-}

wordToFloat :: Word32 -> Float
wordToFloat x      = (fromIntegral i * m_inv_32) + 0.5 + m_inv_33
    where m_inv_33 = 1.16415321826934814453125e-10
          m_inv_32 = 2.3283064365386962890625e-10
          i        = fromIntegral x :: Int32
{-# INLINE wordToFloat #-}

