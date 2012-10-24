{-# LANGUAGE CPP #-}
{-# LANGUAGE KindSignatures, MultiParamTypeClasses, FlexibleInstances, BangPatterns, FlexibleContexts, ScopedTypeVariables, TypeFamilies #-}

#if MIN_VERSION_base(4,6,0)

{-# LANGUAGE DataKinds #-}

#endif

module System.Random.Threefry (
  module System.Random.Threefry, module System.Random.HsRandom123.Utils
  ) where

import Control.Monad.Primitive
import Control.Monad.ST
import Data.Bits
import Data.Primitive.MutVar
import Data.Word
import Prelude hiding (init)
import System.Random.HsRandom123.Utils

class (UnW (W w) ~ w) => ThreefryW w where
  rotL   :: W w -> Word8 -> W w
  parity :: W w

instance ThreefryW W64 where
  rotL   = rotL64
  {-# INLINE rotL #-}
  parity = 0x1BD11BDAA9FC1A22
  {-# INLINE parity #-}

instance ThreefryW W32 where
  rotL   = rotL32
  {-# INLINE rotL #-}
  parity = 0x1BD11BDA
  {-# INLINE parity #-}

rotL64 :: Word64 -> Word8 -> Word64
rotL64 x n = shiftL x (fromIntegral (n .&. 63)) .|. shiftR x (fromIntegral ((64 - n) .&. 63))
{-# INLINE rotL64 #-}

rotL32 :: Word32 -> Word8 -> Word32
rotL32 x n = shiftL x (fromIntegral (n .&. 31)) .|. shiftR x (fromIntegral ((32 - n) .&. 31))
{-# INLINE rotL32 #-}

type ThreefryCtr  n w = A n w
type ThreefryKey  n w = A n w

class (ThreefryW w) => ThreefryRot n w where
  rot0 :: Rot n w
  rot1 :: Rot n w
  rot2 :: Rot n w
  rot3 :: Rot n w
  rot4 :: Rot n w
  rot5 :: Rot n w
  rot6 :: Rot n w
  rot7 :: Rot n w

instance ThreefryRot N4 W64 where
  rot0 = Rot (A2x8 14 16)
  {-# INLINE rot0 #-}
  rot1 = Rot (A2x8 52 57)
  {-# INLINE rot1 #-}
  rot2 = Rot (A2x8 23 40)
  {-# INLINE rot2 #-}
  rot3 = Rot (A2x8  5 37)
  {-# INLINE rot3 #-}
  rot4 = Rot (A2x8 25 33)
  {-# INLINE rot4 #-}
  rot5 = Rot (A2x8 46 12)
  {-# INLINE rot5 #-}
  rot6 = Rot (A2x8 58 22)
  {-# INLINE rot6 #-}
  rot7 = Rot (A2x8 32 32)
  {-# INLINE rot7 #-}

instance ThreefryRot N2 W64 where
  rot0 = Rot (A1x8 16)
  {-# INLINE rot0 #-}
  rot1 = Rot (A1x8 42)
  {-# INLINE rot1 #-}
  rot2 = Rot (A1x8 12)
  {-# INLINE rot2 #-}
  rot3 = Rot (A1x8 31)
  {-# INLINE rot3 #-}
  rot4 = Rot (A1x8 16)
  {-# INLINE rot4 #-}
  rot5 = Rot (A1x8 32)
  {-# INLINE rot5 #-}
  rot6 = Rot (A1x8 24)
  {-# INLINE rot6 #-}
  rot7 = Rot (A1x8 21)
  {-# INLINE rot7 #-}

instance ThreefryRot N4 W32 where
  rot0 = Rot (A2x8 10 26)
  {-# INLINE rot0 #-}
  rot1 = Rot (A2x8 11 21)
  {-# INLINE rot1 #-}
  rot2 = Rot (A2x8 13 27)
  {-# INLINE rot2 #-}
  rot3 = Rot (A2x8 23  5)
  {-# INLINE rot3 #-}
  rot4 = Rot (A2x8  6 20)
  {-# INLINE rot4 #-}
  rot5 = Rot (A2x8 17 11)
  {-# INLINE rot5 #-}
  rot6 = Rot (A2x8 25 10)
  {-# INLINE rot6 #-}
  rot7 = Rot (A2x8 18 20)
  {-# INLINE rot7 #-}

instance ThreefryRot N2 W32 where
  rot0 = Rot (A1x8 13)
  {-# INLINE rot0 #-}
  rot1 = Rot (A1x8 15)
  {-# INLINE rot1 #-}
  rot2 = Rot (A1x8 26)
  {-# INLINE rot2 #-}
  rot3 = Rot (A1x8  6)
  {-# INLINE rot3 #-}
  rot4 = Rot (A1x8 17)
  {-# INLINE rot4 #-}
  rot5 = Rot (A1x8 29)
  {-# INLINE rot5 #-}
  rot6 = Rot (A1x8 16)
  {-# INLINE rot6 #-}
  rot7 = Rot (A1x8 24)
  {-# INLINE rot7 #-}

class ThreefryN n where
  threefry :: forall r w . (ThreefryR r n w) => Rounds r -> ThreefryCtr n w -> ThreefryKey n w -> ThreefryCtr n w

instance ThreefryN N2 where
  threefry r (x :: ThreefryCtr N2 w) ks =
    withA ks $ \ ks0 ks1 ->
    threefryR r x ks (parity `xor` ks0 `xor` ks1) id
  {-# INLINE threefry #-}

instance ThreefryN N4 where
  threefry r (x :: ThreefryCtr N4 w) ks =
    withA ks $ \ ks0 ks1 ks2 ks3 ->
    threefryR r x ks (parity `xor` ks0 `xor` ks1 `xor` ks2 `xor` ks3) id
  {-# INLINE threefry #-}

class (Threefry n w) => ThreefryR r n w where
  threefryR :: Rounds r -> ThreefryCtr n w -> ThreefryKey n w -> W w ->
                          (ThreefryCtr n w -> s) -> s

instance (Threefry N2 w) => ThreefryR R0 N2 w where
  threefryR _ x ks _k2 k =
    withA x $ \ x0 x1 ->
    withA ks $ \ ks0 ks1 ->
    let !y0 = x0 + ks0
        !y1 = x1 + ks1
    in k (mkA y0 y1)
  {-# INLINE threefryR #-}

instance (Threefry N4 w) => ThreefryR R0 N4 w where
  threefryR _ x ks _k4 k =
    withA x $ \ x0 x1 x2 x3 ->
    withA ks $ \ ks0 ks1 ks2 ks3 ->
    let !y0 = x0 + ks0
        !y1 = x1 + ks1
        !y2 = x2 + ks2
        !y3 = x3 + ks3
    in k (mkA y0 y1 y2 y3)
  {-# INLINE threefryR #-}

stdRot2 :: forall w r s . (ThreefryR r N2 w) =>
           Rot N2 w -> Rounds (S r) -> ThreefryCtr N2 w -> ThreefryKey N2 w -> W w ->
                                      (ThreefryCtr N2 w -> s) -> s
stdRot2 rot _ x ks ks2 k = threefryR (Rounds :: Rounds r) x ks ks2 $ \ x' ->
  withA x' $ \ x0 x1 ->
  withRot rot $ \ r ->
    let !y0 = x0 + x1
        !y1 = rotL x1 r `xor` y0
    in k (mkA y0 y1)
{-# INLINE stdRot2 #-}

stdRot4_0 :: forall w r s . (ThreefryR r N4 w) =>
             Rot N4 w -> Rounds (S r) -> ThreefryCtr N4 w -> ThreefryKey N4 w -> W w ->
                                        (ThreefryCtr N4 w -> s) -> s
stdRot4_0 rot _ x ks ks4 k = threefryR (Rounds :: Rounds r) x ks ks4 $ \ x' ->
  withA x' $ \ x0 x1 x2 x3 ->
  withRot rot $ \ r0 r1 ->
    let !y0 = x0 + x1
        !y1 = rotL x1 r0 `xor` y0
        !y2 = x2 + x3
        !y3 = rotL x3 r1 `xor` y2
    in k (mkA y0 y1 y2 y3)
{-# INLINE stdRot4_0 #-}

stdRot4_1 :: forall w r s . (ThreefryR r N4 w) =>
             Rot N4 w -> Rounds (S r) -> ThreefryCtr N4 w -> ThreefryKey N4 w -> W w ->
                                        (ThreefryCtr N4 w -> s) -> s
stdRot4_1 rot _ x ks ks4 k = threefryR (Rounds :: Rounds r) x ks ks4 $ \ x' ->
  withA x' $ \ x0 x1 x2 x3 ->
  withRot rot $ \ r0 r1 ->
    let !y0 = x0 + x3
        !y3 = rotL x3 r0 `xor` y0
        !y2 = x2 + x1
        !y1 = rotL x1 r1 `xor` y2
    in k (mkA y0 y1 y2 y3)
{-# INLINE stdRot4_1 #-}

{-
instance (ThreefryR r N2 w, Mod8 r ~ R0) => ThreefryR (S r) N2 w where
  threefryR = stdRot2 (rot0 :: Rot N2 w)
  {-# INLINE threefryR #-}
-}

instance (ThreefryR R0 N2 w) => ThreefryR R1 N2 w where
  threefryR = stdRot2 (rot0 :: Rot N2 w)
  {-# INLINE threefryR #-}

instance (ThreefryR R1 N2 w) => ThreefryR R2 N2 w where
  threefryR = stdRot2 (rot1 :: Rot N2 w)
  {-# INLINE threefryR #-}

instance (ThreefryR R2 N2 w) => ThreefryR R3 N2 w where
  threefryR = stdRot2 (rot2 :: Rot N2 w)
  {-# INLINE threefryR #-}

instance (ThreefryR R3 N2 w) => ThreefryR R4 N2 w where
  threefryR r x ks ks2 k = stdRot2 (rot3 :: Rot N2 w) r x ks ks2 $ \ x' ->
    withA x' $ \ x0 x1 ->
    withA ks $ \ _ks0 ks1 ->
    let !z0 = x0 + ks1
        !z1 = x1 + ks2 + 1
    in k (mkA z0 z1)
  {-# INLINE threefryR #-}

instance (ThreefryR R4 N2 w) => ThreefryR R5 N2 w where
  threefryR = stdRot2 (rot4 :: Rot N2 w)
  {-# INLINE threefryR #-}

instance (ThreefryR R5 N2 w) => ThreefryR R6 N2 w where
  threefryR = stdRot2 (rot5 :: Rot N2 w)
  {-# INLINE threefryR #-}

instance (ThreefryR R6 N2 w) => ThreefryR R7 N2 w where
  threefryR = stdRot2 (rot6 :: Rot N2 w)
  {-# INLINE threefryR #-}

instance (ThreefryR R7 N2 w) => ThreefryR R8 N2 w where
  threefryR r x ks ks2 k = stdRot2 (rot7 :: Rot N2 w) r x ks ks2 $ \ x' ->
    withA x' $ \ x0 x1 ->
    withA ks $ \ ks0 _ks1 ->
    let !z0 = x0 + ks2
        !z1 = x1 + ks0 + 2
    in k (mkA z0 z1)
  {-# INLINE threefryR #-}

instance (ThreefryR R8 N2 w) => ThreefryR R9 N2 w where
  threefryR = stdRot2 (rot0 :: Rot N2 w)
  {-# INLINE threefryR #-}

instance (ThreefryR R9 N2 w) => ThreefryR R10 N2 w where
  threefryR = stdRot2 (rot1 :: Rot N2 w)
  {-# INLINE threefryR #-}

instance (ThreefryR R10 N2 w) => ThreefryR R11 N2 w where
  threefryR = stdRot2 (rot2 :: Rot N2 w)
  {-# INLINE threefryR #-}

instance (ThreefryR R11 N2 w) => ThreefryR R12 N2 w where
  threefryR r x ks ks2 k = stdRot2 (rot3 :: Rot N2 w) r x ks ks2 $ \ x' ->
    withA x' $ \ x0 x1 ->
    withA ks $ \ ks0 ks1 ->
    let !z0 = x0 + ks0
        !z1 = x1 + ks1 + 3
    in k (mkA z0 z1)
  {-# INLINE threefryR #-}

instance (ThreefryR R12 N2 w) => ThreefryR R13 N2 w where
  threefryR = stdRot2 (rot4 :: Rot N2 w)
  {-# INLINE threefryR #-}

instance (ThreefryR R13 N2 w) => ThreefryR R14 N2 w where
  threefryR = stdRot2 (rot5 :: Rot N2 w)
  {-# INLINE threefryR #-}

instance (ThreefryR R14 N2 w) => ThreefryR R15 N2 w where
  threefryR = stdRot2 (rot6 :: Rot N2 w)
  {-# INLINE threefryR #-}

instance (ThreefryR R15 N2 w) => ThreefryR R16 N2 w where
  threefryR r x ks ks2 k = stdRot2 (rot7 :: Rot N2 w) r x ks ks2 $ \ x' ->
    withA x' $ \ x0 x1 ->
    withA ks $ \ _ks0 ks1 ->
    let !z0 = x0 + ks1
        !z1 = x1 + ks2 + 4
    in k (mkA z0 z1)
  {-# INLINE threefryR #-}

instance (ThreefryR R16 N2 w) => ThreefryR R17 N2 w where
  threefryR = stdRot2 (rot0 :: Rot N2 w)
  {-# INLINE threefryR #-}

instance (ThreefryR R17 N2 w) => ThreefryR R18 N2 w where
  threefryR = stdRot2 (rot1 :: Rot N2 w)
  {-# INLINE threefryR #-}

instance (ThreefryR R18 N2 w) => ThreefryR R19 N2 w where
  threefryR = stdRot2 (rot2 :: Rot N2 w)
  {-# INLINE threefryR #-}

instance (ThreefryR R19 N2 w) => ThreefryR R20 N2 w where
  threefryR r x ks ks2 k = stdRot2 (rot3 :: Rot N2 w) r x ks ks2 $ \ x' ->
    withA x' $ \ x0 x1 ->
    withA ks $ \ ks0 _ks1 ->
    let !z0 = x0 + ks2
        !z1 = x1 + ks0 + 5
    in k (mkA z0 z1)
  {-# INLINE threefryR #-}

instance (ThreefryR R20 N2 w) => ThreefryR R21 N2 w where
  threefryR = stdRot2 (rot4 :: Rot N2 w)
  {-# INLINE threefryR #-}

instance (ThreefryR R21 N2 w) => ThreefryR R22 N2 w where
  threefryR = stdRot2 (rot5 :: Rot N2 w)
  {-# INLINE threefryR #-}

instance (ThreefryR R22 N2 w) => ThreefryR R23 N2 w where
  threefryR = stdRot2 (rot6 :: Rot N2 w)
  {-# INLINE threefryR #-}

instance (ThreefryR R23 N2 w) => ThreefryR R24 N2 w where
  threefryR r x ks ks2 k = stdRot2 (rot7 :: Rot N2 w) r x ks ks2 $ \ x' ->
    withA x' $ \ x0 x1 ->
    withA ks $ \ ks0 ks1 ->
    let !z0 = x0 + ks0
        !z1 = x1 + ks1 + 6
    in k (mkA z0 z1)
  {-# INLINE threefryR #-}

instance (ThreefryR R24 N2 w) => ThreefryR R25 N2 w where
  threefryR = stdRot2 (rot0 :: Rot N2 w)
  {-# INLINE threefryR #-}

instance (ThreefryR R25 N2 w) => ThreefryR R26 N2 w where
  threefryR = stdRot2 (rot1 :: Rot N2 w)
  {-# INLINE threefryR #-}

instance (ThreefryR R26 N2 w) => ThreefryR R27 N2 w where
  threefryR = stdRot2 (rot2 :: Rot N2 w)
  {-# INLINE threefryR #-}

instance (ThreefryR R27 N2 w) => ThreefryR R28 N2 w where
  threefryR r x ks ks2 k = stdRot2 (rot3 :: Rot N2 w) r x ks ks2 $ \ x' ->
    withA x' $ \ x0 x1 ->
    withA ks $ \ _ks0 ks1 ->
    let !z0 = x0 + ks1
        !z1 = x1 + ks2 + 7
    in k (mkA z0 z1)
  {-# INLINE threefryR #-}

instance (ThreefryR R28 N2 w) => ThreefryR R29 N2 w where
  threefryR = stdRot2 (rot4 :: Rot N2 w)
  {-# INLINE threefryR #-}

instance (ThreefryR R29 N2 w) => ThreefryR R30 N2 w where
  threefryR = stdRot2 (rot5 :: Rot N2 w)
  {-# INLINE threefryR #-}

instance (ThreefryR R30 N2 w) => ThreefryR R31 N2 w where
  threefryR = stdRot2 (rot6 :: Rot N2 w)
  {-# INLINE threefryR #-}

instance (ThreefryR R31 N2 w) => ThreefryR R32 N2 w where
  threefryR r x ks ks2 k = stdRot2 (rot7 :: Rot N2 w) r x ks ks2 $ \ x' ->
    withA x' $ \ x0 x1 ->
    withA ks $ \ ks0 _ks1 ->
    let !z0 = x0 + ks2
        !z1 = x1 + ks0 + 8
    in k (mkA z0 z1)
  {-# INLINE threefryR #-}

instance (ThreefryR R0 N4 w) => ThreefryR R1 N4 w where
  threefryR = stdRot4_0 (rot0 :: Rot N4 w)
  {-# INLINE threefryR #-}

instance (ThreefryR R1 N4 w) => ThreefryR R2 N4 w where
  threefryR = stdRot4_1 (rot1 :: Rot N4 w)
  {-# INLINE threefryR #-}

instance (ThreefryR R2 N4 w) => ThreefryR R3 N4 w where
  threefryR = stdRot4_0 (rot2 :: Rot N4 w)
  {-# INLINE threefryR #-}

instance (ThreefryR R3 N4 w) => ThreefryR R4 N4 w where
  threefryR r x ks ks4 k = stdRot4_1 (rot3 :: Rot N4 w) r x ks ks4 $ \ x' ->
    withA x' $ \ x0 x1 x2 x3 ->
    withA ks $ \ _ks0 ks1 ks2 ks3 ->
    let !z0 = x0 + ks1
        !z1 = x1 + ks2
        !z2 = x2 + ks3
        !z3 = x3 + ks4 + 1
    in k (mkA z0 z1 z2 z3)
  {-# INLINE threefryR #-}

instance (ThreefryR R4 N4 w) => ThreefryR R5 N4 w where
  threefryR = stdRot4_0 (rot4 :: Rot N4 w)
  {-# INLINE threefryR #-}

instance (ThreefryR R5 N4 w) => ThreefryR R6 N4 w where
  threefryR = stdRot4_1 (rot5 :: Rot N4 w)
  {-# INLINE threefryR #-}

instance (ThreefryR R6 N4 w) => ThreefryR R7 N4 w where
  threefryR = stdRot4_0 (rot6 :: Rot N4 w)
  {-# INLINE threefryR #-}

instance (ThreefryR R7 N4 w) => ThreefryR R8 N4 w where
  threefryR r x ks ks4 k = stdRot4_1 (rot7 :: Rot N4 w) r x ks ks4 $ \ x' ->
    withA x' $ \ x0 x1 x2 x3 ->
    withA ks $ \ ks0 _ks1 ks2 ks3 ->
    let !z0 = x0 + ks2
        !z1 = x1 + ks3
        !z2 = x2 + ks4
        !z3 = x3 + ks0 + 2
    in k (mkA z0 z1 z2 z3)
  {-# INLINE threefryR #-}

instance (ThreefryR R8 N4 w) => ThreefryR R9 N4 w where
  threefryR = stdRot4_0 (rot0 :: Rot N4 w)
  {-# INLINE threefryR #-}

instance (ThreefryR R9 N4 w) => ThreefryR R10 N4 w where
  threefryR = stdRot4_1 (rot1 :: Rot N4 w)
  {-# INLINE threefryR #-}

instance (ThreefryR R10 N4 w) => ThreefryR R11 N4 w where
  threefryR = stdRot4_0 (rot2 :: Rot N4 w)
  {-# INLINE threefryR #-}

instance (ThreefryR R11 N4 w) => ThreefryR R12 N4 w where
  threefryR r x ks ks4 k = stdRot4_1 (rot3 :: Rot N4 w) r x ks ks4 $ \ x' ->
    withA x' $ \ x0 x1 x2 x3 ->
    withA ks $ \ ks0 ks1 _ks2 ks3 ->
    let !z0 = x0 + ks3
        !z1 = x1 + ks4
        !z2 = x2 + ks0
        !z3 = x3 + ks1 + 3
    in k (mkA z0 z1 z2 z3)
  {-# INLINE threefryR #-}

instance (ThreefryR R12 N4 w) => ThreefryR R13 N4 w where
  threefryR = stdRot4_0 (rot4 :: Rot N4 w)
  {-# INLINE threefryR #-}

instance (ThreefryR R13 N4 w) => ThreefryR R14 N4 w where
  threefryR = stdRot4_1 (rot5 :: Rot N4 w)
  {-# INLINE threefryR #-}

instance (ThreefryR R14 N4 w) => ThreefryR R15 N4 w where
  threefryR = stdRot4_0 (rot6 :: Rot N4 w)
  {-# INLINE threefryR #-}

instance (ThreefryR R15 N4 w) => ThreefryR R16 N4 w where
  threefryR r x ks ks4 k = stdRot4_1 (rot7 :: Rot N4 w) r x ks ks4 $ \ x' ->
    withA x' $ \ x0 x1 x2 x3 ->
    withA ks $ \ ks0 ks1 ks2 _ks3 ->
    let !z0 = x0 + ks4
        !z1 = x1 + ks0
        !z2 = x2 + ks1
        !z3 = x3 + ks2 + 4
    in k (mkA z0 z1 z2 z3)
  {-# INLINE threefryR #-}

instance (ThreefryR R16 N4 w) => ThreefryR R17 N4 w where
  threefryR = stdRot4_0 (rot0 :: Rot N4 w)
  {-# INLINE threefryR #-}

instance (ThreefryR R17 N4 w) => ThreefryR R18 N4 w where
  threefryR = stdRot4_1 (rot1 :: Rot N4 w)
  {-# INLINE threefryR #-}

instance (ThreefryR R18 N4 w) => ThreefryR R19 N4 w where
  threefryR = stdRot4_0 (rot2 :: Rot N4 w)
  {-# INLINE threefryR #-}

instance (ThreefryR R19 N4 w) => ThreefryR R20 N4 w where
  threefryR r x ks ks4 k = stdRot4_1 (rot3 :: Rot N4 w) r x ks ks4 $ \ x' ->
    withA x' $ \ x0 x1 x2 x3 ->
    withA ks $ \ ks0 ks1 ks2 ks3 ->
    let !z0 = x0 + ks0
        !z1 = x1 + ks1
        !z2 = x2 + ks2
        !z3 = x3 + ks3 + 5
    in k (mkA z0 z1 z2 z3)
  {-# INLINE threefryR #-}

instance (ThreefryR R20 N4 w) => ThreefryR R21 N4 w where
  threefryR = stdRot4_0 (rot4 :: Rot N4 w)
  {-# INLINE threefryR #-}

instance (ThreefryR R21 N4 w) => ThreefryR R22 N4 w where
  threefryR = stdRot4_1 (rot5 :: Rot N4 w)
  {-# INLINE threefryR #-}

instance (ThreefryR R22 N4 w) => ThreefryR R23 N4 w where
  threefryR = stdRot4_0 (rot6 :: Rot N4 w)
  {-# INLINE threefryR #-}

instance (ThreefryR R23 N4 w) => ThreefryR R24 N4 w where
  threefryR r x ks ks4 k = stdRot4_1 (rot7 :: Rot N4 w) r x ks ks4 $ \ x' ->
    withA x' $ \ x0 x1 x2 x3 ->
    withA ks $ \ _ks0 ks1 ks2 ks3 ->
    let !z0 = x0 + ks1
        !z1 = x1 + ks2
        !z2 = x2 + ks3
        !z3 = x3 + ks4 + 6
    in k (mkA z0 z1 z2 z3)
  {-# INLINE threefryR #-}

instance (ThreefryR R24 N4 w) => ThreefryR R25 N4 w where
  threefryR = stdRot4_0 (rot0 :: Rot N4 w)
  {-# INLINE threefryR #-}

instance (ThreefryR R25 N4 w) => ThreefryR R26 N4 w where
  threefryR = stdRot4_1 (rot1 :: Rot N4 w)
  {-# INLINE threefryR #-}

instance (ThreefryR R26 N4 w) => ThreefryR R27 N4 w where
  threefryR = stdRot4_0 (rot2 :: Rot N4 w)
  {-# INLINE threefryR #-}

instance (ThreefryR R27 N4 w) => ThreefryR R28 N4 w where
  threefryR r x ks ks4 k = stdRot4_1 (rot3 :: Rot N4 w) r x ks ks4 $ \ x' ->
    withA x' $ \ x0 x1 x2 x3 ->
    withA ks $ \ ks0 _ks1 ks2 ks3 ->
    let !z0 = x0 + ks2
        !z1 = x1 + ks3
        !z2 = x2 + ks4
        !z3 = x3 + ks0 + 7
    in k (mkA z0 z1 z2 z3)
  {-# INLINE threefryR #-}

instance (ThreefryR R28 N4 w) => ThreefryR R29 N4 w where
  threefryR = stdRot4_0 (rot4 :: Rot N4 w)
  {-# INLINE threefryR #-}

instance (ThreefryR R29 N4 w) => ThreefryR R30 N4 w where
  threefryR = stdRot4_1 (rot5 :: Rot N4 w)
  {-# INLINE threefryR #-}

instance (ThreefryR R30 N4 w) => ThreefryR R31 N4 w where
  threefryR = stdRot4_0 (rot6 :: Rot N4 w)
  {-# INLINE threefryR #-}

instance (ThreefryR R31 N4 w) => ThreefryR R32 N4 w where
  threefryR r x ks ks4 k = stdRot4_1 (rot7 :: Rot N4 w) r x ks ks4 $ \ x' ->
    withA x' $ \ x0 x1 x2 x3 ->
    withA ks $ \ ks0 ks1 _ks2 ks3 ->
    let !z0 = x0 + ks3
        !z1 = x1 + ks4
        !z2 = x2 + ks0
        !z3 = x3 + ks1 + 8
    in k (mkA z0 z1 z2 z3)
  {-# INLINE threefryR #-}

-- | The state of a Threefry RNG.
data ThreefryData n w = T !(ThreefryCtr n w) !(ThreefryKey n w)

class (Counter n, ToDoubles n w, WithA n w, ThreefryW w, ThreefryN n, ThreefryRot n w) => Threefry n w where
  stream  :: ThreefryCtr n w -> ThreefryKey n w -> [W w]
  ustream :: ThreefryCtr n w -> ThreefryKey n w -> [Double]

instance Threefry N2 W32 where
  stream ctr key = go (init ctr key)
    where
      go !pd = case step pd of
                 (r, npd) -> withA r $ \ a0 a1 -> a0 : a1 : go npd
  {- INLINE stream #-}
  ustream ctr key = go (init ctr key)
    where
      go !pd = case step pd of
                 (r, npd) -> withA r $ \ a0 a1 -> wordsToDouble a0 a1 : go npd
  {-# INLINE ustream #-}

instance Threefry N4 W32 where
  stream ctr key = go (init ctr key)
    where
      go !pd = case step pd of
                 (r, npd) -> withA r $ \ a0 a1 a2 a3 -> a0 : a1 : a2 : a3 : go npd
  {- INLINE stream #-}
  ustream ctr key = go (init ctr key)
    where
      go !pd = case step pd of
                 (r, npd) -> withA r $ \ a0 a1 a2 a3 -> wordsToDouble a0 a1 : wordsToDouble a2 a3 : go npd
  {- INLINE ustream #-}

instance Threefry N2 W64 where
  stream ctr key = go (init ctr key)
    where
      go !pd = case step pd of
                 (r, npd) -> withA r $ \ a0 a1 -> a0 : a1 : go npd
  {- INLINE stream #-}
  ustream ctr key = go (init ctr key)
    where
      go !pd = case step pd of
                 (r, npd) -> withA r $ \ a0 a1 -> wordToDouble a0 : wordToDouble a1 : go npd
  {- INLINE ustream #-}

instance Threefry N4 W64 where
  stream ctr key = go (init ctr key)
    where
      go !pd = case step pd of
                 (r, npd) -> withA r $ \ a0 a1 a2 a3 -> a0 : a1 : a2 : a3 : go npd
  {- INLINE stream #-}
  ustream ctr key = go (init ctr key)
    where
      go !pd = case step pd of
                 (r, npd) -> withA r $ \ a0 a1 a2 a3 -> wordToDouble a0 : wordToDouble a1 : wordToDouble a2 : wordToDouble a3 : go npd
  {- INLINE ustream #-}

type RD = R20

-- | Initialize a Threefry RNG with a counter and a key.
init :: ThreefryCtr n w -> ThreefryKey n w -> ThreefryData n w
init = T
{-# INLINE init #-}

-- | Increment the counter.
inc :: (Threefry n w) => ThreefryData n w -> ThreefryData n w
inc (T ctr key) = T (incr ctr) key
{-# INLINE inc #-}

-- | Get the current random data.
get :: (Threefry n w, ThreefryR RD n w) => ThreefryData n w -> A n w
get (T ctr key) = threefry (Rounds :: Rounds RD) ctr key
{-# INLINE get #-}

-- | Get the current random data and increment the counter.
step :: (Threefry n w, ThreefryR RD n w) => ThreefryData n w -> (A n w, ThreefryData n w)
step rng = (get rng, inc rng)
{-# INLINE step #-}

data ThreefryState s n w = TS {-# UNPACK #-} !(MutVar s DoubleList) {-# UNPACK #-} !(MutVar s (ThreefryData n w))

type ThreefryStateIO   = ThreefryState (PrimState IO)
type ThreefryStateST s = ThreefryState (PrimState (ST s))

new :: (PrimMonad m, Threefry n w) => A n w -> A n w -> m (ThreefryState (PrimState m) n w)
new ctr key = do
  svd <- newMutVar DoubleNil
  ph  <- newMutVar (init ctr key)
  return (TS svd ph)

uniform :: (PrimMonad m, PrimState m ~ s, Threefry n w, ThreefryR RD n w) => ThreefryState s n w -> m Double
uniform (TS svd ph) = do
  ds <- readMutVar svd
  case ds of
    DoubleCons x xs -> do
                writeMutVar svd xs
                return x
    DoubleNil -> do
                rng <- readMutVar ph
                toDoublesL (get rng) $ \ r rs -> do
                    writeMutVar ph $! inc rng
                    writeMutVar svd rs
                    return r
{-# INLINE uniform #-}

newtype ThreefryStateRaw s n w = TSR (MutVar s (ThreefryData n w))

type ThreefryStateRawIO   = ThreefryStateRaw (PrimState IO)
type ThreefryStateRawST s = ThreefryStateRaw (PrimState (ST s))

newRaw :: (PrimMonad m, Threefry n w) => A n w -> A n w -> m (ThreefryStateRaw (PrimState m) n w)
newRaw ctr key = do
  tf <- newMutVar (init ctr key)
  return (TSR tf)

nextRaw :: (PrimMonad m, PrimState m ~ s, Threefry n w, ThreefryR RD n w) => ThreefryStateRaw s n w -> m (A n w)
nextRaw (TSR tf) = do
  rng <- readMutVar tf
  writeMutVar tf $! inc rng
  return (get rng)
{-# INLINE nextRaw #-}
