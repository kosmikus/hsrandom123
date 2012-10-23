{-# LANGUAGE DataKinds #-}
module Main where

import Control.Monad
import Control.Monad.Primitive
import Criterion.Main
import System.Random.Philox   as P
import System.Random.Threefry as T
import System.Random.MWC      as MWC

main :: IO ()
main = do
  philox2x32   <- P.new (zero :: A N2 W32) zero
  philox4x32   <- P.new (zero :: A N4 W32) zero
  philox2x64   <- P.new (zero :: A N2 W64) zero
  philox4x64   <- P.new (zero :: A N4 W64) zero
  threefry2x32 <- T.new (zero :: A N2 W32) zero
  threefry4x32 <- T.new (zero :: A N4 W32) zero
  threefry2x64 <- T.new (zero :: A N2 W64) zero
  threefry4x64 <- T.new (zero :: A N4 W64) zero
  mwc          <- MWC.create
  defaultMain [
      bgroup "Philox typed" [
          bench "philox2x32" $ nfIO (P.uniform philox2x32)
        , bench "philox4x32" $ nfIO (P.uniform philox4x32)
        , bench "philox2x64" $ nfIO (P.uniform philox2x64)
        , bench "philox4x64" $ nfIO (P.uniform philox4x64)
      ]
    , bgroup "Threefry typed" [
          bench "threefry2x32" $ nfIO (T.uniform threefry2x32)
        , bench "threefry4x32" $ nfIO (T.uniform threefry4x32)
        , bench "threefry2x64" $ nfIO (T.uniform threefry2x64)
        , bench "threefry4x64" $ nfIO (T.uniform threefry4x64)
      ]
    , bgroup "MWC" [
          bench "mwc"         $ nfIO (MWC.uniform mwc :: IO Double)
      ]
{-
{-
    , bgroup "Philox raw" [
          bench "philoxT2x32Raw" $ nfIO (PT.nextRaw philoxT2x32R)
        , bench "philoxT4x32Raw" $ nfIO (PT.nextRaw philoxT4x32R)
        , bench "philoxT2x64Raw" $ nfIO (PT.nextRaw philoxT2x64R)
        , bench "philoxT4x64Raw" $ nfIO (PT.nextRaw philoxT4x64R)
      ]
    , bgroup "Threefry raw" [
          bench "threefryT2x32Raw" $ nfIO (TT.nextRaw threefryT2x32R)
        , bench "threefryT4x32Raw" $ nfIO (TT.nextRaw threefryT4x32R)
        , bench "threefryT2x64Raw" $ nfIO (TT.nextRaw threefryT2x64R)
        , bench "threefryT4x64Raw" $ nfIO (TT.nextRaw threefryT4x64R)
      ]
-}
    , bgroup "Philox stream" [
          bench "philoxT2x32" $ nf (take 1000 . streamToDouble32 . PT.stream PT.zero) (A2 (A1x32 0) :: A2 N2 W32)
        , bench "philoxT4x32" $ nf (take 1000 . streamToDouble32 . PT.stream PT.zero) (A2 (A2x32 0 0) :: A2 N4 W32)
        , bench "philoxT2x64" $ nf (take 1000 . streamToDouble64 . PT.stream PT.zero) (A2 (A1x64 0) :: A2 N2 W64)
        , bench "philoxT4x64" $ nf (take 1000 . streamToDouble64 . PT.stream PT.zero) (A2 (A2x64 0 0) :: A2 N4 W64)
        , bench "philoxT2x32u" $ nf (take 1000 . PT.ustream PT.zero) (A2 (A1x32 0) :: A2 N2 W32)
        , bench "philoxT4x32u" $ nf (take 1000 . PT.ustream PT.zero) (A2 (A2x32 0 0) :: A2 N4 W32)
        , bench "philoxT2x64u" $ nf (take 1000 . PT.ustream PT.zero) (A2 (A1x64 0) :: A2 N2 W64)
        , bench "philoxT4x64u" $ nf (take 1000 . PT.ustream PT.zero) (A2 (A2x64 0 0) :: A2 N4 W64)
        , bench "philoxT2x32'" $ nfIO ((PT.new PT.zero (PT.A1x32 0) :: IO (PhiloxState RealWorld N2 W32)) >>= \ g -> replicateM_ 1000 (PT.uniform g) :: IO ())
        , bench "philoxT4x32'" $ nfIO ((PT.new PT.zero (PT.A2x32 0 0) :: IO (PhiloxState RealWorld N4 W32)) >>= \ g -> replicateM_ 1000 (PT.uniform g) :: IO ())
        , bench "philoxT2x64'" $ nfIO ((PT.new PT.zero (PT.A1x64 0) :: IO (PhiloxState RealWorld N2 W64)) >>= \ g -> replicateM_ 1000 (PT.uniform g) :: IO ())
        , bench "philoxT4x64'" $ nfIO ((PT.new PT.zero (PT.A2x64 0 0) :: IO (PhiloxState RealWorld N4 W64)) >>= \ g -> replicateM_ 1000 (PT.uniform g) :: IO ())
        , bench "philox1" $ nf (take 1000 . P1.dstream4x32 0) 0
      ]
    , bgroup "Threefry stream" [
{-
          bench "threefryT2x32" $ nf (take 1000 . streamToDouble32 . TT.stream) (A2x32 0 0)
        , bench "threefryT4x32" $ nf (take 1000 . streamToDouble32 . TT.stream) (A4x32 0 0 0 0)
        , bench "threefryT2x64" $ nf (take 1000 . streamToDouble64 . TT.stream) (A2x64 0 0)
        , bench "threefryT4x64" $ nf (take 1000 . streamToDouble64 . TT.stream) (A4x64 0 0 0 0)
-}
          bench "threefryT2x32u" $ nf (take 1000 . TT.ustream TT.zero) (A2x32 0 0)
        , bench "threefryT4x32u" $ nf (take 1000 . TT.ustream TT.zero) (A4x32 0 0 0 0)
        , bench "threefryT2x64u" $ nf (take 1000 . TT.ustream TT.zero) (A2x64 0 0)
        , bench "threefryT4x64u" $ nf (take 1000 . TT.ustream TT.zero) (A4x64 0 0 0 0)
{-
        , bench "threefryT2x32'" $ nfIO ((TT.new (PT.A2x32 0 0) :: IO (ThreefryState N2 W32)) >>= \ g -> replicateM 1000 (TT.uniform g) :: IO [Double])
        , bench "threefryT4x32'" $ nfIO ((TT.new (PT.A4x32 0 0 0 0) :: IO (ThreefryState N4 W32)) >>= \ g -> replicateM 1000 (TT.uniform g) :: IO [Double])
        , bench "threefryT2x64'" $ nfIO ((TT.new (PT.A2x64 0 0) :: IO (ThreefryState N2 W64)) >>= \ g -> replicateM 1000 (TT.uniform g) :: IO [Double])
        , bench "threefryT4x64'" $ nfIO ((TT.new (PT.A4x64 0 0 0 0) :: IO (ThreefryState N4 W64)) >>= \ g -> replicateM 1000 (TT.uniform g) :: IO [Double])
-}
      ]
    , bgroup "mwc stream" [
          bench "mwc" $ nfIO (create >>= \ g -> replicateM 1000 (MWC.uniform g) :: IO [Double])
      ]
{-
    , bgroup "C raw" [
          bench "Cphilox4x32Raw" $ nfIO (PC.nextRaw philoxCR)
      ]
    , bgroup "toDouble" [
          bench "wordsToDouble" $ nf (TT.wordsToDouble 0) 0
        , bench "wordTodouble" $ nf TT.wordToDouble 0
      ]
-}
-}
    ]
