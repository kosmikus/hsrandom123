{-# LANGUAGE DataKinds #-}
module Main where

import Control.Monad
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
      bgroup "Philox" [
          bench "philox2x32" $ nfIO (P.uniform philox2x32)
        , bench "philox4x32" $ nfIO (P.uniform philox4x32)
        , bench "philox2x64" $ nfIO (P.uniform philox2x64)
        , bench "philox4x64" $ nfIO (P.uniform philox4x64)
      ]
    , bgroup "Threefry" [
          bench "threefry2x32" $ nfIO (T.uniform threefry2x32)
        , bench "threefry4x32" $ nfIO (T.uniform threefry4x32)
        , bench "threefry2x64" $ nfIO (T.uniform threefry2x64)
        , bench "threefry4x64" $ nfIO (T.uniform threefry4x64)
      ]
    , bgroup "Philox stream" [
          bench "philox2x32" $ nf (take 1000 . P.ustream zero) (zero :: A N1 W32)
        , bench "philox4x32" $ nf (take 1000 . P.ustream zero) (zero :: A N2 W32)
        , bench "philox2x64" $ nf (take 1000 . P.ustream zero) (zero :: A N1 W64)
        , bench "philox4x64" $ nf (take 1000 . P.ustream zero) (zero :: A N2 W64)
      ]
    , bgroup "Threefry stream" [
          bench "threefry2x32" $ nf (take 1000 . T.ustream zero) (zero :: A N2 W32)
        , bench "threefry4x32" $ nf (take 1000 . T.ustream zero) (zero :: A N4 W32)
        , bench "threefry2x64" $ nf (take 1000 . T.ustream zero) (zero :: A N2 W64)
        , bench "threefry4x64" $ nf (take 1000 . T.ustream zero) (zero :: A N4 W64)
      ]
    , bgroup "MWC" [
          bench "mwc"        $ nfIO (MWC.uniform mwc :: IO Double)
        , bench "mwc stream" $ nfIO (replicateM 1000 (MWC.uniform mwc) :: IO [Double])
      ]
    ]
