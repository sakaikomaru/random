module Main where

import           System.Random.MiddleSquare
import qualified Data.Vector.Unboxed                          as VU
import qualified Data.Vector.Unboxed.Mutable                  as VUM

main :: IO ()
main = do
  rng <- newRNG
  rx <- VUM.replicate 4 (0.0 :: Double)
  rep 1000000 $ \_ -> do
    x <- nextDouble rng
    VUM.unsafeModify rx (+x) 0
    VUM.unsafeModify rx (+(x*x)) 1
    VUM.unsafeModify rx (+(x*x*x)) 2
    VUM.unsafeModify rx (+(x*x*x*x)) 3
  rep 4 $ \i -> VUM.unsafeModify rx (/1000000) i
  print =<< VU.unsafeFreeze rx
