module Main where

import           System.Random.PermutedCongruentialGenerators
import qualified Data.Vector.Unboxed           as VU
import qualified Data.Vector.Unboxed.Mutable   as VUM

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

-- LinearCongruentialGenerators
-- [0.24980341101223025,8.322402520666e-2,3.1201541829815004e-2,1.2479942152621047e-2]
-- PermutedCongruentialGenerators
-- [0.5316269040876233,0.3655769299412534,0.2802864910194551,0.2282810667190318]
-- XorShift
-- [0.5000706192024326,0.33337456734842696,0.2500023082321171,0.1999867751358481]
-- XoRoShiRo
-- [0.5001002852984787,0.3335199057374253,0.25022346162880293,0.20023452725995347]
-- メルセンヌツイスタ
-- [0.5004688872835983,0.3336860184269407,0.2502557178253535,0.200192335777942]