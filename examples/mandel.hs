-- This example uses the JuicyPixels package the generate the png.
import Control.Lens
import Data.Bool
import Data.Complex
import Data.Dense
import Data.Word

import Codec.Picture

-- Simple example to compute the manelbrot set in parallel. Compile with
--
-- ghc -O2 -threaded examples/mandel
--
-- (add -fllvm is available) and run with
--
-- examples/mandel +RTS -N4
--
-- replacing 4 with the number of processors to use.

main :: IO ()
main = do
  let plane = complexPlane (V2 8000 8000) ((-2.5) :+ (-2)) (1.5 :+ 2)
      m     = mandel 32 <$> plane
      img   = mkImage (manifest m)

  savePngImage "mandel.png" (ImageY8 img)

complexPlane
  :: V2 Int         -- ^ total size
  -> Complex Double -- ^ lower bound
  -> Complex Double -- ^ upper bound
  -> Delayed V2 (Complex Double)
complexPlane l@(V2 x y) a@(rmin :+ imin) (rmax :+ imax) =
  genDelayed l $
    \(V2 j i) -> a + (rstep * fromIntegral i :+ istep * fromIntegral j)
  where
    rstep = (rmax - rmin) / (fromIntegral x - 1)
    istep = (imax - imin) / (fromIntegral y - 1)

mandel :: Int -> Complex Double -> Word8
mandel n = bool 0 255 . any ((>2) . magnitude) . take n . criticalOrbit

criticalOrbit :: Complex Double -> [Complex Double]
criticalOrbit z0 = iterate (quadratic z0) 0
  where quadratic c z = z*z + c

mkImage :: SArray V2 Word8 -> Image Pixel8
mkImage a = Image x y (a^.vector)
  where V2 x y = extent a

