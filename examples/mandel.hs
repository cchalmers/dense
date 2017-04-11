-- This example uses the JuicyPixels package the generate the png.
import qualified Data.Dense.Unboxed as U
import Data.Dense
import Data.Complex
import Linear
import Control.Lens

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
  let a   = complexPlane (V2 3000 2000) ((-3) :+ (-2)) (3 :+ 3)
      -- The function
      --
      --   over (delayed . mapped) f
      --
      -- appies the function f an array in parallel. An equivilent way of
      -- writing this is
      --
      --   manifest . fmap f . delay
      a'  = over (delayed . mapped) (p 30) a
      img = mkImage a'

  savePngImage "mandel.png" (ImageRGB8 img)

-- Mandelbrot iteration for a complex point.
p :: RealFloat a => Int -> Complex a -> Complex a
p n z = iterate f z !! n
  where f a = a * a + z
{-# INLINE p #-}

complexPlane
  :: V2 Int         -- ^ total size
  -> Complex Double -- ^ lower bound
  -> Complex Double -- ^ upper bound
  -> UArray V2 (Complex Double)
complexPlane l@(V2 x y) a@(rmin :+ imin) (rmax :+ imax) =
  U.generate l $
    \(V2 i j) -> a + (rstep * fromIntegral i :+ istep * fromIntegral j)
  where
    rstep = (rmax - rmin) / fromIntegral x - 1
    istep = (imax - imin) / fromIntegral y - 1

mkImage :: UArray V2 (Complex Double) -> Image PixelRGB8
mkImage arr = generateImage f x y where
  V2 x y = extent arr
  f i j | k > 1      = PixelRGB8 255 255 255
        | otherwise  = PixelRGB8   0   0   0
    where k = norm (U.unsafeIndex arr (V2 i j))

-- should be in linear
instance Metric Complex

