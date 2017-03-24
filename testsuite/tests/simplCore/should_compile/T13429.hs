{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
module Loop (Array(..), Image(..), X, promote, correlate) where
import           Data.Maybe (fromMaybe)

data Kernel e = Kernel Int Int !(Vector (Int, Int, e)) deriving (Show)


toKernel :: Array X e => Image X e -> Kernel e
toKernel img =
  Kernel m2 n2 $ filter (\(_, _, x) -> x /= 0) $ imap addIx $ toVector img
  where
    (m, n) = dims img
    (m2, n2) = (m `div` 2, n `div` 2)
    addIx k (PixelX x) =
      let (i, j) = toIx n k
      in (i - m2, j - n2, x)

correlate :: Array cs e => Image X e -> Image cs e -> Image cs e
correlate kernelImg imgM = makeImage (dims imgM) stencil
  where
    !(Kernel kM2 kN2 kernelV) = toKernel kernelImg
    kLen = length kernelV
    stencil (i, j) =
      loop 0 (promote 0) $ \ k acc ->
        let (iDelta, jDelta, x) = kernelV !! k
            imgPx = index imgM (i + iDelta, j + jDelta)
        in liftPx2 (+) acc (liftPx (x *) imgPx)
    loop init' initAcc f = go init' initAcc
      where
        go step acc =
          if step < kLen
            then go (step + 1) (f step acc)
            else acc
{-# INLINE correlate #-}



-- | A Pixel family with a color space and a precision of elements.
data family Pixel cs e :: *


class (Eq e, Num e) => ColorSpace cs e where
  promote :: e -> Pixel cs e
  liftPx :: (e -> e) -> Pixel cs e -> Pixel cs e
  liftPx2 :: (e -> e -> e) -> Pixel cs e -> Pixel cs e -> Pixel cs e



data family Image cs e :: *

class ColorSpace cs e => Array cs e where
  dims :: Image cs e -> (Int, Int)
  makeImage :: (Int, Int) -> ((Int, Int) -> Pixel cs e) -> Image cs e
  toVector :: Image cs e -> Vector (Pixel cs e)
  index :: Image cs e -> (Int, Int) -> Pixel cs e

fromIx :: Int -> (Int, Int) -> Int
fromIx n (i, j) = n * i + j

toIx :: Int -> Int -> (Int, Int)
toIx n k = divMod k n

instance (Show (Pixel cs e), ColorSpace cs e, Array cs e) =>
         Show (Image cs e) where
  show img =
    let (m, n) = dims img
    in "<Image " ++ show m ++ "x" ++ show n ++ ">: " ++ show (toVector img)


data X = X

newtype instance Pixel X e = PixelX e

instance Show e => Show (Pixel X e) where
  show (PixelX e) = "Pixel: " ++ show e


instance (Eq e, Num e) => ColorSpace X e where
  promote = PixelX
  liftPx f (PixelX g) = PixelX (f g)
  liftPx2 f (PixelX g1) (PixelX g2) = PixelX (f g1 g2)


data instance Image X e = VImage Int Int (Vector (Pixel X e))

instance ColorSpace X e => Array X e where
  dims (VImage m n _) = (m, n)
  makeImage (m, n) f = VImage m n $ generate (m * n) (f . toIx n)
  toVector (VImage _ _ v) = v
  index (VImage _ n v) ix = fromMaybe (promote 0) (v !? (fromIx n ix))


-- Vector emulation

type Vector a = [a]

imap :: (Num a, Enum a) => (a -> b -> c) -> [b] -> [c]
imap f = zipWith f [0..]

(!?) :: [a] -> Int -> Maybe a
(!?) ls i
  | i < 0 || i >= length ls = Nothing
  | otherwise = Just (ls !! i)

generate :: (Ord t, Num t) => t -> (t -> a) -> [a]
generate n f = go (n-1) [] where
  go i acc | i < 0 = acc
           | otherwise = go (i-1) (f i : acc)

