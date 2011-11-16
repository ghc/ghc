{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -W #-}

import Data.Int
import Data.Packed
import Data.Packed.ST
import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Foreign.Storable
import Foreign.Ptr
import Foreign.Marshal.Utils

import Control.Parallel.Strategies

import Graphics.Plot


main :: IO ()
main = let whee = jacobiST zeroRho (0, 1) (constLeftBorder 100 128)
       in writeFile "Something.pgm" $ matrixToPGM (computeElementMatrixToDouble whee)

inParallel = parMap rwhnf id

zeroMatrix m n = buildMatrix m n (const 0)

twoMatrix m n = buildMatrix m n (const (Value 2))

data ComputeElement = Constant !Double
                    | Value !Double
                    deriving (Eq)

-- We don't care about showing if it's constant or not
instance Show ComputeElement where
  show (Constant v) = show v
  show (Value v) = show v

instance Element ComputeElement

isConstant (Constant _) = True
isConstant _            = False

fromComputeElement (Constant v) = v
fromComputeElement (Value    v) = v

sizeofDouble = sizeOf (undefined :: Double)
sizeofInt64  = sizeOf (undefined :: Int64)

instance Storable ComputeElement where
  sizeOf _ = sizeofDouble + sizeofInt64
  alignment _ = 16

  peek p = do v <- peek (castPtr p)
              c <- peek (castPtr (p `plusPtr` sizeofDouble))
              return $ if toBool (c :: Int64)
                         then Constant v
                         else Value v

  poke p v = do let c :: Int64
                    c = fromBool (isConstant v)
                poke (castPtr p) (fromComputeElement v)
                poke (castPtr p `plusPtr` sizeofDouble) c

jacobi :: Element a => Int -> Matrix a -> Matrix a
jacobi n mat = undefined
  where
    core = subMatrix (1, 1) (rows mat - 1, cols mat - 1) mat

applyComputeElement _ v@(Constant _) = v
applyComputeElement f   (Value    v) = Value (f v)


writeMatrix' = uncurry . writeMatrix
readMatrix'  = uncurry . readMatrix

zeroRho _ _ = 0

type STComputeMatrix s = STMatrix s ComputeElement

type RelaxationFunction s =  STComputeMatrix s    -- initial matrix
                          -> STComputeMatrix s -- new matrix
                          -> Int               -- i
                          -> Int               -- j
                          -> ST s Double       -- new element

applyMethod :: RelaxationFunction s -> STComputeMatrix s -> STComputeMatrix s -> Int -> Int -> ST s ()
applyMethod f mat mat' i j = do
  c <- readMatrix mat i j
  u <- f mat mat' i j
  writeMatrix mat' i j $ if isConstant c
                           then c
                           else Value u

{-# INLINE readElement #-}
readElement mat x y = fromComputeElement <$> readMatrix mat x y

jacobiST :: (Double -> Double -> Double) -> (Double, Double) -> Matrix ComputeElement -> Matrix ComputeElement
jacobiST rho (rangeX, rangeY) origMat = runST $ do
  let m = rows origMat
      n = cols origMat

      dx = rangeX / fromIntegral (m - 1)
      dy = rangeY / fromIntegral (n - 1)
      dd = dx * dy

      rs = [1 .. (m - 2)] -- without borders
      cs = [1 .. (n - 2)]

      evalRho i j = rho (fromIntegral i * dx) (fromIntegral j * dy)

      gaussSeidel f mat mat' i j = do
        -- Read from old matrix
        a1 <- readElement mat (i + 1) j
        a2 <- readElement mat i       (j + 1)

        -- Read from new matrix
        b1 <- readElement mat' (i - 1) j
        b2 <- readElement mat' i       (j - 1)
        let f = evalRho i j
            u = 0.25 * (a1 + a2 + b1 + b2) + (pi * f * dd)
        return u


      jacobi mat mat' i j = do
        a <- readElement mat (i + 1) j
        b <- readElement mat (i - 1) j
        c <- readElement mat i       (j + 1)
        d <- readElement mat i       (j - 1)

        let f = evalRho i j
            u = 0.25 * (a + b + c + d) + (pi * f * dd)
        return u

      jacobiThings = applyMethod jacobi

      --iterateJacobi mat mat' = sequence_ [jacobiThings mat mat' r c | r <- rs, c <- cs]

      -- faster
      iterateJacobi mat mat' = sequence_ $ map (uncurry (jacobiThings mat mat')) [(r, c) | r <- rs, c <- cs]

      -- Swap the matrices. Iterations will be an event number, 2 * n
      iterateNJacobi n mat mat' = replicateM n (iterateJacobi mat mat' >> iterateJacobi mat' mat)

  mat  <- thawMatrix origMat
  mat' <- thawMatrix origMat

  iterateNJacobi 4000 mat mat'

  freezeMatrix mat'

constLeftBorder v n = fromColumns (border:replicate (n - 1) rest)
  where border = buildVector n (const (Constant v))
        rest = buildVector n (const (Value 0))

computeElementMatrixToDouble :: Matrix ComputeElement -> Matrix Double
computeElementMatrixToDouble = liftMatrix (mapVector fromComputeElement)

