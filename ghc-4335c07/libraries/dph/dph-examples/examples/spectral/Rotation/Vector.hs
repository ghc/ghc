module Vector
    (solveV
    )
where
import qualified Data.Vector.Unboxed    as VU

type Point = (Double,Double)

{-# NOINLINE solveV #-}
solveV
    :: Int      -- ^ depth
    -> Double   -- ^ time
    -> VU.Vector Point
solveV depth t
 = let s p   = solveV' t depth p
   in s 0 VU.++ s (pi/2) VU.++ s pi VU.++ s (3*pi / 2)

solveV' :: Double -> Int -> Double -> VU.Vector Point
solveV' t iG pG
 = let 
       {-# INLINE l #-}
       l         = fromIntegral iG
       {-# INLINE p #-}
       p         = pG
       {-# INLINE f #-}
       f         = fromIntegral iG / 20

       
       {-# INLINE r' #-}
       r'        = p + f*t
       cos'      = cos r'
       sin'      = sin r'
       (px, py)  = (- l * sin', l * cos')

       {-# INLINE pts #-}
       pts       = VU.concatMap (\iG2 -> solveV' t (iG - 1) (fromIntegral iG2 / l * 2 * pi - pi)) (VU.enumFromN (1::Int) iG)
       {-# INLINE pts' #-}
       pts'      = VU.map (\(x,y) -> (x * cos' - y * sin' + px, x * sin' + y * cos' + py)) pts
   in VU.singleton (px, py) VU.++ pts'


