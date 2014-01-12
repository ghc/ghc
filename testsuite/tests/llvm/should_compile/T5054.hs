{-# OPTIONS_GHC -W #-}

import Data.Int
import Data.Packed
import Data.Packed.ST
import Control.Monad.ST
import Foreign.Storable
import Foreign.Ptr
import Foreign.Marshal.Utils

main :: IO ()
main = print $ arst (zeroMatrix 10 10) (Constant 9)

data ComputeElement
    = Constant !Double
    | Value !Double
  deriving (Eq)

isConstant (Constant _) = True
isConstant _            = False

instance Element ComputeElement

fromComputeElement (Constant v) = v
fromComputeElement (Value    v) = v

sizeofDouble = sizeOf (undefined :: Double)
sizeofInt64  = sizeOf (undefined :: Int64)

instance Storable ComputeElement where
    sizeOf    _ = sizeofDouble + sizeofInt64
    alignment _ = 16

    peek p = do
        v <- peek (castPtr p)
        c <- peek (castPtr (p `plusPtr` sizeofDouble))
        return $ if toBool (c :: Int64)
            then Constant v
            else Value v

    poke p v = do
        let c :: Int64
            c = fromBool (isConstant v)
        poke (castPtr p) (fromComputeElement v)
        poke (castPtr p `plusPtr` sizeofDouble) c


arst mat v = runST $ do
    mat' <- thawMatrix mat
    writeMatrix mat' 1 2 v
    x <- fromComputeElement `fmap` readMatrix mat' 1 9
    return (x > 0)

zeroMatrix m n = buildMatrix m n (const (Value 0))

