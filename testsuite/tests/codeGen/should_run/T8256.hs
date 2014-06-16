{-# LANGUAGE MagicHash, UnboxedTuples #-}



import GHC.Prim

import Data.Vector.Storable.Mutable
import Foreign.Ptr
import GHC.ST
import Data.Primitive.ByteArray
import Control.Monad.Primitive



sameByteArray  :: Control.Monad.Primitive.PrimMonad m => ByteArray -> ByteArray -> m Bool
sameByteArray ar1 ar2 =
        do  v1 <- unsafeThawByteArray ar1
            v2 <- unsafeThawByteArray ar2
            return $ sameMutableByteArray v1 v2

pf0 (ByteArray by) =  ByteArray ( prefetchByteArray0# by 1#)

pf1 (ByteArray by) =  ByteArray  (prefetchByteArray1# by 1#)

pf2 (ByteArray by) = ByteArray (  prefetchByteArray2# by 1#)

pf3 (ByteArray by) = ByteArray (  prefetchByteArray3# by  1#)


monoSame v f = sameByteArray v (f v)




main :: IO ()
main = do  
    mv1 <- newByteArray 17
    v1 <- unsafeFreezeByteArray mv1
    return () 
    t0<- monoSame v1 pf0 
    t1 <- monoSame v1 pf1 
    t2 <- monoSame v1 pf2 
    t3 <- monoSame v1 pf3
    if t0 && t1 && t2 && t3  then putStrLn "success" else error "bad prefetch operation! please report" 




