

module Main where
import Data.Vector.Unboxed              as V
import Prelude                          as P
import Unboxed


test    :: Vector Int
        -> Vector Int
        -> Vector Int

test aa bb
        = V.zipWith3
                (\x y z -> x + y + z)
                aa
                bb
                (V.replicate (V.length aa) 12345)


test2   :: Vector Int
        -> Vector Int
        -> Vector Int

test2 aa bb 
        = map3  (\x y z -> x + y + z)
                aa 
                bb
                (V.replicate (V.length aa) 12345)

main
 = do   let xs1 = V.fromList [1, 2, 3, 4, 5]
        let xs2 = V.fromList [6, 7, 8, 9, 9]

        print $ test  xs1 xs2
        print $ test2 xs1 xs2
