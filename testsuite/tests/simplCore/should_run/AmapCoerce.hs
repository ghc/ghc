{-# LANGUAGE MagicHash #-}

import GHC.Exts
import Unsafe.Coerce
import Data.Array

newtype Age = Age Int

fooAge :: Array Int Int -> Array Int Age
fooAge = fmap Age
fooCoerce :: Array Int Int -> Array Int Age
fooCoerce = fmap coerce
fooUnsafeCoerce :: Array Int Int -> Array Int Age
fooUnsafeCoerce = fmap unsafeCoerce

same :: a -> b -> IO ()
same x y = case reallyUnsafePtrEquality# (unsafeCoerce x) y of
    1# -> putStrLn "yes"
    _  -> putStrLn "no"

main = do
    let l = listArray (1,3) [1,2,3]
    same (fooAge l) l
    same (fooCoerce l) l
    same (fooUnsafeCoerce l) l
