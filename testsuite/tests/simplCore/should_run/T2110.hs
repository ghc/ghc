{-# LANGUAGE MagicHash #-}

import GHC.Exts
import Unsafe.Coerce

newtype Age = Age Int

foo :: [Int] -> [Int]
foo = map id
fooAge :: [Int] -> [Age]
fooAge = map Age
fooCoerce :: [Int] -> [Age]
fooCoerce = map coerce
fooUnsafeCoerce :: [Int] -> [Age]
fooUnsafeCoerce = map unsafeCoerce

same :: a -> b -> IO ()
same x y = case reallyUnsafePtrEquality# (unsafeCoerce x) y of
    1# -> putStrLn "yes"
    _  -> putStrLn "no"

main = do
    let l = [1,2,3]
    same (foo l) l
    same (fooAge l) l
    same (fooCoerce l) l
    same (fooUnsafeCoerce l) l
