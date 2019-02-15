{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}

import GHC.Exts
import Unsafe.Coerce

newtype Age a b where
  Age :: forall b a. Int -> Age a b

foo :: [Int] -> [Int]
foo = map id
fooAge :: [Int] -> [Age a b]
fooAge = map Age
fooCoerce :: [Int] -> [Age a b]
fooCoerce = map coerce
fooUnsafeCoerce :: [Int] -> [Age a b]
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
