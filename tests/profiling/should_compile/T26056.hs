module M where

import GHC.Exts      ( Any )
import Unsafe.Coerce ( unsafeCoerce )

data Sigma = MkT Any

testSubList :: Maybe Bool -> Sigma -> Sigma
testSubList (Just x) final = {-# SCC "y" #-} (
   let x' = seq x ()
   in case testSubList Nothing final of
         MkT w -> {-# SCC "x" #-}
                    (unsafeCoerce MkT (konst x' myHead (unsafeCoerce w))))
testSubList Nothing final = final

myHead :: [a] -> a
myHead (x:_) = x

konst :: () -> ([a] -> a) -> [a] -> a
konst _ x = x
{-# OPAQUE konst #-}
