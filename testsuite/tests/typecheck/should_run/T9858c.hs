{-# LANGUAGE KindSignatures #-}
module Main(main) where

import Data.Typeable
import Data.Kind

test1 :: Bool
test1 = typeRep (Proxy :: Proxy (() :: Type)) ==
        typeRep (Proxy :: Proxy (() :: Constraint))

test2 :: Bool
test2 = typeRepTyCon (typeRep (Proxy :: Proxy (Int,Int))) ==
        typeRepTyCon (typeRep (Proxy :: Proxy (Eq Int, Eq Int)))

main :: IO ()
main = print (test1,test2)



