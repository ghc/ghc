{-# LANGUAGE MagicHash, UnboxedTuples, UnboxedSums, ScopedTypeVariables, TypeApplications, AllowAmbiguousTypes #-}
module Main where

import Type.Reflection
import Data.Proxy
import GHC.Types
import GHC.Prim

moduleOf :: forall a . Typeable a => String
moduleOf = case someTypeRep (Proxy @a) of
              SomeTypeRep tr -> (show tr ++ ": " ++ (tyConModule $ typeRepTyCon tr))

main = do
  -- These are in GHC.Types
  putStrLn $ moduleOf @Levity
  putStrLn $ moduleOf @'Lifted
  putStrLn $ moduleOf @RuntimeRep
  putStrLn $ moduleOf @'IntRep
  putStrLn $ moduleOf @'BoxedRep
  putStrLn $ moduleOf @'Lifted
  putStrLn $ moduleOf @VecCount
  putStrLn $ moduleOf @'Vec2
  putStrLn $ moduleOf @VecElem
  putStrLn $ moduleOf @'Int8ElemRep

  -- This is from GHC.Tuple
  putStrLn $ moduleOf @((),())

  -- These are in GHC.Prim
  putStrLn $ moduleOf @(# () , () #)
--  putStrLn $ moduleOf @(# () | () #)
--
  putStrLn $ moduleOf @(Int64#)
  putStrLn $ moduleOf @(Word64#)
  putStrLn $ moduleOf @TYPE
  putStrLn $ moduleOf @CONSTRAINT
