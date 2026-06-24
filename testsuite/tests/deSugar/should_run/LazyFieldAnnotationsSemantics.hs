{-# LANGUAGE LazyFieldAnnotations #-}

-- | LazyFieldAnnotations does not change the default strictness of fields.
-- Without StrictData an unannotated field stays lazy and a ~ field is lazy
-- too; a ! field is still strict.
module Main where

import Control.Exception (try, evaluate, SomeException)

data T = Plain  Int   -- unannotated: lazy
       | Lazy   ~Int   -- explicit ~: lazy
       | Strict !Int   -- explicit !: strict

-- | Does building the constructor force the field?
forcesField :: T -> IO Bool
forcesField x = do
  r <- try (evaluate x) :: IO (Either SomeException T)
  pure (either (const True) (const False) r)

main :: IO ()
main = do
  print =<< forcesField (Plain  undefined)  -- False: lazy
  print =<< forcesField (Lazy   undefined)  -- False: lazy
  print =<< forcesField (Strict undefined)  -- True:  strict
