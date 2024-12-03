{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}

module Main where

-- Shouldn't work because we don't accept multiple occurrences of a binding variable.
foo :: Maybe String -> Maybe String -> String
foo (Nothing @a) (Nothing @a) = ("" :: a)
foo (Just @a x) (Nothing @a) = (x :: a)
foo (Nothing @a) (Just @a y) = (y :: a)
foo (Just @a x) (Just @a y) = (x ++ y :: a)

main = do
  print (foo Nothing (Just "hello"))
