{-# LANGUAGE DataKinds #-}

module Main where

import GHC.TypeError

class C a where
  meth1 :: a -> Bool
  meth2 :: a -> Bool

  meth1 = not . meth2
  meth2 = not . meth1
  {-# MINIMAL meth1 | meth2 #-}

instance Unsatisfiable (Text "Msg") => C a

main :: IO ()
main = print (meth1 'x')
