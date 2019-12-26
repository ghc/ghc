{-# OPTIONS_GHC -fprof-auto-top #-}

module Main where

data D = D

class C a where
  shouldn'tSCC :: a -> ()

instance C D where
  shouldn'tSCC D = ()
  {-# INLINE shouldn'tSCC #-}

hasSCC :: D -> ()
hasSCC D = ()

hasn'tSCC :: D -> ()
hasn'tSCC D = ()
{-# INLINE hasn'tSCC #-}

main :: IO ()
main = do
  print (hasSCC D)
  print (hasn'tSCC D)
  print (shouldn'tSCC D)

