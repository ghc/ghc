{-# LANGUAGE DataKinds, UnicodeSyntax #-}
{-# OPTIONS -Wno-star-is-type #-}

import Data.Typeable
import GHC.Types

-- Test that Typeable works for various wired-in types.
-- See, for instance, #11120.

main :: IO ()
main = do
  print $ typeOf "hello world"
  print $ typeOf '4'
  print $ typeOf (42 :: Int)
  print $ typeOf (42 :: Word)
  print $ typeOf (3.1415 :: Double)
  print $ typeOf (return () :: IO ())
  print $ typeOf ('a', 1::Int, "hello")
  print $ typeOf (typeOf "hi")
  print $ typeOf True
  print $ typeOf EQ
  print $ typeOf (id :: Int -> Int)

  print $ typeOf (Proxy :: Proxy (Eq Int))
  print $ typeOf (Proxy :: Proxy (Int, Int))
  print $ typeOf (Proxy :: Proxy "hello world")
  print $ typeOf (Proxy :: Proxy 1)
  print $ typeOf (Proxy :: Proxy [1,2,3])
  print $ typeOf (Proxy :: Proxy 'EQ)
  print $ typeOf (Proxy :: Proxy TYPE)
  print $ typeOf (Proxy :: Proxy (TYPE 'LiftedRep))
  print $ typeOf (Proxy :: Proxy *)
  print $ typeOf (Proxy :: Proxy ★)
  print $ typeOf (Proxy :: Proxy 'LiftedRep)
  print $ typeOf (Proxy :: Proxy '(1, "hello"))
  print $ typeOf (Proxy :: Proxy (~~))
