{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StaticPointers #-}

-- | A test to try the static form in splices.
--
-- A static form is defined in a splice and then it is used in the program.
--
module Main(main) where

import GHC.Fingerprint
import GHC.StaticPtr

main = print $ $(case staticKey (static 'a') of
  Fingerprint w0 w1 ->
    let w0i = fromIntegral w0 :: Integer
        w1i = fromIntegral w1 :: Integer
    in
     [| fmap (\p -> deRefStaticPtr p :: Char) $ unsafeLookupStaticPtr $
          Fingerprint (fromIntegral w0i) (fromIntegral w1i)
      |]
     )
