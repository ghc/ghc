{-# LANGUAGE ExplicitNamespaces, GADTs, LinearTypes, MagicHash #-}
{-# LANGUAGE RequiredTypeArguments, TypeAbstractions, TypeApplications #-}
module Main where

import Control.Exception (ErrorCall, displayException, evaluate, try)
import Data.List (isPrefixOf)
import GHC.Exts (Int(I#))
import GadtOrderedUnboxedProvider

view :: Payload a b c %1 -> (Int, String)
view (Payload raw (type b) @c hidden) = (I# raw, show hidden)

implicit :: T4 a -> String
implicit (MkT4 x _) = show x

main :: IO ()
main = do
  print (view (Payload @Double 37# (type Bool) @Char (True :: Bool)),
         implicit (MkT4 @Double @Bool 2 False))

  let (raw, hidden) = view (Payload @Double 43# (type Bool) @Char (error "hidden" :: Bool))
  print raw
  forcedHidden <- try @ErrorCall (evaluate hidden)
  forcedRaw <- try @ErrorCall (evaluate (case error "unboxed" of
    I# n -> Payload @Double n (type Bool) @Char True))
  let raised message (Left err) = message `isPrefixOf` displayException err
      raised _ (Right _) = False
  print (raised "hidden" forcedHidden, raised "unboxed" forcedRaw)
