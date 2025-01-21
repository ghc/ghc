{-# LANGUAGE ImplicitStagePersistence #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module SI34M2 (
    makeMkT,
    TWrapper(..),
    wrapT
) where

import SI34M1
import Language.Haskell.TH.Syntax

-- A wrapper for T
data TWrapper = WrapT T
  deriving Show

-- Create a MkT with the given Int
makeMkT :: Int -> T
makeMkT = MkT

-- Wrap a T in a TWrapper
wrapT :: T -> TWrapper
wrapT = WrapT

-- Quote functions for TWrapper
instance Lift TWrapper where
  lift (WrapT t) = [| WrapT $(lift t) |]
  liftTyped (WrapT t) = [|| WrapT $$(liftTyped t) ||]
