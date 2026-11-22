{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE NoStarIsType, PolyKinds #-}
module CuskWarningNoTypeImport where

data Hidden (a :: k) = MkHidden
