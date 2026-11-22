{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE NoStarIsType, PolyKinds, TypeFamilies #-}
module CuskWarningNoTypeImport where

data Hidden (a :: k) = MkHidden

type family Empty :: a
