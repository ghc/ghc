{-# LANGUAGE Haskell2010 #-}
module A where

data HsExpr i

instance Eq i => Eq (HsExpr i)

