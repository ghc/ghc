{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE ExistentialQuantification #-}

module T27386 ( resVar ) where

data ResultVar b = forall a. ResultVar (a -> b) (Maybe a)

mkResultVar :: Maybe a -> ResultVar a
mkResultVar = ResultVar id

resVar :: ResultVar a
resVar = mkResultVar Nothing
