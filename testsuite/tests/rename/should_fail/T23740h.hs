{-# LANGUAGE RequiredTypeArguments #-}

module T23740h where

class C a where
    f :: a -> Bool

instance C (Maybe id) where
    f (Just _) = True
    f Nothing = False