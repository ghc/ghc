{-# LANGUAGE DefaultSignatures, FlexibleContexts, DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module T7437 where

import GHC.Generics

class GPut f where
    gput :: f a -> [()]

class Put a where
    put :: a -> [()]

    default put :: (Generic t, GPut (Rep t)) => t -> [()]
    put = gput . from
