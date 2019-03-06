{-# LANGUAGE TypeInType, ExistentialQuantification #-}

module T16221a where

data SameKind :: k -> k -> *
data T2 a = forall k (b :: k). MkT2 (SameKind a b) !Int

