{-# LANGUAGE GADTs, TypeFamilies #-}
module T10562 where

type family Flip a

data QueryRep qtyp a where
    QAtom :: a -> QueryRep () a
    QOp   :: QueryRep (Flip qtyp) a -> QueryRep qtyp a

instance Eq (QueryRep qtyp a) where
  (==) = error "urk"

instance (Ord a) => Ord (QueryRep qtyp a) where
  compare (QOp a) (QOp b) = a `compare` b
