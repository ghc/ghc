{-# LANGUAGE RankNTypes #-}

module T21265 (extractorProduct') where

class GSerialiseProduct f where
  dummy :: f x -> ()
  productExtractor :: TransFusion [] ((->) Bool) (f Int)

extractorProduct' :: GSerialiseProduct f => Maybe (f Int)
extractorProduct' = unTransFusion productExtractor go

go :: f x -> Maybe (g x)
go _ = Nothing

newtype TransFusion f g a = TransFusion { unTransFusion :: forall h. Applicative h => (forall x. f x -> h (g x)) -> h a }
