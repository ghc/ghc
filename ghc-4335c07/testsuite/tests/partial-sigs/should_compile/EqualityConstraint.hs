{-# LANGUAGE PartialTypeSignatures, TypeFamilies #-}
module EqualityConstraint where

foo :: a ~ Bool => (a, _)
foo = (True, False)
