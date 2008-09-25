{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleContexts #-}

module Simple24 where

linear :: HasTrie (Basis v) => (Basis v, v)
linear =  basisValue
 
class HasTrie a where
 
type family Basis u :: *
 
basisValue :: (Basis v,v)
basisValue = error "urk"
