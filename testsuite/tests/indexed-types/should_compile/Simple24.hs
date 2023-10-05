{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleContexts #-}

module Simple24 where

import Data.Kind (Type)

linear :: HasTrie (Basis v) => (Basis v, v)
linear =  basisValue
 
class HasTrie a where
 
type family Basis u :: Type
 
basisValue :: (Basis v,v)
basisValue = error "urk"
