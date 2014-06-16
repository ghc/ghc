{-# LANGUAGE  TypeFamilies, ScopedTypeVariables #-}
module T3169 where

import Prelude hiding ( lookup )

class Key k where
  type Map k :: * -> *
  lookup :: k -> Map k elt -> Maybe elt

instance (Key a, Key b) => Key (a,b) where
  type Map (a,b) = MP a b
  lookup (a,b) (m :: Map (a,b) elt) 
     = case lookup a m :: Maybe (Map b elt) of
	  Just (m2 :: Map b elt) -> lookup b m2 :: Maybe elt

data MP a b elt = MP (Map a (Map b elt))
