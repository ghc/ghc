-- The code below is based on containers-0.8
module T22611a
  ( alterF
  ) where

import Data.Coerce (coerce)
import Data.Functor.Const (Const(..))
import Data.Functor.Identity (Identity(..))
import GHC.Exts (inline)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

alterF :: (Functor f, Ord k)
       => (Maybe a -> f (Maybe a)) -> k -> Map k a -> f (Map k a)
alterF f k m = inline Map.alterF f k m

-- alterF is INLINABLE and we expect it to be specialized at the call site.
{-# INLINABLE [2] alterF #-}

{-# RULES
"alterF/Const" forall k (f :: Maybe a -> Const b (Maybe a)) . alterF f k = \m -> Const . getConst . f $ Map.lookup k m
"alterF/Identity" forall k f . alterF f k = atKeyIdentity k f
 #-}

atKeyIdentity :: Ord k => k -> (Maybe a -> Identity (Maybe a)) -> Map k a -> Identity (Map k a)
atKeyIdentity k f t = Identity (Map.alter (coerce f) k t)
{-# INLINABLE atKeyIdentity #-}
