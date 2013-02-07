{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.OldTypeable (Typeable, mkTyConApp, cast) where

import Data.Maybe
import {-# SOURCE #-} Data.Typeable.Internal

cast :: (Typeable a, Typeable b) => a -> Maybe b

