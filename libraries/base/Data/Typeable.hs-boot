{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Typeable (Typeable, mkTyConApp, cast) where

import Data.Maybe
import {-# SOURCE #-} Data.Typeable.Internal

cast :: (Typeable a, Typeable b) => a -> Maybe b

