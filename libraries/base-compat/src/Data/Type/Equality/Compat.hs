{-# LANGUAGE CPP, NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
module Data.Type.Equality.Compat (
#if MIN_VERSION_base(4,7,0)
  module Base
#endif
) where

#if MIN_VERSION_base(4,7,0)
import Data.Type.Equality as Base
#endif
