{-# LANGUAGE CPP, NoImplicitPrelude, PackageImports #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE ExplicitNamespaces #-}
#endif
module Data.Type.Equality.Compat (
  -- * The equality types
  (:~:)(..),
#if __GLASGOW_HASKELL__ >= 800
  type (~~),
#endif
#if __GLASGOW_HASKELL__ >= 800
  (:~~:)(..),
#endif

  -- * Working with equality
  sym, trans, castWith, gcastWith,
#if __GLASGOW_HASKELL__ >= 706
    apply,
#endif
    inner,
#if __GLASGOW_HASKELL__ >= 706
    outer,
#endif

  -- * Inferring equality from other types
  TestEquality(..),

#if __GLASGOW_HASKELL__ >= 708
  -- * Boolean type-level equality
  type (==),
#endif
) where

#if MIN_VERSION_base(4,7,0)
import "base" Data.Type.Equality
#else
import "type-equality" Data.Type.Equality
#endif

#if MIN_VERSION_base(4,9,0) && !MIN_VERSION_base(4,10,0)
import "type-equality" Data.Type.Equality.Hetero
#endif
