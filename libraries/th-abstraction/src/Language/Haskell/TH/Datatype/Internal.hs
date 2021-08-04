{-# LANGUAGE CPP #-}

#if MIN_VERSION_template_haskell(2,12,0)
{-# Language Safe #-}
#elif __GLASGOW_HASKELL__ >= 702
{-# Language Trustworthy #-}
#endif

{-|
Module      : Language.Haskell.TH.Datatype.Internal
Description : Backwards-compatible interface to reified information about datatypes.
Copyright   : Eric Mertens 2017
License     : ISC
Maintainer  : emertens@gmail.com

Internal Template Haskell 'Name's.

-}
module Language.Haskell.TH.Datatype.Internal where

import Language.Haskell.TH.Syntax

eqTypeName :: Name
#if MIN_VERSION_base(4,9,0) && !(MIN_VERSION_base(4,13,0))
eqTypeName = mkNameG_tc "base" "Data.Type.Equality" "~"
#else
eqTypeName = mkNameG_tc "ghc-prim" "GHC.Types" "~"
#endif

-- This is only needed for GHC 7.6-specific bug
starKindName :: Name
starKindName = mkNameG_tc "ghc-prim" "GHC.Prim" "*"
