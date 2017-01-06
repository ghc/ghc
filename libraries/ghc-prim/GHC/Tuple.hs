{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude, DeriveGeneric #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Tuple
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/ghc-prim/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (GHC extensions)
--
-- The tuple data types
--
-----------------------------------------------------------------------------

module GHC.Tuple where

import GHC.CString ()  -- Make sure we do it first, so that the
                       -- implicit Typeable stuff can see GHC.Types.TyCon
                       -- and unpackCString# etc

default () -- Double and Integer aren't available yet

-- | The unit datatype @()@ has one non-undefined member, the nullary
-- constructor @()@.
data () = ()

-- The desugarer uses 1-tuples,
-- but "()" is already used up for 0-tuples
-- See Note [One-tuples] in TysWiredIn
data Unit a = Unit a

data (a,b) = (a,b)
data (a,b,c) = (a,b,c)
data (a,b,c,d) = (a,b,c,d)
data (a,b,c,d,e) = (a,b,c,d,e)
data (a,b,c,d,e,f) = (a,b,c,d,e,f)
data (a,b,c,d,e,f,g) = (a,b,c,d,e,f,g)
data (a,b,c,d,e,f,g,h) = (a,b,c,d,e,f,g,h)
data (a,b,c,d,e,f,g,h,i) = (a,b,c,d,e,f,g,h,i)
data (a,b,c,d,e,f,g,h,i,j) = (a,b,c,d,e,f,g,h,i,j)
data (a,b,c,d,e,f,g,h,i,j,k) = (a,b,c,d,e,f,g,h,i,j,k)
data (a,b,c,d,e,f,g,h,i,j,k,l) = (a,b,c,d,e,f,g,h,i,j,k,l)
data (a,b,c,d,e,f,g,h,i,j,k,l,m) = (a,b,c,d,e,f,g,h,i,j,k,l,m)
data (a,b,c,d,e,f,g,h,i,j,k,l,m,n) = (a,b,c,d,e,f,g,h,i,j,k,l,m,n)
data (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o)

-- XXX Larger tuples are in base:GHC.LargeTuple
