{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Trustworthy        #-}
{-# LANGUAGE TypeOperators      #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Generics
-- Copyright   :  (c) Universiteit Utrecht 2010-2011, University of Oxford 2012-2014
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  internal
-- Portability :  non-portable
--
-- @since 4.6.0.0
--
-- If you're using @GHC.Generics@, you should consider using the
-- <http://hackage.haskell.org/package/generic-deriving> package, which
-- contains many useful generic functions.

module GHC.Generics
  ( module GHC.Generics.Internal
  ) where

import Data.Either          ( Either(..) )
import Data.Maybe           ( Maybe(..) )
import Data.Ord             ( Down(..) )
import Data.Proxy           ( Proxy(..) )
import GHC.Base             ( Functor(..), NonEmpty(..), Void )
import GHC.Fingerprint.Type ( Fingerprint(..) )
import GHC.Ptr              ( Ptr )
import GHC.Stack.Types      ( SrcLoc(..) )
import GHC.Tuple            ( Solo (..) )
import GHC.Types
import GHC.Unicode          ( GeneralCategory(..) )

import GHC.Generics.Internal

--------------------------------------------------------------------------------
-- Derived instances for types in GHC.Generics.Internal
--------------------------------------------------------------------------------

-- | @since 4.9.0.0
deriving instance Generic (V1 p)
-- | @ since 4.9.0.0
deriving instance Generic1 V1

-- | @since 4.7.0.0
deriving instance Generic (U1 p)
-- | @ since 4.9.0.0
deriving instance Generic1 U1

-- | @since 4.7.0.0
deriving instance Generic (Par1 p)
-- | @ since 4.9.0.0
deriving instance Generic1 Par1

-- | @since 4.7.0.0
deriving instance Generic (Rec1 f p)
-- | @ since 4.9.0.0
deriving instance Generic1 (Rec1 f)

-- | @since 4.7.0.0
deriving instance Generic (K1 i c p)
-- | @ since 4.9.0.0
deriving instance Generic1 (K1 i c)

-- | @since 4.7.0.0
deriving instance Generic (M1 i c f p)
-- | @ since 4.9.0.0
deriving instance Generic1 (M1 i c f)

-- | @since 4.7.0.0
deriving instance Generic ((f :+: g) p)
-- | @ since 4.9.0.0
deriving instance Generic1 (f :+: g)

-- | @since 4.7.0.0
deriving instance Generic ((f :*: g) p)
-- | @ since 4.9.0.0
deriving instance Generic1 (f :*: g)

-- | @since 4.7.0.0
deriving instance Generic ((f :.: g) p)
-- | @ since 4.9.0.0
deriving instance Functor f => Generic1 (f :.: g)

-- | @since 4.9.0.0
deriving instance Generic (URec (Ptr ()) p)
-- | @ since 4.9.0.0
deriving instance Generic1 (URec (Ptr ()))

-- | @since 4.9.0.0
deriving instance Generic (URec Char p)
-- | @ since 4.9.0.0
deriving instance Generic1 (URec Char)

-- | @since 4.9.0.0
deriving instance Generic (URec Double p)
-- | @ since 4.9.0.0
deriving instance Generic1 (URec Double)

-- | @since 4.9.0.0
deriving instance Generic (URec Float p)
-- | @ since 4.9.0.0
deriving instance Generic1 (URec Float)

-- | @since 4.9.0.0
deriving instance Generic (URec Int p)
-- | @ since 4.9.0.0
deriving instance Generic1 (URec Int)

-- | @since 4.9.0.0
deriving instance Generic (URec Word p)
-- | @ since 4.9.0.0
deriving instance Generic1 (URec Word)

-- | @since 4.7.0.0
deriving instance Generic Fixity

-- | @since 4.7.0.0
deriving instance Generic Associativity

-- | @since 4.9.0.0
deriving instance Generic SourceUnpackedness

-- | @since 4.9.0.0
deriving instance Generic SourceStrictness

-- | @since 4.9.0.0
deriving instance Generic DecidedStrictness

--------------------------------------------------------------------------------
-- Derived instances for other types in base
--------------------------------------------------------------------------------

-- | @since 4.8.0.0
deriving instance Generic Void

-- | @since 4.6.0.0
deriving instance Generic [a]

-- | @since 4.6.0.0
deriving instance Generic (NonEmpty a)

-- | @since 4.6.0.0
deriving instance Generic (Maybe a)

-- | @since 4.6.0.0
deriving instance Generic (Either a b)

-- | @since 4.6.0.0
deriving instance Generic Bool

-- | @since 4.6.0.0
deriving instance Generic Ordering

-- | @since 4.6.0.0
deriving instance Generic (Proxy t)

-- | @since 4.6.0.0
deriving instance Generic ()

-- | @since 4.15
deriving instance Generic (Solo a)

-- | @since 4.6.0.0
deriving instance Generic ((,) a b)

-- | @since 4.6.0.0
deriving instance Generic ((,,) a b c)

-- | @since 4.6.0.0
deriving instance Generic ((,,,) a b c d)

-- | @since 4.6.0.0
deriving instance Generic ((,,,,) a b c d e)

-- | @since 4.6.0.0
deriving instance Generic ((,,,,,) a b c d e f)

-- | @since 4.6.0.0
deriving instance Generic ((,,,,,,) a b c d e f g)

-- | @since 4.16.0.0
deriving instance Generic ((,,,,,,,) a b c d e f g h)

-- | @since 4.16.0.0
deriving instance Generic ((,,,,,,,,) a b c d e f g h i)

-- | @since 4.16.0.0
deriving instance Generic ((,,,,,,,,,) a b c d e f g h i j)

-- | @since 4.16.0.0
deriving instance Generic ((,,,,,,,,,,) a b c d e f g h i j k)

-- | @since 4.16.0.0
deriving instance Generic ((,,,,,,,,,,,) a b c d e f g h i j k l)

-- | @since 4.16.0.0
deriving instance Generic ((,,,,,,,,,,,,) a b c d e f g h i j k l m)

-- | @since 4.16.0.0
deriving instance Generic ((,,,,,,,,,,,,,) a b c d e f g h i j k l m n)

-- | @since 4.16.0.0
deriving instance Generic ((,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o)

-- | @since 4.12.0.0
deriving instance Generic (Down a)

-- | @since 4.15.0.0
deriving instance Generic SrcLoc

-- | @since 4.15.0.0
deriving instance Generic GeneralCategory

-- | @since 4.15.0.0
deriving instance Generic Fingerprint

-- | @since 4.6.0.0
deriving instance Generic1 []

-- | @since 4.6.0.0
deriving instance Generic1 NonEmpty

-- | @since 4.6.0.0
deriving instance Generic1 Maybe

-- | @since 4.6.0.0
deriving instance Generic1 (Either a)

-- | @since 4.6.0.0
deriving instance Generic1 Proxy

-- | @since 4.15
deriving instance Generic1 Solo

-- | @since 4.6.0.0
deriving instance Generic1 ((,) a)

-- | @since 4.6.0.0
deriving instance Generic1 ((,,) a b)

-- | @since 4.6.0.0
deriving instance Generic1 ((,,,) a b c)

-- | @since 4.6.0.0
deriving instance Generic1 ((,,,,) a b c d)

-- | @since 4.6.0.0
deriving instance Generic1 ((,,,,,) a b c d e)

-- | @since 4.6.0.0
deriving instance Generic1 ((,,,,,,) a b c d e f)

-- | @since 4.16.0.0
deriving instance Generic1 ((,,,,,,,) a b c d e f g)

-- | @since 4.16.0.0
deriving instance Generic1 ((,,,,,,,,) a b c d e f g h)

-- | @since 4.16.0.0
deriving instance Generic1 ((,,,,,,,,,) a b c d e f g h i)

-- | @since 4.16.0.0
deriving instance Generic1 ((,,,,,,,,,,) a b c d e f g h i j)

-- | @since 4.16.0.0
deriving instance Generic1 ((,,,,,,,,,,,) a b c d e f g h i j k)

-- | @since 4.16.0.0
deriving instance Generic1 ((,,,,,,,,,,,,) a b c d e f g h i j k l)

-- | @since 4.16.0.0
deriving instance Generic1 ((,,,,,,,,,,,,,) a b c d e f g h i j k l m)

-- | @since 4.16.0.0
deriving instance Generic1 ((,,,,,,,,,,,,,,) a b c d e f g h i j k l m n)

-- | @since 4.12.0.0
deriving instance Generic1 Down
