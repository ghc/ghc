{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude, MagicHash, StandaloneDeriving, BangPatterns,
             KindSignatures, DataKinds, ConstraintKinds,
              MultiParamTypeClasses, FunctionalDependencies #-}

{-# LANGUAGE AllowAmbiguousTypes, RoleAnnotations, IncoherentInstances #-}
  -- LANGUAGE pragmas: see Note [IP: implicit parameter class]

{-# OPTIONS_HADDOCK not-home #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Internal.Classes.IP
-- Copyright   :  (c) The University of Glasgow, 1992-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- Basic classes.
-- Do not import this module directly.  It is an GHC internal only
-- module.  Some of its contents are instead available from @Prelude@
-- and @GHC.Int@.
--
-----------------------------------------------------------------------------

module GHC.Internal.Classes.IP( IP(..)) where

import GHC.Internal.Types


default ()              -- Double isn't available yet

-- | The syntax @?x :: a@ is desugared into @IP "x" a@
-- IP is declared very early, so that libraries can take
-- advantage of the implicit-call-stack feature
type role IP nominal representational   -- See (IPRoles)
class IP (x :: Symbol) a | x -> a where
  ip :: a

{- Note [IP: implicit parameter class]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
An implicit parameter constraint (?foo::ty) is just short for

   IP "foo" ty

where ghc-internal:GHC.Internal.Classes.IP is a special class that
GHC knows about, defined in this module.

* It is a unary type class, with one method `ip`, so it has no cost.
  For example, (?foo::Int) is represented just by an Int.

* Criticially, it has a functional dependency:
    class IP (x :: Symbol) a | x -> a where ...
  So if we have
    [G] IP "foo" Int
    [W] IP "foo" alpha
  the fundep wil lgive us alpha ~ Int, as desired.

* The solver has a number of special cases for implicit parameters,
  mainly because a binding  (let ?foo::Int = rhs in body)
  is like a local instance declaration for IP.  Search for uses
  of `isIPClass`.

Wrinkles

(IPAmbiguity) The single method of IP has an ambiguous type
      ip :: forall a. IP s a => a
   Hence the LANGUAGE pragama AllowAmbiguousTypes.
   The method `ip` is never called by the user, so ambiguity doesn't matter.

(IPRoles) IP has a role annotation.  Why?  See #26737.  We want
     [W] IP "foo" t1 ~R# IP "foo" t2
  to decompose to give [W] IP t1 ~R# t2, using /representational/
  equality for (t1 ~R# t2) not nominal.

  This usually gives a complaint about incoherence, because in general
  (t1 ~R# t2) does NOT imply (C t1) ~R# (C t2) for any normal class.
  But it does for IP, because instance selection is controlled by the Symbol,
  not the type of the payload.  Hence LANGUAGE pragma IncoherentInstances.
  (It is unfortunate that we need a module-wide IncoherentInstances here;
  see #17167.)

  Side note: arguably this treatment could be applied to any class
  with a functional dependency; but for now we restrict it to IP.
-}

