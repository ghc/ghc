{- This example illustrates a nasty case of "vacuous" abstraction
   It comes from Cabal library Distribution.Simple.Utils

Consider this

  type HCS = (?callStack :: CallStack)
  wcs :: forall a. (HCS => a) -> a
  foo :: Int
  ($) :: forall p q. (p->q) -> p -> q

The call:      wcs $ foo

From QL on the first arg of $ we instantiate wcs with a:=kappa.  Then
we can work out what p and q must instantiate to.  (the (p->q) arg of
($) is guarded): get p := (HCS => kappa), q := kappa

But alas, the second arg of $, namely foo, satisfies our
fiv(rho_r)=empty condition.  (Here rho_r is Int.)  So we try to mgu(
HCS => kappa, Int ), and fail.

The basic problem is this: we said in 5.4 of the Quick Look paper we
didn’t about vacuous type abstraction, but we clearly do care about
type-class abstraction.

How does this work in GHC today, with the built-in rule?  It works
because we are order-dependent: we look at the first argument first.

The same would work here.  If we applied the QL substitution as we go,
by the time we got to the second argument, the expected type would
look like (HCS => kappa), and we would abandon QL on it (App-lightning
only applies to rho).  But the price is order-dependence.
-}

module CabalEx where

import GHC.Stack( withFrozenCallStack )

-- withFrozenCallStack :: HasCallStack
--                     => ( HasCallStack => a )
--                     -> a

printRawCommandAndArgsAndEnv :: Int -> IO ()
printRawCommandAndArgsAndEnv = error "urk"

printRawCommandAndArgs :: Int -> IO ()
printRawCommandAndArgs verbosity
  = withFrozenCallStack $ printRawCommandAndArgsAndEnv verbosity
