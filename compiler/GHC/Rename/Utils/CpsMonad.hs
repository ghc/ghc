{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveFunctor #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns   #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

module GHC.Rename.Utils.CpsMonad
  ( -- CpsRn monad
    CpsT(..)
  , CpsRn
  , runCps
  , liftCps
  , liftCpsFV
  , wrapSrcSpanCps
  , lookupConCps
  ) where

import GHC.Prelude

import GHC.Tc.Utils.Monad
import GHC.Rename.Env
import GHC.Types.Name
import GHC.Types.Name.Set
import GHC.Types.Name.Reader
import GHC.Types.SrcLoc

import Control.Monad       ( ap )

{-
*********************************************************
*                                                      *
        The CpsRn Monad
*                                                      *
*********************************************************

Note [CpsRn monad]
~~~~~~~~~~~~~~~~~~
The CpsRn monad uses continuation-passing style to support this
style of programming:

        do { ...
           ; ns <- bindNames rs
           ; ...blah... }

   where rs::[RdrName], ns::[Name]

The idea is that '...blah...'
  a) sees the bindings of ns
  b) returns the free variables it mentions
     so that bindNames can report unused ones

In particular,
    mapM rnPatAndThen [p1, p2, p3]
has a *left-to-right* scoping: it makes the binders in
p1 scope over p2,p3.
-}

type CpsRn = CpsT RnM

newtype CpsT m b = CpsT
  { unCpsRn :: forall r
            .  (b -> m (r, FreeVars))
            -> m (r, FreeVars)
  } deriving (Functor)
        -- See Note [CpsRn monad]

instance Applicative (CpsT m) where
    pure x = CpsT $ \k -> k x
    (<*>) = ap

instance Monad (CpsT m) where
  CpsT m >>= mk = CpsT $ \k -> m (\v -> unCpsRn (mk v) k)

runCps :: CpsRn a -> RnM (a, FreeVars)
runCps (CpsT m) = m $ \r -> return (r, emptyFVs)

liftCps :: RnM a -> CpsRn a
liftCps rn_thing = CpsT $ \k -> rn_thing >>= k

liftCpsFV :: RnM (a, FreeVars) -> CpsRn a
liftCpsFV rn_thing = CpsT $ \k -> do
  (v,fvs1) <- rn_thing
  (r,fvs2) <- k v
  return (r, fvs1 `plusFV` fvs2)

wrapSrcSpanCps :: (a -> CpsRn b) -> Located a -> CpsRn (Located b)
-- Set the location, and also wrap it around the value returned
wrapSrcSpanCps fn (L loc a) = CpsT $ \k ->
  setSrcSpan loc $ unCpsRn (fn a) $ \v -> k (L loc v)

lookupConCps :: Located RdrName -> CpsRn (Located Name)
lookupConCps con_rdr = CpsT $ \k -> do
   con_name <- lookupLocatedOccRn con_rdr
   (r, fvs) <- k con_name
   return (r, addOneFV fvs $ unLoc con_name)
    -- We add the constructor name to the free vars
    -- See Note [Patterns are uses]

{-
Note [Patterns are uses]
~~~~~~~~~~~~~~~~~~~~~~~~
Consider
  module Foo( f, g ) where
  data T = T1 | T2

  f T1 = True
  f T2 = False

  g _ = T1

Arguably we should report T2 as unused, even though it appears in a
pattern, because it never occurs in a constructed position.  See
#7336.
However, implementing this in the face of pattern synonyms would be
less straightforward, since given two pattern synonyms

  pattern P1 <- P2
  pattern P2 <- ()

we need to observe the dependency between P1 and P2 so that type
checking can be done in the correct order (just like for value
bindings). Dependencies between bindings is analyzed in the renamer,
where we don't know yet whether P2 is a constructor or a pattern
synonym. So for now, we do report conid occurrences in patterns as
uses.
-}
