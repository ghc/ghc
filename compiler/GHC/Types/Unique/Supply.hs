{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998
-}

{-# OPTIONS_GHC -fno-state-hack #-}
    -- This -fno-state-hack is important
    -- See Note [Optimising the unique supply]

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE BangPatterns #-}

#if !defined(GHC_LOADED_INTO_GHCI)
{-# LANGUAGE UnboxedTuples #-}
#endif

module GHC.Types.Unique.Supply (
        -- * Main data type
        UniqSupply, -- Abstractly

        -- ** Operations on supplies
        uniqFromSupply, uniqsFromSupply, -- basic ops
        takeUniqFromSupply, uniqFromMask,

        mkSplitUniqSupply,
        splitUniqSupply, listSplitUniqSupply,

        -- * Unique supply monad and its abstraction
        UniqSM, MonadUnique(..),

        -- ** Operations on the monad
        initUs, initUs_,

        -- * Set supply strategy
        initUniqSupply
  ) where

import GHC.Prelude

import GHC.Types.Unique
import GHC.Utils.Panic.Plain (panic)

import GHC.IO

import GHC.Utils.Monad
import Control.Monad
import Data.Bits
import Data.Char

#include "Unique.h"

{-
************************************************************************
*                                                                      *
\subsection{Splittable Unique supply: @UniqSupply@}
*                                                                      *
************************************************************************
-}

{- Note [How the unique supply works]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The basic idea (due to Lennart Augustsson) is that a UniqSupply is
lazily-evaluated infinite tree.

* At each MkSplitUniqSupply node is a unique Int, and two
  sub-trees (see data UniqSupply)

* takeUniqFromSupply :: UniqSupply -> (Unique, UniqSupply)
  returns the unique Int and one of the sub-trees

* splitUniqSupply :: UniqSupply -> (UniqSupply, UniqSupply)
  returns the two sub-trees

* When you poke on one of the thunks, it does a foreign call
  to get a fresh Int from a thread-safe counter, and returns
  a fresh MkSplitUniqSupply node.  This has to be as efficient
  as possible: it should allocate only
     * The fresh node
     * A thunk for each sub-tree

Note [Optimising the unique supply]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The inner loop of mkSplitUniqSupply is a function closure

     mk_supply :: IO UniqSupply
     mk_supply = unsafeInterleaveIO $
                 genSym      >>= \ u ->
                 mk_supply   >>= \ s1 ->
                 mk_supply   >>= \ s2 ->
                 return (MkSplitUniqSupply (mask .|. u) s1 s2)

It's a classic example of an IO action that is captured
and the called repeatedly (see #18238 for some discussion).
It turns out that we can get something like

  $wmkSplitUniqSupply c# s
    = letrec
        mk_supply
          = \s -> unsafeDupableInterleaveIO1
                    (\s2 -> case noDuplicate# s2 of s3 ->
                            ...
                            case mk_supply s4 of (# s5, t1 #) ->
                            ...
                            (# s6, MkSplitUniqSupply ... #)
      in mk_supply s

This is bad becuase we allocate that inner (\s2...) every time.
Why doesn't full laziness float out the (\s2...)?  Because of
the state hack (#18238).

So for this module we switch the state hack off -- it's an example
of when it makes things worse rather than better. Now full laziness
can float that lambda out, and we get

  $wmkSplitUniqSupply c# s
    = letrec
        lvl = \s2 -> case noDuplicate# s2 of s3 ->
                     ...
                     case unsafeDupableInterleaveIO
                              lvl s4 of (# s5, t1 #) ->
                     ...
                     (# s6, MkSplitUniqSupply ... #)
      in unsafeDupableInterleaveIO1 lvl s

This is all terribly delicate.  It just so happened that before I
fixed #18078, and even with the state-hack still enabled, we were
getting this:

  $wmkSplitUniqSupply c# s
    = letrec
        mk_supply = \s2 -> case noDuplicate# s2 of s3 ->
                           ...
                           case mks_help s3 of (# s5,t1 #) ->
                           ...
                           (# s6, MkSplitUniqSupply ... #)
        mks_help = unsafeDupableInterleaveIO mk_supply
           -- mks_help marked as loop breaker
      in mks_help s

The fact that we didn't need full laziness was somewhat fortuitious.
We got the right number of allocations. But the partial application of
the arity-2 unsafeDupableInterleaveIO in mks_help makes it quite a
bit slower.  (Test perf/should_run/UniqLoop had a 20% perf change.)

Sigh.  The test perf/should_run/UniqLoop keeps track of this loop.
Watch it carefully.
-}


-- | Unique Supply
--
-- A value of type 'UniqSupply' is unique, and it can
-- supply /one/ distinct 'Unique'.  Also, from the supply, one can
-- also manufacture an arbitrary number of further 'UniqueSupply' values,
-- which will be distinct from the first and from all others.
data UniqSupply
  = MkSplitUniqSupply {-# UNPACK #-} !Int -- make the Unique with this
                   UniqSupply UniqSupply
                                -- when split => these two supplies

mkSplitUniqSupply :: Char -> IO UniqSupply
-- ^ Create a unique supply out of thin air. The character given must
-- be distinct from those of all calls to this function in the compiler
-- for the values generated to be truly unique.

-- See Note [How the unique supply works]
-- See Note [Optimising the unique supply]
mkSplitUniqSupply c
  = mk_supply
  where
     !mask = ord c `shiftL` uNIQUE_BITS

        -- Here comes THE MAGIC: see Note [How the unique supply works]
        -- This is one of the most hammered bits in the whole compiler
        -- See Note [Optimising the unique supply]
        -- NB: Use unsafeInterleaveIO for thread-safety.
     mk_supply = unsafeInterleaveIO $
                 genSym      >>= \ u ->
                 mk_supply   >>= \ s1 ->
                 mk_supply   >>= \ s2 ->
                 return (MkSplitUniqSupply (mask .|. u) s1 s2)

foreign import ccall unsafe "genSym" genSym :: IO Int
foreign import ccall unsafe "initGenSym" initUniqSupply :: Int -> Int -> IO ()

splitUniqSupply :: UniqSupply -> (UniqSupply, UniqSupply)
-- ^ Build two 'UniqSupply' from a single one, each of which
-- can supply its own 'Unique'.
listSplitUniqSupply :: UniqSupply -> [UniqSupply]
-- ^ Create an infinite list of 'UniqSupply' from a single one
uniqFromSupply  :: UniqSupply -> Unique
-- ^ Obtain the 'Unique' from this particular 'UniqSupply'
uniqsFromSupply :: UniqSupply -> [Unique] -- Infinite
-- ^ Obtain an infinite list of 'Unique' that can be generated by constant splitting of the supply
takeUniqFromSupply :: UniqSupply -> (Unique, UniqSupply)
-- ^ Obtain the 'Unique' from this particular 'UniqSupply', and a new supply

splitUniqSupply (MkSplitUniqSupply _ s1 s2) = (s1, s2)
listSplitUniqSupply  (MkSplitUniqSupply _ s1 s2) = s1 : listSplitUniqSupply s2

uniqFromSupply  (MkSplitUniqSupply n _ _)  = mkUniqueGrimily n
uniqsFromSupply (MkSplitUniqSupply n _ s2) = mkUniqueGrimily n : uniqsFromSupply s2
takeUniqFromSupply (MkSplitUniqSupply n s1 _) = (mkUniqueGrimily n, s1)

uniqFromMask :: Char -> IO Unique
uniqFromMask mask
  = do { uqNum <- genSym
       ; return $! mkUnique mask uqNum }


{-
************************************************************************
*                                                                      *
\subsubsection[UniqSupply-monad]{@UniqSupply@ monad: @UniqSM@}
*                                                                      *
************************************************************************
-}

-- Avoids using unboxed tuples when loading into GHCi
#if !defined(GHC_LOADED_INTO_GHCI)

type UniqResult result = (# result, UniqSupply #)

pattern UniqResult :: a -> b -> (# a, b #)
pattern UniqResult x y = (# x, y #)
{-# COMPLETE UniqResult #-}

#else

data UniqResult result = UniqResult !result {-# UNPACK #-} !UniqSupply
  deriving (Functor)

#endif

-- | A monad which just gives the ability to obtain 'Unique's
newtype UniqSM result = USM { unUSM :: UniqSupply -> UniqResult result }
    deriving (Functor)

instance Monad UniqSM where
  (>>=) = thenUs
  (>>)  = (*>)

instance Applicative UniqSM where
    pure = returnUs
    (USM f) <*> (USM x) = USM $ \us0 -> case f us0 of
                            UniqResult ff us1 -> case x us1 of
                              UniqResult xx us2 -> UniqResult (ff xx) us2
    (*>) = thenUs_

-- TODO: try to get rid of this instance
instance MonadFail UniqSM where
    fail = panic

-- | Run the 'UniqSM' action, returning the final 'UniqSupply'
initUs :: UniqSupply -> UniqSM a -> (a, UniqSupply)
initUs init_us m = case unUSM m init_us of { UniqResult r us -> (r, us) }

-- | Run the 'UniqSM' action, discarding the final 'UniqSupply'
initUs_ :: UniqSupply -> UniqSM a -> a
initUs_ init_us m = case unUSM m init_us of { UniqResult r _ -> r }

{-# INLINE thenUs #-}
{-# INLINE returnUs #-}
{-# INLINE splitUniqSupply #-}

-- @thenUs@ is where we split the @UniqSupply@.

liftUSM :: UniqSM a -> UniqSupply -> (a, UniqSupply)
liftUSM (USM m) us0 = case m us0 of UniqResult a us1 -> (a, us1)

instance MonadFix UniqSM where
    mfix m = USM (\us0 -> let (r,us1) = liftUSM (m r) us0 in UniqResult r us1)

thenUs :: UniqSM a -> (a -> UniqSM b) -> UniqSM b
thenUs (USM expr) cont
  = USM (\us0 -> case (expr us0) of
                   UniqResult result us1 -> unUSM (cont result) us1)

thenUs_ :: UniqSM a -> UniqSM b -> UniqSM b
thenUs_ (USM expr) (USM cont)
  = USM (\us0 -> case (expr us0) of { UniqResult _ us1 -> cont us1 })

returnUs :: a -> UniqSM a
returnUs result = USM (\us -> UniqResult result us)

getUs :: UniqSM UniqSupply
getUs = USM (\us0 -> case splitUniqSupply us0 of (us1,us2) -> UniqResult us1 us2)

-- | A monad for generating unique identifiers
class Monad m => MonadUnique m where
    -- | Get a new UniqueSupply
    getUniqueSupplyM :: m UniqSupply
    -- | Get a new unique identifier
    getUniqueM  :: m Unique
    -- | Get an infinite list of new unique identifiers
    getUniquesM :: m [Unique]

    -- This default definition of getUniqueM, while correct, is not as
    -- efficient as it could be since it needlessly generates and throws away
    -- an extra Unique. For your instances consider providing an explicit
    -- definition for 'getUniqueM' which uses 'takeUniqFromSupply' directly.
    getUniqueM  = liftM uniqFromSupply  getUniqueSupplyM
    getUniquesM = liftM uniqsFromSupply getUniqueSupplyM

instance MonadUnique UniqSM where
    getUniqueSupplyM = getUs
    getUniqueM  = getUniqueUs
    getUniquesM = getUniquesUs

getUniqueUs :: UniqSM Unique
getUniqueUs = USM (\us0 -> case takeUniqFromSupply us0 of
                           (u,us1) -> UniqResult u us1)

getUniquesUs :: UniqSM [Unique]
getUniquesUs = USM (\us0 -> case splitUniqSupply us0 of
                            (us1,us2) -> UniqResult (uniqsFromSupply us1) us2)
