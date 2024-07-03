{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UnboxedTuples #-}

module GHC.Types.Unique.Supply (
        -- * Main data type
        UniqSupply, -- Abstractly

        -- ** Operations on supplies
        uniqFromSupply, uniqsFromSupply, -- basic ops
        takeUniqFromSupply, uniqFromTag,

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
import GHC.Utils.Panic.Plain

import GHC.IO

import GHC.Utils.Monad
import Control.Monad
import Data.Word
import GHC.Exts( Ptr(..), noDuplicate#, oneShot )
import Foreign.Storable
import GHC.Utils.Monad.State.Strict as Strict

#include "MachDeps.h"

#if WORD_SIZE_IN_BITS != 64
#define NO_FETCH_ADD
#endif

#if defined(NO_FETCH_ADD)
import GHC.Exts ( atomicCasWord64Addr#, eqWord64#, readWord64OffAddr# )
#else
import GHC.Exts( fetchAddWordAddr#, word64ToWord# )
#endif

import GHC.Exts ( Addr#, State#, Word64#, RealWorld )
import GHC.Int ( Int(..) )
import GHC.Word( Word64(..) )
import GHC.Exts( plusWord64#, int2Word#, wordToWord64# )

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

* At each MkSplitUniqSupply node is a unique Word64, and two
  sub-trees (see data UniqSupply)

* takeUniqFromSupply :: UniqSupply -> (Unique, UniqSupply)
  returns the unique Word64 and one of the sub-trees

* splitUniqSupply :: UniqSupply -> (UniqSupply, UniqSupply)
  returns the two sub-trees

* When you poke on one of the thunks, it does a foreign call
  to get a fresh Word64 from a thread-safe counter, and returns
  a fresh MkSplitUniqSupply node.  This has to be as efficient
  as possible: it should allocate only
     * The fresh node
     * A thunk for each sub-tree

Note [How unique supplies are used]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The general design (used throughout GHC) is to:

* For creating new uniques either a UniqSupply is used and threaded through
  or for monadic code a MonadUnique instance might conjure up uniques using
  `uniqFromTag`.
* Different parts of the compiler will use a UniqSupply or MonadUnique instance
  with a specific tag. This way the different parts of the compiler will
  generate uniques with different tags.

If different code shares the same tag then care has to be taken that all uniques
still get distinct numbers. Usually this is done by relying on genSym which
has *one* counter per GHC invocation that is relied on by all calls to it.
But using something like the address for pinned objects works as well and in fact is done
for fast strings.

This is important for example in the simplifier. Most passes of the simplifier use
the same tag 's'. However in some places we create a unique supply using `mkSplitUniqSupply`
and thread it through the code, while in GHC.Core.Opt.Simplify.Monad  we use the
`instance MonadUnique SimplM`, which uses `mkSplitUniqSupply` in getUniqueSupplyM
and `uniqFromTag` in getUniqueM.

Ultimately all these boil down to each new unique consisting of the tag and the result from
a call to `genSym`. The latter producing a distinct number for each invocation ensuring
uniques are distinct.

Note [Optimising the unique supply]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The inner loop of mkSplitUniqSupply is a function closure

     mk_supply s0 =
        case noDuplicate# s0 of { s1 ->
        case unIO genSym s1 of { (# s2, u #) ->
        case unIO (unsafeDupableInterleaveIO (IO mk_supply)) s2 of { (# s3, x #) ->
        case unIO (unsafeDupableInterleaveIO (IO mk_supply)) s3 of { (# s4, y #) ->
        (# s4, MkSplitUniqSupply (tag .|. u) x y #)
        }}}}

It's a classic example of an IO action that is captured and then called
repeatedly (see #18238 for some discussion). It mustn't allocate!  The test
perf/should_run/UniqLoop keeps track of this loop.  Watch it carefully.

We used to write it as:

     mk_supply :: IO UniqSupply
     mk_supply = unsafeInterleaveIO $
                 genSym      >>= \ u ->
                 mk_supply   >>= \ s1 ->
                 mk_supply   >>= \ s2 ->
                 return (MkSplitUniqSupply (tag .|. u) s1 s2)

and to rely on -fno-state-hack, full laziness and inlining to get the same
result. It was very brittle and required enabling -fno-state-hack globally. So
it has been rewritten using lower level constructs to explicitly state what we
want.

Note [Optimising use of unique supplies]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When it comes to having a way to generate new Uniques
there are generally three ways to deal with this:

For pure code the only good approach is to take an UniqSupply
as argument. Then  thread it through the code splitting it
for sub-passes or when creating uniques.
The code for this is about as optimized as it gets, but we can't
get around the need to allocate one `UniqSupply` for each Unique
we need.

For code in IO we can improve on this by threading only the *tag*
we are going to use for Uniques. Using `uniqFromTag` to
generate uniques as needed. This gets rid of the overhead of
allocating a new UniqSupply for each unique generated. It also avoids
frequent state updates when the Unique/Tag is part of the state in a
state monad.

For monadic code in IO which always uses the same tag we can go further
and hardcode the tag into the MonadUnique instance. On top of all the
benefits of threading the tag this *also* has the benefit of avoiding
the tag getting captured in thunks, or being passed around at runtime.
It does however come at the cost of having to use a fixed tag for all
code run in this Monad. But remember, the tag is purely cosmetic:
See Note [Uniques and tags].

NB: It's *not* an optimization to pass around the UniqSupply inside an
IORef instead of the tag. While this would avoid frequent state updates
it still requires allocating one UniqSupply per Unique. On top of some
overhead for reading/writing to/from the IORef.

All of this hinges on the assumption that UniqSupply and
uniqFromTag use the same source of distinct numbers (`genSym`) which
allows both to be used at the same time, with the same tag, while still
ensuring distinct uniques.
One might consider this fact to be an "accident". But GHC worked like this
as far back as source control history goes. It also allows the later two
optimizations to be used. So it seems safe to depend on this fact.

-}


-- | Unique Supply
--
-- A value of type 'UniqSupply' is unique, and it can
-- supply /one/ distinct 'Unique'.  Also, from the supply, one can
-- also manufacture an arbitrary number of further 'UniqueSupply' values,
-- which will be distinct from the first and from all others.
data UniqSupply
  = MkSplitUniqSupply {-# UNPACK #-} !Word64 -- make the Unique with this
                   UniqSupply UniqSupply
                                -- when split => these two supplies

mkSplitUniqSupply :: Char -> IO UniqSupply
-- ^ Create a unique supply out of thin air.
-- The "tag" (Char) supplied is purely cosmetic, making it easier
-- to figure out where a Unique was born. See
-- Note [Uniques and tags].
--
-- The payload part of the Uniques allocated from this UniqSupply are
-- guaranteed distinct wrt all other supplies, regardless of their "tag".
-- This is achieved by allocating the payload part from
-- a single source of Uniques, namely `genSym`, shared across
-- all UniqSupply's.

-- See Note [How the unique supply works]
-- See Note [Optimising the unique supply]
mkSplitUniqSupply c
  = unsafeDupableInterleaveIO (IO mk_supply)

  where
     !tag = mkTag c

        -- Here comes THE MAGIC: see Note [How the unique supply works]
        -- This is one of the most hammered bits in the whole compiler
        -- See Note [Optimising the unique supply]
        -- NB: Use noDuplicate# for thread-safety.
     mk_supply s0 =
        case noDuplicate# s0 of { s1 ->
        case unIO genSym s1 of { (# s2, u #) ->
        -- deferred IO computations
        case unIO (unsafeDupableInterleaveIO (IO mk_supply)) s2 of { (# s3, x #) ->
        case unIO (unsafeDupableInterleaveIO (IO mk_supply)) s3 of { (# s4, y #) ->
        (# s4, MkSplitUniqSupply (tag .|. u) x y #)
        }}}}

#if defined(NO_FETCH_ADD)
-- GHC currently does not provide this operation on 32-bit platforms,
-- hence the CAS-based implementation.
fetchAddWord64Addr# :: Addr# -> Word64# -> State# RealWorld
                    -> (# State# RealWorld, Word64# #)
fetchAddWord64Addr# = go
  where
    go ptr inc s0 =
      case readWord64OffAddr# ptr 0# s0 of
        (# s1, n0 #) ->
          case atomicCasWord64Addr# ptr n0 (n0 `plusWord64#` inc) s1 of
            (# s2, res #)
              | 1# <- res `eqWord64#` n0 -> (# s2, n0 #)
              | otherwise -> go ptr inc s2
#else
fetchAddWord64Addr# :: Addr# -> Word64# -> State# RealWorld
                    -> (# State# RealWorld, Word64# #)
fetchAddWord64Addr# addr inc s0 =
    case fetchAddWordAddr# addr (word64ToWord# inc) s0 of
      (# s1, res #) -> (# s1, wordToWord64# res #)
#endif

genSym :: IO Word64
genSym = do
    let !mask = (1 `unsafeShiftL` uNIQUE_BITS) - 1
    let !(Ptr counter) = ghc_unique_counter64
    I# inc# <- peek ghc_unique_inc
    let !inc = wordToWord64# (int2Word# inc#)
    u <- IO $ \s1 -> case fetchAddWord64Addr# counter inc s1 of
            (# s2, val #) ->
                let !u = W64# (val `plusWord64#` inc) .&. mask
                in (# s2, u #)
#if defined(DEBUG)
    -- Uh oh! We will overflow next time a unique is requested.
    -- (Note that if the increment isn't 1 we may miss this check)
    massert (u /= mask)
#endif
    return u

foreign import ccall unsafe "&ghc_unique_counter64" ghc_unique_counter64 :: Ptr Word64
foreign import ccall unsafe "&ghc_unique_inc"       ghc_unique_inc       :: Ptr Int

initUniqSupply :: Word64 -> Int -> IO ()
initUniqSupply counter inc = do
    poke ghc_unique_counter64 counter
    poke ghc_unique_inc       inc

uniqFromTag :: Char -> IO Unique
uniqFromTag !tag
  = do { uqNum <- genSym
       ; return $! mkUnique tag uqNum }
{-# NOINLINE uniqFromTag #-} -- We'll unbox everything, but we don't want to inline it

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

{-# INLINE splitUniqSupply #-}

{-
************************************************************************
*                                                                      *
\subsubsection[UniqSupply-monad]{@UniqSupply@ monad: @UniqSM@}
*                                                                      *
************************************************************************
-}

type UniqResult result = (# result, UniqSupply #)

pattern UniqResult :: a -> b -> (# a, b #)
pattern UniqResult x y = (# x, y #)
{-# COMPLETE UniqResult #-}

-- | A monad which just gives the ability to obtain 'Unique's
newtype UniqSM result = USM { unUSM :: UniqSupply -> UniqResult result }
  deriving (Functor, Applicative, Monad) via (Strict.State UniqSupply)

-- | Smart constructor for 'UniqSM', as described in Note [The one-shot state
-- monad trick].
mkUniqSM :: (UniqSupply -> UniqResult a) -> UniqSM a
mkUniqSM f = USM (oneShot f)
{-# INLINE mkUniqSM #-}

-- TODO: try to get rid of this instance
instance MonadFail UniqSM where
    fail = panic

-- | Run the 'UniqSM' action, returning the final 'UniqSupply'
initUs :: UniqSupply -> UniqSM a -> (a, UniqSupply)
initUs init_us m = case unUSM m init_us of { UniqResult r us -> (r, us) }

-- | Run the 'UniqSM' action, discarding the final 'UniqSupply'
initUs_ :: UniqSupply -> UniqSM a -> a
initUs_ init_us m = case unUSM m init_us of { UniqResult r _ -> r }

liftUSM :: UniqSM a -> UniqSupply -> (a, UniqSupply)
liftUSM (USM m) us0 = case m us0 of UniqResult a us1 -> (a, us1)

instance MonadFix UniqSM where
    mfix m = mkUniqSM (\us0 -> let (r,us1) = liftUSM (m r) us0 in UniqResult r us1)

getUs :: UniqSM UniqSupply
getUs = mkUniqSM (\us0 -> case splitUniqSupply us0 of (us1,us2) -> UniqResult us1 us2)

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
getUniqueUs = mkUniqSM (\us0 -> case takeUniqFromSupply us0 of
                           (u,us1) -> UniqResult u us1)

getUniquesUs :: UniqSM [Unique]
getUniquesUs = mkUniqSM (\us0 -> case splitUniqSupply us0 of
                            (us1,us2) -> UniqResult (uniqsFromSupply us1) us2)
