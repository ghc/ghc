{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE PatternSynonyms #-}

-- | A state monad which is strict in its state.
module GHC.Utils.Monad.State.Strict
  ( -- * The State monad
    State(State)
  , state
  , evalState
  , execState
  , runState
    -- * Operations
  , get
  , gets
  , put
  , modify
  ) where

import GHC.Prelude

import GHC.Exts (oneShot)

{- Note [Strict State monad]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A State monad can be strict in many ways. Which kind of strictness do we mean?

First of, since we represent the result pair as an unboxed pair, this State
monad is strict in the sense of "Control.Monad.Trans.State.Strict": The
computations and the sequencing there-of (through 'Applicative and 'Monad'
instances) are forced strictly.

Beyond the manual unboxing of one level (which CPR could achieve similarly,
yet perhaps a bit less reliably), our 'State' is even stricter than the
transformers version:
It's also strict in the state `s` (but still lazy in the value `a`). What this
means is that whenever callers examine the state component (perhaps through
'runState'), they will find that the `s` has already been evaluated.

This additional strictness maintained in a single place, by the ubiquitous
'State' pattern synonym, by forcing the state component *after* any state action
has been run. The INVARIANT is:

> Any `s` that makes it into the unboxed pair representation is evaluated.

This invariant has another nice effect: Because the evaluatedness is quite
apparent, Nested CPR will try to unbox the state component `s` nestedly if
feasible. Detecting evaluatedness of nested components is a necessary
condition for Nested CPR to trigger; see the user's guide entry on that:
https://ghc.gitlab.haskell.org/ghc/doc/users_guide/using-optimisation.html#ghc-flag--fcpr-anal

Note that this doesn't have any effects on whether Nested CPR will unbox the `a`
component (which is still lazy by default). The user still has to use the
`return $!` idiom from the user's guide to encourage Nested CPR to unbox the `a`
result of a stateful computation.
-}

-- | A state monad which is strict in the state `s`, but lazy in the value `a`.
--
-- See Note [Strict State monad] for the particular notion of strictness and
-- implementation details.
newtype State s a = State' { runState' :: s -> (# a, s #) }

pattern State :: (s -> (# a, s #))
              -> State s a

-- This pattern synonym makes the monad eta-expand,
-- which as a very beneficial effect on compiler performance
-- See #18202.
-- See Note [The one-shot state monad trick] in GHC.Utils.Monad
-- It also implements the particular notion of strictness of this monad;
-- see Note [Strict State monad].
pattern State m <- State' m
  where
    State m = State' (oneShot $ \s -> forceState (m s))

-- | Forces the state component of the unboxed representation pair of 'State'.
-- See Note [Strict State monad]. This is The Place doing the forcing!
forceState :: (# a, s #) -> (# a, s #)
forceState (# a, !s #) = (# a, s #)

instance Functor (State s) where
  fmap f m = State $ \s -> case runState' m s  of (# x, s' #) -> (# f x, s' #)

instance Applicative (State s) where
  pure x  = State $ \s -> (# x, s #)
  m <*> n = State $ \s ->
    case runState' m s  of { (# f, s' #) ->
    case runState' n s' of { (# x, s'' #) ->
                             (# f x, s'' #) }}

instance Monad (State s) where
  m >>= n = State $ \s -> case runState' m s of
    (# r, !s' #) -> runState' (n r) s'

state :: (s -> (a, s)) -> State s a
state f = State $ \s -> case f s of (r, s') -> (# r, s' #)

get :: State s s
get = State $ \s -> (# s, s #)

gets :: (s -> a) -> State s a
gets f = State $ \s -> (# f s, s #)

put :: s -> State s ()
put s' = State $ \_ -> (# (), s' #)

modify :: (s -> s) -> State s ()
modify f = State $ \s -> (# (), f s #)

evalState :: State s a -> s -> a
evalState s i = case runState' s i of (# a, _ #) -> a

execState :: State s a -> s -> s
execState s i = case runState' s i of (# _, s' #) -> s'

runState :: State s a -> s -> (a, s)
runState s i = case runState' s i of (# a, !s' #) -> (a, s')
