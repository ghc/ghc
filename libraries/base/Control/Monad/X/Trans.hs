module Control.Monad.X.Trans 
  ( -- * General transformer classes
    MonadTrans(..),
    HasBaseMonad(..),

    -- * Plumbing transformers
    -- $PlumbingDoc

    -- ** Reader
    MonadReader(..), 
    -- $MonadReaderDoc
    asks,
    localSet,

    -- ** Writer
    MonadWriter(..),
    -- $MonadWriterDoc
    listens,
    censor,
    pass,

    -- ** State
    MonadState(..),   
    -- $MonadStateDoc
    gets,
    modify,

    -- * Control transformers
    -- $ControlDoc

    -- ** Exceptions
    MonadError(..),
    -- $MonadErrorDoc

    -- ** Non-determinism
    MonadNondet(..),
    -- $MonadNondetDoc

    -- ** Resumptions
    MonadResume(..),
    -- $MonadResumeDoc

    -- ** Continuations
    MonadCont(..),
    -- $MonadContDoc
  )
  where

import Prelude (Monad(..),(.),const,IO,Maybe,id)
import Control.Monad(MonadPlus,liftM)

import Data.Monoid(Monoid)



--------------------------------------------------------------------------------
-- | Provides a way of going across one transformer layer.

class MonadTrans t where
  lift  :: Monad m => m a -> t m a
  -- ^ Provides a way of going across one transformer layer.


--------------------------------------------------------------------------------
-- | The predicate @HasBaseMonad m n@ indicates that 'm' is a monad
-- built by applying a number of transformers to 'n'.

class (Monad m, Monad n) => HasBaseMonad m n | m -> n where
  inBase :: n a -> m a
  -- ^ Provides a way of going across multiple transformer layers,
  -- all the way to the innermost atomic monad.


-- Move me somewhere else.
instance HasBaseMonad IO IO where inBase = id
instance HasBaseMonad [] [] where inBase = id
instance HasBaseMonad Maybe Maybe where inBase = id




{- $PlumbingDoc
  /Plumbing transformers/ take care of propagating information around in a computation.
They all commute with each other.  This means that it doesn't meter 
in what order they are added to a computation, the final effect is the same.
-}

-- | A reader monad has the ability to propagate around a read-only environment.
-- One can think of the environment as a special read only variable that can
-- be accessed via the methods of the class.

class (Monad m) => MonadReader r m | m -> r where
  ask         :: m r
  -- ^ Read the value of the variable.

  local       :: (r -> r) -> m a -> m a
  -- ^ The method @local f m@ uses @f@ to change the value of the variable 
  -- for the duration of a computation @m@. After @m@ completes its execution
  -- the original value of the variable is restored.

{- $MonadReaderDoc
  Read-only variables are useful when some information needs to be carried
around, but is not used all the time. Such a situation may occur when a deeply nested
function call needs the information, but most of the functions involved in
a computation will not use it and simply pass it around.  Read-only variables
are very closely related to /implicit parameters/ <...>.
See also `MonadWriter'. 
-}


-- | Gets specific component of the environment, using the projection function
-- supplied.
asks          :: (MonadReader r m) => (r -> a) -> m a
asks f        = liftM f ask


-- | Temporarily sets the value of the read-only variable. One can think of
-- @localSet x m@ as a @let@ construct.  
localSet      :: MonadReader r m => r -> m a -> m a
localSet      = local . const


-- | A writer monad has the ability to collect a number of outputs generated
-- during a computation.  It is like carrying around a buffer that can be
-- manipulated with the methods of the class.  The 'Monoid' class specifies
-- how to make an empty buffer, and how to join two buffers together.
class (Monoid w, Monad m) => MonadWriter w m | m -> w where
  tell        :: w -> m ()
  -- ^ @tell w@ appends the new information @w@ to the buffer.

  listen      :: m a -> m (a, w)
  -- ^ @listen m@ moves the contents of the buffer of computation @m@ to its result.
  -- The resulting computation has an empty buffer.

{- $MonadWriterDoc
  Buffer variables are often useful when one needs to collect some
information, for example while traversing a data structure.  In a sense,
they are the dual of read-only variables, as they propagate outputs
of functions, rather then their inputs.
-}


-- | Gets specific component of the output, using the projection function supplied.
listens       :: (MonadWriter w m) => (w -> b) -> m a -> m (a, b)
listens f m   = liftM (\ ~(a,w) -> (a,f w)) (listen m)


-- | @censor f m@ behaves like @m@ except its output is modified by @f@. 
censor        :: MonadWriter w m => (w -> w) -> m a -> m a
censor f m    = do (a,w) <- listen m
                   tell (f w)   -- the media :-)
                   return a

-- | NOTE: SHOULD THIS BE IN THE LIBRARY?
-- Does what the type suggests.
pass          :: (MonadWriter w m) => m (a, w -> w) -> m a
pass m        = do ((a,f),w) <- listen m
                   tell (f w)
                   return a



-- | A state monad carries around a piece of state.  It is just like
-- having a read-write variable in an imperative language.

class (Monad m) => MonadState s m | m -> s where
  get         :: m s
  -- ^ reads the value of the variable 

  put         :: s -> m ()
  -- ^ @put s@ permanently changes the value of the variable to @s@.

-- $MonadStateDoc
-- 

-- | Gets specific component of the state, using the projection function supplied.
gets          :: (MonadState s m) => (s -> a) -> m a
gets f        = liftM f get

-- | Update the state with a function.
modify        :: (MonadState s m) => (s -> s) -> m ()
modify f      = get >>= put . f


-- $ControlDoc
-- /Control transformers/ are used to manipulate the control flow in a program.
-- In general they do not commute between themselves and with other transformers.
-- This means that it is important in what order they are added on top of a monad.
-- Different orders yield monads with different behavior.  See "FeatureInteract.hs".



-- | An error (or exception) monad is aware that computations may fail.
-- The type @e@ specifies what errors may occur in a computation.
class (Monad m) => MonadError e m | m -> e where
  throwError  :: e -> m a
  -- ^ The method @throwError e@ raises exception @e@.
  -- It never returns a value.

  catchError  :: m a -> (e -> m a) -> m a
  -- ^ The method @catchError m h@ uses the handler @h@ to handle exceptions
  -- raised in computation @m@.  If no exceptions are
  -- raised, the final computation behaves as @m@.  It is possible
  -- for the handler itself to throw an exception.

-- $ErrorDoc

-- | A nondeterminism (or backtracking) monad supports computations that 
-- may fail and backtrack or produce multiple results.  
--
-- Currently some of the methods in this class are inherited from 
-- the class 'MonadPlus' defined in module "Control.Monad".
-- 'mzero' is used to indicate no results. 
-- 'mplus' is used to indicate alternatives.
--
-- Since the use of 'MonadPlus' is somewhat overloaded in Haskell
-- (it is also used for exception handling)
-- in the future 'mzero' and 'mplus' may be added explicitly to this class
-- (with different names).
class (MonadPlus m) => MonadNondet m where
  findAll     :: m a -> m [a]
  -- ^ @findAll m@ is analogous to the construct found in logic languages
  -- (e.g. Prolog, Curry). It produces all possible results of @m@.
  commit      :: m a -> m a
  -- ^ @commit m@ behaves like @m@ except it will produce at most one result.
  -- Thus, it resembles the /cut/ operator of Prolog.
  -- (VERIFY) @findAll (commit m)@ should never produce a list with more than one element.

class Monad m => MonadResume m where
  delay       :: m a -> m a
  force       :: m a -> m a

-- | TODO.
class (Monad m) => MonadCont m where
  callCC      :: ((a -> m b) -> m a) -> m a






