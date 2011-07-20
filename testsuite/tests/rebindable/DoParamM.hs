{-# OPTIONS -XRebindableSyntax #-}
-- Haskell98!

-- Tests of the do-notation for the parameterized monads
-- We demonstrate a variable-type state `monadic' transformer
-- and its phantom-type-state relative to enforce the locking protocol
-- (a lock can be released only if it is being held, and acquired only
-- if it is not being held)
-- The tests are based on the code
-- http://okmij.org/ftp/Computation/monads.html#param-monad

-- Please search for DO-NOT-YET

module DoParamM where

import Prelude (const, String, ($), (.), Maybe(..), 
		Int, fromInteger, succ, pred, fromEnum, toEnum, 
		(+), Char, (==), Bool(..),
		IO, getLine, putStrLn, read, show)
import qualified Prelude
import qualified Control.Monad.State as State
import qualified Control.Monad.Identity as IdM

-- A parameterized `monad'
class Monadish m where
    return :: a -> m p p a
    fail   :: String -> m p p a
    (>>=)  :: m p q a -> (a -> m q r b) -> m p r b

m1 >> m2 = m1 >>= (const m2)

-- All regular monads are the instances of the parameterized monad

newtype RegularM m p q a = RegularM{unRM :: m a}

instance Prelude.Monad m => Monadish (RegularM m) where
    return = RegularM . Prelude.return
    fail   = RegularM . Prelude.fail
    m >>= f = RegularM ((Prelude.>>=) (unRM m) (unRM . f))

-- As a warm-up, we write the regular State computation, with the same 
-- type of state throughout. We thus inject Monad.State into the
-- parameterized monad

test1 = State.runState (unRM c) (0::Int) where
         c = gget >>= (\v -> gput (succ v) >> return v)
         gget :: (State.MonadState s m) => RegularM m s s s
         gget = RegularM State.get
         gput :: (State.MonadState s m) => s -> RegularM m s s ()
         gput = RegularM . State.put
-- (0,1)

-- The same in the do-notation
test1_do = State.runState (unRM c) (0::Int) where
         c = do
	     v <- gget
	     gput (succ v)
	     return v
         gget :: (State.MonadState s m) => RegularM m s s s
         gget = RegularM State.get
         gput :: (State.MonadState s m) => s -> RegularM m s s ()
         gput = RegularM . State.put
-- (0,1)


-- Introduce the variable-type state (transformer)

newtype VST m si so v = VST{runVST:: si -> m (so,v)}

instance Prelude.Monad m => Monadish (VST m) where
  return x = VST (\si -> Prelude.return (si,x))
  fail x   = VST (\si -> Prelude.fail x)
  m >>= f  = VST (\si -> (Prelude.>>=) (runVST m si) 
		                       (\ (sm,x) -> runVST (f x) sm))

vsget :: Prelude.Monad m => VST m si si si
vsget = VST (\si -> Prelude.return (si,si))
vsput :: Prelude.Monad m => so -> VST m si so ()
vsput x = VST (\si -> Prelude.return (x,()))


-- Repeat test1 via VST: the type of the state is the same
vsm1 () = vsget >>= (\v -> vsput (succ v) >> return v)

-- The same with the do-notation
vsm1_do () = do
	     v <- vsget 
	     vsput (succ v)
	     return v

{-
 *DoParamM> :t vsm1
 vsm1 :: (Monadish (VST m), IdM.Monad m, Prelude.Enum si) =>
         () -> VST m si si si
-}

test2 = IdM.runIdentity (runVST (vsm1 ()) (0::Int))
-- (1,0)

test2_do = IdM.runIdentity (runVST (vsm1_do ()) (0::Int))
-- (1,0)


-- Now, we vary the type of the state, from Int to a Char
vsm2 () = vsget >>= (\v -> vsput ((toEnum (65+v))::Char) >> 
                           vsget >>= \v' -> return (v,v'))

{-
 *DoParamM> :t vsm2
 vsm2 :: (Monadish (VST m), IdM.Monad m) => () -> VST m Int Char (Int, Char)
-}

-- The same with the do-notation
 -- the following does not yet work
vsm2_do () = do
	     v <- vsget
             vsput ((toEnum (65+v))::Char)
             v' <- vsget
	     return (v,v')

test3 = IdM.runIdentity (runVST (vsm2 ()) (0::Int))
-- ('A',(0,'A'))

test3_do = IdM.runIdentity (runVST (vsm2_do ()) (0::Int))
-- ('A',(0,'A'))

{- The following is a deliberate error:

 DoParamM.hs:147:55:
    Couldn't match expected type `Int' against inferred type `Char'
    In the second argument of `(==)', namely `v''
    In the first argument of `return', namely `(v == v')'
    In the expression: return (v == v')

vsm3 () = vsget >>= (\v -> vsput ((toEnum (65+v))::Char) >> 
                           vsget >>= \v' -> return (v==v'))
 -}


 -- The following too must report a type error -- the expression
--    return (v == v') must be flagged, rather than something else
vsm3_do () = do
	     v <- vsget
             vsput ((toEnum (65+v))::Char)
             v' <- vsget
	     return (v==v')



-- Try polymorphic recursion, over the state.
-- crec1 invokes itself, and changes the type of the state from
-- some si to Bool.
crec1 :: (Prelude.Enum si, Prelude.Monad m) => VST m si si Int
crec1 = vsget >>= (\s1 -> case fromEnum s1 of
                      0 -> return 0
                      1 -> vsput (pred s1) >> return 1
                      _ -> vsput True >> 
                           crec1 >>= (\v ->
                             (vsput s1 >> -- restore state type to si
                              return (v + 10))))

-- The same in the do-notation
crec1_do :: (Prelude.Enum si, Prelude.Monad m) => VST m si si Int
crec1_do = do
	s1 <- vsget 
        case fromEnum s1 of
           0 -> return 0
           1 -> do {vsput (pred s1); return 1}
           _ -> do
		vsput True
                v <- crec1_do
                vsput s1 -- restore state type to si
                return (v + 10)


test4 = IdM.runIdentity (runVST crec1 'a')
-- ('a',11)

test4_do = IdM.runIdentity (runVST crec1_do 'a')
-- ('a',11)

-- Another example, to illustrate locking and static reasoning about
-- the locking state

data Locked = Locked; data Unlocked = Unlocked
newtype LIO p q a = LIO{unLIO::IO a}

instance Monadish LIO where
  return  = LIO . Prelude.return
  m >>= f = LIO ((Prelude.>>=) (unLIO m) (unLIO . f))

lput :: String -> LIO p p ()
lput = LIO . putStrLn
lget :: LIO p p String
lget = LIO getLine

-- In the real program, the following will execute actions to acquire
-- or release the lock. Here, we just print out our intentions.
lock :: LIO Unlocked Locked ()
lock = LIO (putStrLn "Lock")

unlock :: LIO Locked Unlocked ()
unlock = LIO (putStrLn "UnLock")

-- We should start in unlocked state, and finish in the same state
runLIO :: LIO Unlocked Unlocked a -> IO a
runLIO = unLIO

-- User code

tlock1 = lget >>=  (\l -> 
	 return (read l) >>= (\x ->
	 lput (show (x+1))))

tlock1r = runLIO tlock1

-- the same in the do-notation
tlock1_do = do
	    l <- lget
	    let x = read l
	    lput (show (x+1))

{-
  *VarStateM> :t tlock1
  tlock1 :: LIO p p ()
 Inferred type has the same input and output states and is polymorphic:
 tlock1 does not affect the state of the lock.
-}


tlock2 = lget >>= (\l -> 
	 lock >> (
	 return (read l) >>= (\x ->
	 lput (show (x+1)))))

tlock2_do = do
	    l <- lget
	    lock
	    let x = read l
	    lput (show (x+1))

{-
  *VarStateM> :t tlock2
  tlock2 :: LIO Unlocked Locked ()

The inferred type says that the computation does the locking.
-}

tlock3 = tlock2 >> unlock
tlock3r = runLIO tlock3

{-
  *DoParamM> :t tlock3
  tlock3 :: LIO Unlocked Unlocked ()
-}

{-
*DoParamM> tlock3r
-- user input: 123
Lock
124
UnLock
-}

tlock3_do = do {tlock2_do; unlock}
tlock3r_do = runLIO tlock3_do


-- An attempt to execute the following
-- tlock4 = tlock2 >> tlock2

{-
 gives a type error:
    Couldn't match expected type `Locked'
	   against inferred type `Unlocked'
      Expected type: LIO Locked r b
      Inferred type: LIO Unlocked Locked ()
    In the expression: tlock2
    In a lambda abstraction: \ _ -> tlock2

The error message correctly points out an error of acquiring an already
held lock.
-}

-- The following too must be an error: with the SAME error message as above
tlock4_do = do {tlock2_do; tlock2_do}

-- Similarly, the following gives a type error because of an attempt
-- to release a lock twice
-- tlock4' = tlock2 >> unlock >> unlock
{-
DoParamM.hs:298:30:
    Couldn't match expected type `Unlocked'
	   against inferred type `Locked'
      Expected type: LIO Unlocked r b
      Inferred type: LIO Locked Unlocked ()
    In the second argument of `(>>)', namely `unlock'
    In the expression: (tlock2 >> unlock) >> unlock
-}

 -- The following too must be an error: with the SAME error message as above
tlock4'_do = do {tlock2_do; unlock; unlock}

