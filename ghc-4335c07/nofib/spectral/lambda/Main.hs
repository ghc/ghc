{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main( main ) where

-- From Mark: marku@cs.waikato.ac.nz [Nov 2001]
-- This program contrasts the cost of direct and
-- state-monadic style of computation.

-- Experiments with Higher-Order mappings over terms.
-- Speed comparisons of passing environments in a monad, versus explicitly.
--
--  With Hugs98 Feb 2001:  (K reductions/ K cells)
--     N       mainSimple            mainMonad
--             Redns   Cells       Redns    Cells
--     10         4.7     8          15      28
--     20        16.0    27          49      92
--     30        33.5    57         102     192
--     40        58      98         175     329
--     50        89     150         268     502
--     80       221     373         664    1242
--    160       865    1456        2582    4826
--    320      3419    5754       10181   19021
--
-- With GHC 5.00.1
--     N       mainSimple            mainMonad
--             secs    Kbytes         secs    Kbytes
--     100     0.02      1180         0.08      8213
--     200     0.080     4529         0.30     31997
--     400     0.310    17850         1.36    126333
--     800     1.54     70928          6.6    502096
--  1 1600     7.66    282833         34.6   2001963
--  2 1600                            28.76  1725128  1 with newtype
--  3 1600                            28.1   1694119  2 + eval covers ALL cases
--  4 1600                            28.5   1742828  3 + recursion calls eval
--  5 1600     7.13    282832                         1 with no Add typechecks
--
-- Conclusion:  mainMonad is about 4 times slower than mainSimple
--              and about 7 times more memory.
--              

import System.Environment

import Control.Monad.Trans.State.Strict
import Data.Functor.Identity

main :: IO ()
main = do { mainSimple ; mainMonad }

mainSimple =
    do  args <- getArgs
	if null args
	   then putStrLn "Args: number-to-sum-up-to"
	   else putStrLn (show (simpleEval [] (App sum0 (Con (read(head args))))))

mainMonad =
    do  args <- getArgs
	if null args
	   then putStrLn "Args: number-to-sum-up-to"
	   else (ev (App sum0 (Con (read(head args))))) >> return ()


------------------------------------------------------------
-- Data structures
------------------------------------------------------------
--instance Show (a -> b) where
--    show f = "<function>"


data Term
    = Var String
    | Con Int
    | Incr
    | Add Term Term
    | Lam String Term
    | App Term Term
    | IfZero Term Term Term
    -- the following terms are used internally
    | Thunk Term Env  -- a closure
    deriving (Eq,Read,Show)

type Env = [(String,Term)]


----------------------------------------------------------------------
-- Evaluate a term
----------------------------------------------------------------------
ev :: Term -> IO (Env,Term)
ev t =
    do  let (t2, env) = runState (traverseTerm t :: State Env Term) []
	putStrLn (pp t2 ++ "  " ++ ppenv env)
	return (env,t2)


-----------------------------------------------------------------
-- This class extends Monad to have the standard features
-- we expect while evaluating/manipulating expressions.
----------------------------------------------------
class (Monad m) => EvalEnvMonad m where
    incr :: m ()     -- example of a state update function
    -- these defines the traversal!
    traverseTerm :: Term -> m Term
    --traversePred :: Pred -> m Pred
    lookupVar :: String -> m Term
    pushVar   :: String -> Term -> m a -> m a
    currEnv   :: m Env         -- returns the current environment
    withEnv   :: Env -> m a -> m a  -- uses the given environment
    pushVar v t m = do env <- currEnv; withEnv ((v,t):env) m

instance EvalEnvMonad (State Env) where
    incr = return ()
    traverseTerm = eval
    lookupVar v = do
          env <- get
          return $ lookup2 env
	where
          lookup2 env = maybe (error ("undefined var: " ++ v)) id (lookup v env)
    currEnv = get
    withEnv tmp m = return (evalState m tmp)


eval :: (EvalEnvMonad m) => Term -> m Term
eval (Var x)   =
    do e <- currEnv
       t <- lookupVar x
       traverseTerm t
eval (Add u v) =
    do {Con u' <- traverseTerm u;
	Con v' <- traverseTerm v;
	return (Con (u'+v'))}
eval (Thunk t e) =
    withEnv e (traverseTerm t)
eval f@(Lam x b) =
    do  env <- currEnv
	return (Thunk f env)  -- return a closure!
eval (App u v) =
    do {u' <- traverseTerm u;
	-- call-by-name, so we do not evaluate the argument v
	apply u' v
       }
eval (IfZero c a b) =
    do {val <- traverseTerm c;
	if val == Con 0
	   then traverseTerm a
	   else traverseTerm b}
eval (Con i)   = return (Con i)
eval (Incr)    = incr >> return (Con 0)

--apply :: Term -> Term -> StateMonad2 Term
apply (Thunk (Lam x b) e) a =
    do  orig <- currEnv
	withEnv e (pushVar x (Thunk a orig) (traverseTerm b))
apply a b         = fail ("bad application: " ++ pp a ++
			      "  [ " ++ pp b ++ " ].")




----------------------------------------------------------------------
-- A directly recursive Eval, with explicit environment
----------------------------------------------------------------------
-- A trivial monad so that we can use monad syntax.
newtype Id a = Id (Identity a)
    deriving (Applicative, Functor, Monad)

instance Show a => Show (Id a) where
    show (Id i) = show (runIdentity i)

simpleEval :: Env -> Term -> Id Term
simpleEval env (Var v) =
    simpleEval env (maybe (error ("undefined var: " ++ v)) id (lookup v env))
simpleEval env e@(Con _) =
    return e
simpleEval env e@Incr =
    return (Con 0)
simpleEval env (Add u v) =
    do {Con u' <- simpleEval env u;
	Con v' <- simpleEval env v;
	return (Con (u' + v'))}
    where
    addCons (Con a) (Con b) = return (Con (a+b))
    addCons (Con _) b = fail ("type error in second arg of Add: " ++ pp b)
    addCons a (Con _) = fail ("type error in first arg of Add: " ++ pp a)
simpleEval env f@(Lam x b) =
    return (Thunk f env)  -- return a closure!
simpleEval env (App u v) =
    do {u' <- simpleEval env u;
	-- call-by-name, so we do not evaluate the argument v
	simpleApply env u' v
       }
simpleEval env (IfZero c a b) =
    do {val <- simpleEval env c;
	if val == Con 0
	   then simpleEval env a
	   else simpleEval env b}
simpleEval env (Thunk t e) =
    simpleEval e t

simpleApply :: Env -> Term -> Term -> Id Term
simpleApply env (Thunk (Lam x b) e) a =
    simpleEval env2 b
    where
    env2 = (x, Thunk a env) : e
simpleApply env a b         = fail ("bad application: " ++ pp a ++
			      "  [ " ++ pp b ++ " ].")

------------------------------------------------------------
-- Utility functions for printing terms and envs.
------------------------------------------------------------
ppenv env = "[" ++ concatMap (\(v,t) -> v ++ "=" ++ pp t ++ ", ") env ++ "]"


pp :: Term -> String
pp = ppn 0

-- Precedences:
--   0 = Lam and If (contents never bracketed)
--   1 = Add
--   2 = App
--   3 = atomic and bracketed things
ppn :: Int -> Term -> String
ppn _ (Var v) = v
ppn _ (Con i) = show i
ppn _ (Incr)  = "INCR"
ppn n (Lam v t) = bracket n 0 ("@" ++ v ++ ". " ++ ppn (-1) t)
ppn n (Add a b) = bracket n 1 (ppn 1 a ++ " + " ++ ppn 1 b)
ppn n (App a b) = bracket n 2 (ppn 2 a ++ " " ++ ppn 2 b)
ppn n (IfZero c a b) = bracket n 0
    ("IF " ++ ppn 0 c ++ " THEN " ++ ppn 0 a ++ " ELSE " ++ ppn 0 b)
ppn n (Thunk t e) = bracket n 0 (ppn 3 t ++ "::" ++ ppenv e)

bracket outer this t | this <= outer = "(" ++ t ++ ")"
		     | otherwise     = t


------------------------------------------------------------
-- Test Data
------------------------------------------------------------
x  = (Var "x")
y  = (Var "y")
a1 = (Lam "x" (Add (Var "x") (Con 1)))
aa = (Lam "x" (Add (Var "x") (Var "x")))

-- These should all return 1
iftrue = (IfZero (Con 0) (Con 1) (Con 2))
iffalse = (IfZero (Con 1) (Con 2) (Con 1))

-- This function sums all the numbers from 0 upto its argument.
sum0 :: Term
sum0 = (App fix partialSum0)
partialSum0 = (Lam "sum"
		  (Lam "n"
		   (IfZero (Var "n")
		    (Con 0)
		    (Add (Var "n") (App (Var "sum") nMinus1)))))
nMinus1 = (Add (Var "n") (Con (-1)))

lfxx :: Term
lfxx = (Lam "x" (App (Var "F") (App (Var "x") (Var "x"))))

-- This is the fix point combinator:  Y
fix :: Term
fix = (Lam "F" (App lfxx lfxx))
