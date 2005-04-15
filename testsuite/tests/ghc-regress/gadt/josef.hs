{-# OPTIONS -fglasgow-exts #-}

-- Program from Josef Svenningsson 

-- Just a short explanation of the program. It contains
-- some class declarations capturing some definitions from
-- category theory. Further down he have a data type for well typed
-- lambda expressions using GADTs. Finally we have a
-- function defining the semantics for lambda terms called
-- 'interp'.

-- Made GHC 6.4 bleat 
--    Quantified type variable `t' is unified with
--        another quantified type variable `terminal'
--    When trying to generalise the type inferred for `interp'


module Bug where

class Category arr where
  idA  :: arr a a
  comp :: arr a b -> arr b c -> arr a c

class Category arr =>
   Terminal terminal arr {- | arr -> terminal -} where
  terminal :: arr a terminal

class Category arr =>
      ProductCategory prod arr {- | arr -> prod -} where
  first  :: arr (prod a b) a
  second :: arr (prod a b) b
  pair   :: arr a b -> arr a c -> arr a (prod b c)

class Category arr =>
      CoproductCategory coprod arr {- | arr -> coprod -} where
  inLeft  :: arr a (coprod a b)
  inRight :: arr b (coprod a b)
  ccase   :: arr a c -> arr b c -> arr (coprod a b) c

class ProductCategory prod arr => 
      Exponential exp prod arr {- | arr -> exp -} where
  eval   :: arr (prod (exp a b) a) b
  curryA :: arr (prod c a) b -> arr c (exp a b)


class (Exponential exp prod arr, Terminal terminal arr) => 
  CartesianClosed terminal exp prod arr {- | arr -> terminal exp prod -}

data V prod env t where
  Z :: V prod (prod env t) t
  S :: V prod env t -> V prod (prod env x) t

data Lambda terminal (exp :: * -> * -> *) prod env t where
    Unit :: Lambda foo exp prod env foo
    Var  :: V prod env t -> Lambda terminal exp prod env t
{-    Lam  :: Lambda terminal exp prod (prod env a) t
	 -> Lambda terminal exp prod env (exp a t)
    App  :: Lambda terminal exp prod env (exp t t') 
	 -> Lambda terminal exp prod env t -> Lambda terminal exp prod env t'
-}

interp :: CartesianClosed terminal exp prod arr => 
	  Lambda terminal exp prod s t -> arr s t
interp (Unit)       = terminal -- Terminal terminal arr => arr a terminal
-- interp (Var Z)      = second
-- interp (Var (S v))  = first `comp` interp (Var v)
-- interp (Lam e)      = curryA (interp e)
-- interp (App e1 e2)  = pair (interp e1) (interp e2) `comp` eval
