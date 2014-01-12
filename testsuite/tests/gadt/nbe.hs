{-# LANGUAGE GADTs, RankNTypes #-}

module Main where

import Control.Applicative (Applicative(..))
import Control.Monad (liftM, ap)

-- abstract syntax -------------------------------------------------------------
data Ty t where
  Bool :: Ty Bool
  Arr  :: Ty a -> Ty b -> Ty (a -> b)

data Exp g t where
  Var 	 :: Var g t -> Exp g t
  Lam 	 :: Ty a -> Exp (g,a) b -> Exp g (a->b) 
  App 	 :: Exp g (s -> t) -> Exp g s -> Exp g t
  If  	 :: Exp g Bool -> Exp g t -> Exp g t -> Exp g t
  ETrue  :: Exp g Bool
  EFalse :: Exp g Bool

data Var g t where
  ZVar :: Var (h,t) t
  SVar :: Var h t -> Var (h,s) t

-- smart constructors ----------------------------------------------------------
lamE :: Ty s -> (Exp (g,s) s -> Exp (g,s) t) -> Exp g (s -> t)
lamE s f = Lam s (f (Var ZVar))

ifE :: Exp g Bool -> Exp g t -> Exp g t -> Exp g t
ifE t ETrue EFalse = t
ifE t e e' = if eqE e e' then e else If t e e'

-- boring equality tests -------------------------------------------------------
eqB :: BoxExp t -> BoxExp s -> Bool
eqB (Box e) (Box e_) = eqE e e_

eqE :: Exp g t -> Exp h s -> Bool
eqE (Var x) (Var y) = eqV x y
eqE (Lam s e) (Lam s_ e_) = eqT s s_ && eqE e e_
eqE (App e1 e2) (App e1_ e2_) = eqE e1 e1_ && eqE e2 e2_
eqE (If e1 e2 e3) (If e1_ e2_ e3_) = eqE e1 e1_ && (eqE e2 e2_ && eqE e3 e3_)
eqE (ETrue) (ETrue) = True
eqE (EFalse) (EFalse) = True
eqE _ _ = False

eqT :: Ty t -> Ty s -> Bool
eqT (Arr s t) (Arr s_ t_) = eqT s s_ && eqT t t_
eqT Bool Bool = True
eqT _ _ = False

eqV :: Var g t -> Var h s -> Bool
eqV (SVar x) (SVar y) = eqV x y
eqV ZVar ZVar = True
eqV _ _ = False

-- evaluation ------------------------------------------------------------------
var :: Var g t -> g -> t
var ZVar     (_,t) = t
var (SVar x) (h,s) = var x h

eval :: Exp g t -> g -> t
eval (Var x)    g = var x g
eval (Lam _ e)  g = \a -> eval e (g,a)
eval (App e e') g = eval e g (eval e' g)
eval (ETrue)    g = True
eval (EFalse)   g = False
eval (If c t e) g = if eval c g then eval t g else eval e g

-- type inference --------------------------------------------------------------
data TyEnv g where
  Nil :: TyEnv g
  Cons :: Ty t -> TyEnv h -> TyEnv (h,t)

infer :: TyEnv g -> Exp g t -> Ty t
infer g (Var x)		= inferVar g x
infer g (Lam t e)	= Arr t (infer (Cons t g) e)
infer g (App e e')	= case infer g e of Arr _ t -> t
infer g (ETrue)		= Bool
infer g (EFalse)	= Bool
infer g (If _ e _)	= infer g e

inferVar :: TyEnv g -> Var g t -> Ty t
inferVar (Cons t h) (SVar x) = inferVar h x
inferVar (Cons t h) (ZVar)   = t

-- tree monad ------------------------------------------------------------------

data Tree a = Val a | Choice (Tree a) (Tree a)
-- doesn't yet force trees to be fully balanced:
-- 	Val :: a -> Tree a Z
-- 	Choice :: Tree a n -> Tree a n -> Tree a (S n)

instance Functor Tree where
    fmap = liftM

instance Applicative Tree where
    pure = return
    (<*>) = ap

instance Monad Tree where
  return x = Val x
  (Val a) >>= f = f a
  (Choice l r) >>= f = Choice (l >>= f) (r >>= f)

tmap :: Monad m => (a->b) -> m a -> m b
tmap f x = do { a <- x; return (f a) }

flatten t = flatten_ t []
 where
   flatten_ (Val a)      k = a:k
   flatten_ (Choice l r) k = flatten_ l (flatten_ r k)


-- quote & friends -------------------------------------------------------------

-- for values --------------------------
enumV		:: Ty t -> Tree t
questionsV	:: Ty t -> [t -> Bool]


enumV Bool      = Choice (Val True) (Val False)
enumV (Arr s t) = mkEnum (questionsV s) (enumV t)
 where
   mkEnum [] t = tmap const t
   mkEnum (q:qs) es = do
   		   f1 <- mkEnum qs es
		   f2 <- mkEnum qs es
		   return (\d -> if q d then f1 d else f2 d)

questionsV Bool		= return (\x -> x)
questionsV (Arr s t)	= do
			  d <- flatten (enumV s)
			  q <- questionsV t
			  return (\f -> q (f d))

-- for expressions ---------------------
enumE		:: Ty t -> Tree (Exp g t)
questionsE	:: Ty t -> [Exp g t -> Exp g Bool]

enumE Bool      = Choice (Val ETrue) (Val EFalse)
enumE (Arr s t) = tmap (lamE s) (mkEnumE (questionsE s) (enumE t))
 where
   mkEnumE [] t = tmap const t
   mkEnumE (q:qs) es = do
   			f1 <- mkEnumE qs es
			f2 <- mkEnumE qs es
			return (\d -> ifE (q d) (f1 d) (f2 d))

questionsE Bool		= return (\x -> x)
questionsE (Arr s t)	= do
			  d <- flatten (enumE s)
			  q <- questionsE t
			  return (\f -> q (App f d))

-- should be 
-- 	find (List (Exp g Bool) n) -> Tree (Exp g a) n -> Exp g a
find :: [Exp g Bool] -> Tree (Exp g a) -> Exp g a
find []		(Val a)		= a
find (b:bs)	(Choice l r)	= ifE b (find bs l) (find bs r)
find _		_		= error "bad arguments to find"

quote :: Ty t -> t -> Exp g t
quote Bool      t = case t of True -> ETrue; False -> EFalse
quote (Arr s t) f = lamE s (\e -> find (do q <- questionsE s; return (q e))
					(tmap (quote t . f) (enumV s)))

-- normalization (by evaluation) -----------------------------------------------
data BoxExp t = Box (forall g. Exp g t)

normalize :: Ty t -> BoxExp t -> BoxExp t
normalize s (Box e) = Box (quote s (eval e ()))

-- examples --------------------------------------------------------------------
b2b = Arr Bool Bool
b22b = Arr b2b b2b
zero = Var ZVar
one = Var (SVar ZVar)
once   = Box (Lam b2b (Lam Bool (App one zero)))
twice  = Box (Lam b2b (Lam Bool (App one (App one zero))))
thrice = Box (Lam b2b (Lam Bool (App one (App one (App one zero)))))

test = [ eqB (nf b22b thrice) (nf b22b once)
       , eqB (nf b22b twice)  (nf b22b once)]
  where nf = normalize

main = print test