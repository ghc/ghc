{-# LANGUAGE GADTs, ExistentialQuantification #-}

-- This typechecker, written by Stephanie Weirich at Dagstuhl (Sept 04)
-- demonstrates that it's possible to write functions of type
--	tc :: String -> Term a
-- where Term a is our strongly-typed GADT.  
-- That is, generate a typed term from an untyped source; Lennart 
-- Augustsson set this as a challenge.
--
-- In fact the main function goes
--	tc :: UTerm -> exists ty. (Ty ty, Term ty)
-- so the type checker returns a pair of an expression and its type,
-- wrapped, of course, in an existential.

module Main where

-- Untyped world --------------------------------------------
data UTerm = UVar String
	   | ULam String UType UTerm
	   | UApp UTerm UTerm
	   | UConBool Bool
	   | UIf UTerm UTerm UTerm

data UType = UBool | UArr UType UType

-- Typed world -----------------------------------------------
data Ty t where
  Bool :: Ty Bool
  Arr  :: Ty a -> Ty b -> Ty (a -> b)

data Term g t where
  Var :: Var g t -> Term g t
  Lam :: Ty a -> Term (g,a) b -> Term g (a->b) 
  App :: Term g (s -> t) -> Term g s -> Term g t
  ConBool :: Bool -> Term g Bool
  If :: Term g Bool -> Term g a -> Term g a -> Term g a

data Var g t where
  ZVar :: Var (h,t) t
  SVar :: Var h t -> Var (h,s) t

data Typed thing = forall ty. Typed (Ty ty) (thing ty)

-- Typechecking types
data ExType = forall t. ExType (Ty t)

tcType :: UType -> ExType
tcType UBool = ExType Bool
tcType (UArr t1 t2) = case tcType t1 of { ExType t1' ->
		      case tcType t2 of { ExType t2' ->
		      ExType (Arr t1' t2') }}

-- The type environment and lookup
data TyEnv g where
  Nil :: TyEnv g
  Cons :: String -> Ty t -> TyEnv h -> TyEnv (h,t)

lookupVar :: String -> TyEnv g -> Typed (Var g)
lookupVar _ Nil = error "Variable not found"
lookupVar v (Cons s ty e) 
  | v==s      = Typed ty ZVar
  | otherwise = case lookupVar v e of
		   Typed ty v -> Typed ty (SVar v)

-- Comparing types
newtype C1 c a2 d = C1 { unC1 :: c (d -> a2) }
newtype C2 c b1 d = C2 { unC2 :: c (b1 -> d) }

cast2 :: Ty a -> Ty b -> (c a -> c b)
cast2 Bool Bool x = x
cast2 (Arr a1 a2) (Arr b1 b2) f
  = let   C1 x = cast2 a1 b1 (C1 f)
	  C2 y = cast2 a2 b2 (C2 x)
    in y

data Equal a b where
  Equal :: Equal c c

cmpTy :: Ty a -> Ty b -> Maybe (Equal a b)
cmpTy Bool Bool = Just Equal
cmpTy (Arr a1 a2) (Arr b1 b2)
  = do	{ Equal <- cmpTy a1 b1
	; Equal <- cmpTy a2 b2
	; return Equal }

-- Typechecking terms
tc :: UTerm -> TyEnv g -> Typed (Term g)
tc (UVar v) env = case lookupVar v env of
		    Typed ty v -> Typed ty (Var v)
tc (UConBool b) env
  = Typed Bool (ConBool b)
tc (ULam s ty body) env 
  = case tcType ty of { ExType bndr_ty' ->
    case tc body (Cons s bndr_ty' env) of { Typed body_ty' body' ->
    Typed (Arr bndr_ty' body_ty') 
	  (Lam bndr_ty' body') }}
tc (UApp e1 e2) env
  = case tc e1 env of { Typed (Arr bndr_ty body_ty) e1' ->
    case tc e2 env of { Typed arg_ty e2' ->
    case cmpTy arg_ty bndr_ty of
	Nothing -> error "Type error"
	Just Equal -> Typed body_ty (App e1' e2') }}
tc (UIf e1 e2 e3) env
  = case tc e1 env of { Typed Bool e1' ->
    case tc e2 env of { Typed t2   e2' ->
    case tc e3 env of { Typed t3   e3' ->
    case cmpTy t2 t3 of
	Nothing -> error "Type error"
	Just Equal -> Typed t2 (If e1' e2' e3') }}}

showType :: Ty a -> String
showType Bool = "Bool"
showType (Arr t1 t2) = "(" ++ showType t1 ++ ") -> (" ++ showType t2 ++ ")"

uNot = ULam "x" UBool (UIf (UVar "x") (UConBool False) (UConBool True))

test :: UTerm
test = UApp uNot (UConBool True)

main = putStrLn (case tc test Nil of
			Typed ty _ -> showType ty
		)