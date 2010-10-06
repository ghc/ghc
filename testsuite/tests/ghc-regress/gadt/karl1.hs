{-# LANGUAGE GADTs, KindSignatures #-}

-- See Trac #301
-- This particular one doesn't use GADTs per se, 
-- but it does use dictionaries in constructors 

module Expr1 where

data Expr :: * -> * where	-- Not a GADT at all
  Const :: Show a => a -> Expr a
		-- Note the Show constraint here
  Var   :: Var a -> Expr a

newtype Var a = V String

instance Show (Var a) where show (V s) = s

--------------------------
e1 :: Expr Int
e1 = Const 42

e2 :: Expr Bool
e2 = Const True

e3 :: Expr Integer
e3 = Var (V "mersenne100")

--------------------------
eval :: Expr a -> a
eval (Const c) = c
eval (Var v) = error ("free variable `" ++ shows v "'")

{-
    Up to here, everything works nicely:

    \begin{verbatim}
    *Expr0> eval e1
    42
    *Expr0> eval e2
    True
    *Expr1> eval e3
    *** Exception: free variable `mersenne100'
    \end{verbatim}

    But let us now try to define a |shows| function.

    In the following, without the type signature we get:
    \begin{verbatim}
    *Expr1> :t showsExpr
    showsExpr :: forall a. (Show a) => Expr a -> String -> String
    *Expr1> showsExpr e1 ""
    "42"
    *Expr1> showsExpr e2 ""
    "True"
    *Expr1> showsExpr e3 ""
    "mersenne100"
    \end{verbatim}

    However, in the last case, the instance |Show Integer| was not used,
    so should not have been required.
    Therefore I would expect it to work as it is now, i.e.,
    with the type signature:
-}

showsExpr :: Expr a -> ShowS
showsExpr (Const c) = shows c
showsExpr (Var v) = shows v

{- 

We used to get a complaint about the |Const| alternative (then line
63) that documents that the constraint in the type of |Const| must
have been ignored:

    No instance for (Show a)
      arising from use of `shows' at Expr1.lhs:63:22-26
    Probable fix: add (Show a) to the type signature(s) for `showsExpr'
    In the definition of `showsExpr': showsExpr (Const c) = shows c
-}
