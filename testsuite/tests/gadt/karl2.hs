{-# LANGUAGE GADTs, KindSignatures #-}

module Expr0 where

-- See Trac #301
-- This one *does* use GADTs (Fct)

data Expr :: * -> * where
  Const :: Show a => a -> Expr a
  Apply :: Fct a b -> Expr a -> Expr b

data Fct :: * -> * -> * where
  Succ :: Fct Int Int
  EqZero :: Fct Int Bool
  Add :: Fct Int (Int -> Int)

------------------------------
e1 :: Expr Int
e1 = Apply Succ (Const 41)

e2 :: Expr Bool
e2 = Apply EqZero e1

e3 :: Expr (Int -> Int)
e3 = Apply Add e1

------------------------------
eval :: Expr a -> a
eval (Const c) = c
eval (Apply f a) = evalFct f $ eval a

evalFct :: Fct a b -> a -> b
evalFct Succ = succ
evalFct EqZero = (0 ==)
evalFct Add = (+)


{-  Up to here, everything works nicely:

    \begin{verbatim}
    *Expr0> eval e1
    42
    *Expr0> eval e2
    False
    *Expr0> eval e3 5
    47
    \end{verbatim}

    But let us now try to define a |Show| instance.
    For |Fct|, this is not a problem:
-}

instance Show (Fct a b) where
  show Succ = "S"
  show EqZero = "isZero"
  show Add = "add"

showsExpr :: Expr a -> ShowS
showsExpr (Const c) = shows c
showsExpr (Apply f a) =
    ('(' :) . shows f . (' ' :) . showsExpr a . (')' :)

instance Show (Expr a) where
  showsPrec _ (Const c) = shows c
  showsPrec _ (Apply f a) =
    ('(' :) . shows f . (' ' :) . shows a . (')' :)

{- But we used to get a complaint about the |Const| alternative (then
   line 56) that documents that the constraint in the type of |Const|
   must have been ignored:

   \begin{verbatim}
       No instance for (Show a)
         arising from use of `shows' at Expr0.lhs:56:22-26
       Probable fix: add (Show a) to the type signature(s) for `showsExpr'
       In the definition of `showsExpr': showsExpr (Const c) = shows c
   \end{verbatim}

   But if we do that, the recursive call is of course still unsatisfied:
   \begin{verbatim}
       No instance for (Show a)
         arising from use of `showsExpr' at Expr0.lhs:65:34-42
       Probable fix: add (Show a) to the existential context for `Apply'
       In the first argument of `(.)', namely `showsExpr a'
       In the second argument of `(.)', namely `(showsExpr a) . ((')' :))'
       In the second argument of `(.)', namely
           `((' ' :)) . ((showsExpr a) . ((')' :)))'
   \end{verbatim}

   Following also the advice given in this last error message
   actually makes GHC accept this, and then we can say:

   \begin{verbatim}
   *Expr0> showsExpr e1 ""
   "(S 41)"
   *Expr0> showsExpr e2 ""
   "(isZero (S 41))"
   \end{verbatim}

   However, following this advice is counterintuitive
   and should be unnecessary
   since the |Show| instance for argument types
   is only ever used in the const case.
   We get:

   \begin{verbatim}
   *Expr0> showsExpr e3 ""

   <interactive>:1:0:
       No instance for (Show (Int -> Int))
         arising from use of `showsExpr' at <interactive>:1:0-8
       Probable fix: add an instance declaration for (Show (Int -> Int))
       In the definition of `it': it = showsExpr e3 ""
   \end{verbatim}

   But of course we would expect the following:

   \begin{verbatim}
   *Expr0> showsExpr e3 ""
   "(add (S 41))"
   \end{verbatim}


   \bigskip
   The error messages are almost the same
   if we define a |Show| instance directly
   (line 90 was the |Const| alternative):

   \begin{verbatim}
       Could not deduce (Show a) from the context (Show (Expr a))
         arising from use of `shows' at Expr0.lhs:90:26-30
       Probable fix: add (Show a) to the class or instance method `showsPrec'
   \end{verbatim}
-}


