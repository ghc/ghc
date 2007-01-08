{-# OPTIONS -fglasgow-exts #-}

module Termination where

{- Message from Jim Apple to Haskell-Cafe, 7/1/07

To show how expressive GADTs are, the datatype Terminating can hold
any term in the untyped lambda calculus that terminates, and none that
don't. I don't think that an encoding of this is too surprising, but I
thought it might be a good demonstration of the power that GADTs
bring.


   Using GADTs to encode normalizable and non-normalizable terms in
   the lambda calculus. For definitions of normalizable and de Bruin
   indices, I used:

   Christian Urban and Stefan Berghofer - A Head-to-Head Comparison of
   de Bruijn Indices and Names. In Proceedings of the International
   Workshop on Logical Frameworks and Meta-Languages: Theory and
   Practice (LFMTP 2006). Seattle, USA. ENTCS. Pages 46-59

   http://www4.in.tum.de/~urbanc/Publications/lfmtp-06.ps

   @incollection{ pierce97foundational,
    author = "Benjamin Pierce",
    title = "Foundational Calculi for Programming Languages",
    booktitle = "The Computer Science and Engineering Handbook",
    publisher = "CRC Press",
    address = "Boca Raton, FL",
    editor = "Allen B. Tucker",
    year = "1997",
    url = "citeseer.ist.psu.edu/pierce95foundational.html"
   }

-}


-- Terms in the untyped lambda-calculus with the de Bruijn representation

data Term t where
    Var :: Nat n -> Term (Var n)
    Lambda :: Term t -> Term (Lambda t)
    Apply :: Term t1 -> Term t2 -> Term (Apply t1 t2)

-- Natural numbers

data Nat n where
    Zero :: Nat Z
    Succ :: Nat n -> Nat (S n)

data Z
data S n

data Var t
data Lambda t
data Apply t1 t2

data Less n m where
    LessZero :: Less Z (S n)
    LessSucc :: Less n m -> Less (S n) (S m)

data Equal a b where
    Equal :: Equal a a

data Plus a b c where
    PlusZero :: Plus Z b b
    PlusSucc :: Plus a b c -> Plus (S a) b (S c)

{- We can reduce a term by function application, reduction under the lambda,
   or reduction of either side of an application. We don't need this full
   power, since we could get by with normal-order evaluation, but that
   required a more complicated datatype for Reduce.
-}
data Reduce t1 t2 where
    ReduceSimple :: Replace Z t1 t2 t3 -> Reduce (Apply (Lambda t1) t2) t3
    ReduceLambda :: Reduce t1 t2 -> Reduce (Lambda t1) (Lambda t2)
    ReduceApplyLeft :: Reduce t1 t2 -> Reduce (Apply t1 t3) (Apply t2 t3)
    ReduceApplyRight :: Reduce t1 t2 -> Reduce (Apply t3 t1) (Apply t3 t2)

{- Lift and Replace use the de Bruijn operations as expressed in the Urban
   and Berghofer paper. -}
data Lift n k t1 t2 where
    LiftVarLess :: Less i k -> Lift n k (Var i) (Var i)
    LiftVarGTE :: Either (Equal i k) (Less k i) -> Plus i n r -> Lift n k (Var i) (Var r)
    LiftApply :: Lift n k t1 t1' -> Lift n k t2 t2' -> Lift n k (Apply t1 t2) (Apply t1' t2')
    LiftLambda :: Lift n (S k) t1 t2 -> Lift n k (Lambda t1) (Lambda t2)

data Replace k t n r where
    ReplaceVarLess :: Less i k -> Replace k (Var i) n (Var i)
    ReplaceVarEq :: Equal i k -> Lift k Z n r -> Replace k (Var i) n r
    ReplaceVarMore :: Less k (S i) -> Replace k (Var (S i)) n (Var i)
    ReplaceApply :: Replace k t1 n r1 -> Replace k t2 n r2 -> Replace k (Apply t1 t2) n (Apply r1 r2)
    ReplaceLambda :: Replace (S k) t n r -> Replace k (Lambda t) n (Lambda r)

{- Reflexive transitive closure of the reduction relation. -}
data ReduceEventually t1 t2 where
    ReduceZero :: ReduceEventually t1 t1
    ReduceSucc :: Reduce t1 t2 -> ReduceEventually t2 t3 -> ReduceEventually t1 t3

-- Definition of normal form: nothing with a lambda term applied to anything.
data Normal t where
    NormalVar :: Normal (Var n)
    NormalLambda :: Normal t -> Normal (Lambda t)
    NormalApplyVar :: Normal t -> Normal (Apply (Var i) t)
    NormalApplyApply :: Normal (Apply t1 t2) -> Normal t3 -> Normal (Apply (Apply t1 t2) t3)

-- Something is terminating when it reduces to something normal
data Terminating where
    Terminating :: Term t -> ReduceEventually t t' -> Normal t' -> Terminating

{- We can encode terms that are non-terminating, even though this set is
   only co-recursively enumerable, so we can't actually prove all of the
   non-normalizable terms of the lambda calculus are non-normalizable.
-}

data Reducible t1 where
    Reducible :: Reduce t1 t2 -> Reducible t1
-- A term is non-normalizable if, no matter how many reductions you have applied,
-- you can still apply one more.
type Infinite t1 = forall t2 . ReduceEventually t1 t2 -> Reducible t2

data NonTerminating where
    NonTerminating :: Term t -> Infinite t -> NonTerminating

-- x
test1 :: Terminating
test1 = Terminating (Var Zero) ReduceZero NormalVar

-- (\x . x)@y
test2 :: Terminating
test2 = Terminating (Apply (Lambda (Var Zero))(Var Zero))
        (ReduceSucc (ReduceSimple (ReplaceVarEq Equal (LiftVarGTE (Left Equal) PlusZero))) ReduceZero)
        NormalVar

-- omega = \x.x@x
type Omega = Lambda (Apply (Var Z) (Var Z))
omega = Lambda (Apply (Var Zero) (Var Zero))

-- (\x . \y . y)@(\z.z@z)
test3 :: Terminating
test3 = Terminating (Apply (Lambda (Lambda (Var Zero))) omega)
        (ReduceSucc (ReduceSimple (ReplaceLambda (ReplaceVarLess LessZero))) ReduceZero)
        (NormalLambda NormalVar)

-- (\x.x@x)(\x.x@x)
test4 :: NonTerminating
test4 = NonTerminating (Apply omega omega) help3

help1 :: Reducible (Apply Omega Omega)
help1 = Reducible (ReduceSimple 
		(ReplaceApply (ReplaceVarEq Equal (LiftLambda 
			(LiftApply (LiftVarLess LessZero) (LiftVarLess LessZero)))) 
		(ReplaceVarEq Equal (LiftLambda (LiftApply 
			(LiftVarLess LessZero) (LiftVarLess LessZero))))))

help2 :: ReduceEventually (Apply Omega Omega) t -> Equal (Apply Omega Omega) t
help2 ReduceZero = Equal
help2 (ReduceSucc (ReduceSimple (ReplaceApply 
	(ReplaceVarEq _ (LiftLambda (LiftApply (LiftVarLess _) (LiftVarLess _)))) 
	(ReplaceVarEq _ (LiftLambda (LiftApply (LiftVarLess _) (LiftVarLess _)))))) y)
  = case help2 y of
      Equal -> Equal

help3 :: Infinite (Apply Omega Omega)
help3 x = case help2 x of
	      Equal -> help1
