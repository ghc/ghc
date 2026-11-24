{-# LANGUAGE CPP, DataKinds, ExperimentalDependentTypes, GADTs, RankNTypes #-}
module NativeCheckedClosureProvider
#ifdef HIDE_LOOP
    (Sort (..), Scope (..), Slot (..), Expr (At, Bind, Call, Lit, Test),
     extendName, renameExpr, extendExpr, walk, replace)
#endif
where

data Sort = Nat | Arrow Sort Sort
data Scope = Empty | Push Scope Sort

data Slot (g :: Scope) (a :: Sort) where
    New :: Slot (Push g a) a
    Old :: Slot g a -> Slot (Push g b) a

data Expr (g :: Scope) (a :: Sort) where
    At   :: Slot g a -> Expr g a
    Bind :: Expr (Push g a) b -> Expr g (Arrow a b)
    Call :: Expr g (Arrow a b) -> Expr g a -> Expr g b
    Lit  :: Expr g Nat
    Test :: Expr g Nat -> Expr g a -> Expr (Push g Nat) a -> Expr g a
    Loop :: Expr (Push g a) a -> Expr g a

extendName :: (forall a. Slot g a -> Slot d a)
           -> (forall s. Slot (Push g b) s -> Slot (Push d b) s)
extendName _ New = New
extendName f (Old x) = Old (f x)

renameExpr :: (forall a. Slot g a -> Slot d a) -> Expr g s -> Expr d s
renameExpr f (At x) = At (f x)
renameExpr f (Bind n) = Bind (renameExpr (extendName f) n)
renameExpr f (Call l m) = Call (renameExpr f l) (renameExpr f m)
renameExpr _ Lit = Lit
renameExpr f (Test l m n) = Test (renameExpr f l) (renameExpr f m)
                              (renameExpr (extendName f) n)
renameExpr f (Loop n) = Loop (renameExpr (extendName f) n)

extendExpr :: (forall a. Slot g a -> Expr d a)
           -> (forall s. Slot (Push g b) s -> Expr (Push d b) s)
extendExpr _ New = At New
extendExpr f (Old x) = renameExpr Old (f x)

walk :: (forall a. Slot g a -> Expr d a) -> Expr g s -> Expr d s
walk f (At x) = f x
walk f (Bind n) = Bind (walk (extendExpr f) n)
walk f (Call l m) = Call (walk f l) (walk f m)
walk _ Lit = Lit
walk f (Test l m n) = Test (walk f l) (walk f m) (walk (extendExpr f) n)
walk f (Loop n) = Loop (walk (extendExpr f) n)

replace :: Expr (Push g a) b -> Expr g a -> Expr g b
replace n w = walk (\case
  New -> w
#ifdef STRICT_CAPTURE
  Old x -> w `seq` At x
#else
  Old x -> At x
#endif
  ) n
