{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances #-}
-- -fallow-undecidable-instanced now needed because the Coverage Condition fails

-- !!! Functional dependency test. Hugs [Apr 2001] fails to typecheck this
-- We should infer this type for foo
--	foo :: Q (S (S Z)) (S Z)

module ShouldCompile where

data Z = Z
data S a = S a

class Add a b c | a b -> c where add :: a -> b -> c

instance Add Z a a
instance Add a b c => Add (S a) b (S c)

class Mul a b c | a b -> c where mul :: a -> b -> c

instance Mul Z a Z
instance (Mul a b c, Add b c d) => Mul (S a) b d

data Q a b = Q a b

-- Problem here.  This is the addition of rational
-- numbers: (a/b) + (c/d) = (ad+bc)/bd

instance (Mul a d ad,
          Mul b c bc,
          Mul b d bd,
          Add ad bc ad_bc) => Add (Q a b) (Q c d) (Q ad_bc bd)

z = Z
sz = S Z
ssz = S (S Z)

foo = add (Q sz sz) (Q sz sz)
