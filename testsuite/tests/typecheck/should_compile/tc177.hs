{-# LANGUAGE FlexibleInstances, UndecidableInstances,
             MultiParamTypeClasses, FunctionalDependencies #-}

-- This is a rather complicated program that uses functional 
-- dependencies to do Peano arithmetic.
--
-- GHC 6.2 dies because tcSimplifyRestricted was trying to
-- be too clever.  See 'Plan B' in tcSimplifyRestricted

module ShouldCompile where



-- This is the offending definition.  It falls under
-- the monomorphism restriction, so tcSimplifyRestricted applies
bug = ins b (ins b Nil)


------------------------------------
data LAB l r = LAB l r deriving Show

data OR a b = OR a b deriving Show


data Cons x y = Cons x y deriving Show

data Nil = Nil deriving Show

data T = T

data F = F

data A = A deriving Show

data B = B deriving Show

data Zero = Zero

data Succ n = Succ n

b = ((LAB B []),[])

-- insertion function
-- insert label pairs in the a list of list, each list contains a collection of
-- label pair that sharing the common label.


class Ins r l l' | r l -> l' where
    ins :: r -> l -> l'


instance Ins ((LAB l1 r1),r1') Nil (Cons (Cons ((LAB l1 r1),r1') Nil) Nil) where
    ins l Nil = (Cons (Cons l Nil) Nil)
   

instance ( L2N l1 n1
     , L2N l2 n2
     , EqR n1 n2 b
     , Ins1 ((LAB l1 r1),r1') (Cons (Cons ((LAB l2 r2),r2') rs) rs') b l
     ) => Ins ((LAB l1 r1),r1') (Cons (Cons ((LAB l2 r2),r2') rs) rs') l 
    where
      ins ((LAB l1 r1),r1') (Cons (Cons ((LAB l2 r2),r2') rs) rs') 
	= ins1  ((LAB l1 r1),r1') (Cons (Cons ((LAB l2 r2),r2') rs) rs') 
		(eqR (l2n l1)  (l2n l2))
-- Note that n1 and n2 are functionally defined by l1 and l2, respectively,
-- and b is functionally defined by n1 and n2.


class Ins1 r l b l' | r l b -> l' where
    ins1 :: r -> l -> b -> l'

instance Ins1 ((LAB l1 r1),r1') (Cons r rs) T 
	      (Cons (Cons ((LAB l1 r1),r1') r) rs) where
   ins1 l (Cons r rs) _ = (Cons (Cons l r) rs)

instance ( Ins ((LAB l1 r1),r1') rs rs') 
      => Ins1 ((LAB l1 r1),r1') (Cons r rs) F (Cons r rs') where
    ins1 l (Cons r rs) _ = (Cons r (ins l rs))

-- class for mapping label to number

class L2N l n | l -> n where
    l2n :: l -> n

instance L2N A Zero where
    l2n A = Zero

instance L2N B (Succ Zero) where
    l2n B = Succ Zero


-- class for comparing number type

class EqR n1 n2 b | n1 n2 -> b where
    eqR :: n1 -> n2 -> b

instance EqR Zero Zero T where
    eqR _ _ = T

instance EqR Zero (Succ n) F where
    eqR _ _ = F

instance EqR (Succ n) Zero F where
    eqR _ _ = F

instance (EqR n1 n2 b) => EqR (Succ n1) (Succ n2) b where
    eqR (Succ n1) (Succ n2) = eqR n1 n2

