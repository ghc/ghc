{-
From: Kevin Hammond <kh>
To: partain
Subject: Nasty Overloading
Date: Wed, 23 Oct 91 16:19:46 BST
-}
module Main where

class Foo a where
	o1 :: a -> a -> Bool
	o2 :: a -> Int

--	o2 :: Int
    -- Lennart: The type of method o2 does not contain the variable a
    -- (and it must according to line 1 page 29 of the manual).

class Foo tyvar => Bar tyvar where
	o3 :: a -> tyvar -> tyvar

-- class (Eq a, Foo a) => Baz a where
class (Ord a, Foo a) => Baz a where
	o4 :: a -> a -> (String,String,String,a)

instance (Ord a, Foo a) =>  Foo [a] where
	o2 x = 100
	o1 a b = a < b || o1 (head a) (head b)

-- instance Bar [a] where
instance (Ord a, Foo a) => Bar [a] where
	o3 x l = []
    --
    -- Lennart: I guess the instance declaration 
    -- 	instance Bar [w] where
    -- 		o3 x l = []
    -- is wrong because to be a Bar you have to be a Foo.  For [w] to
    -- be a Foo, w has to be Ord and Foo.  But w is not Ord or Foo in
    -- this instance declaration so it must be wrong.  (Page 31, line
    -- 7: The context c' must imply ...)

instance Baz a => Baz [a] where
 	o4 [] [] = ("Nil", "Nil", "Nil", [])
	o4 l1 l2 = 
		(if o1 l1 l2 then "Y" else "N",
		 if l1 == l2 then "Y" else "N",
--		 if o4 (head l1) (head l2) then "Y" else "N",
		 case o4 (head l1) (head l2) of
			(_,_,_,l3) -> if (o1 (head l1) l3) then "Y" else "N",
		 l1 ++ l2 )

instance Foo Int where
	o2 x = x
	o1 i j = i == j

instance Bar Int where
	o3 _ j = j + 1

instance Baz Int where
--	o4 i j = i > j
	o4 i j = (if i>j then "Y" else "Z", "p", "q", i+j)
--simpl:o4 i j = ("Z", "p", "q", i+j)

{- also works w/ glhc! -}

main =  if o4 [1,2,3] [1,3,2::Int] /= ("Y","N","Y",[1,2,3,1,3,2]) then
		(print "43\n") 
	else	(print "144\n")

{- works: glhc
main =  case o4 [1,2,3] [1,3,2::Int] of
		(s1,s2,s3,x) -> print s1

main =  case o4 ([]::[Int]) ([]::[Int]) of
		(s1,s2,s3,x) -> print s1
-}

{- simple main: breaks nhc, works w/ glhc 
main = case o4 (3::Int) (4::Int) of (s1,s2,s3,x) -> print s1
-}
