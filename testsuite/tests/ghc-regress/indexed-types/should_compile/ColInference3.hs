{-# LANGUAGE TypeFamilies #-}

module Main where 

type family Elem c

class Col c where
 isEmpty   :: c -> Bool
 add       :: c -> Elem c -> c
 headTail  :: c -> (Elem c,c)

-- LIST
instance Col [a] where
  isEmpty   	  = null
  add	   	  = flip (:)
  headTail (x:xs) =  (x,xs)

type instance Elem [a] = a

-- SEQUENCE
data Sequence a = Nil | Snoc (Sequence a) a deriving Show

instance Col (Sequence a) where
  isEmpty Nil = True
  isEmpty _   = False

  add     s x = Snoc s x

  headTail (Snoc s x) = (x,s)

type instance Elem (Sequence a) = a

-- 
addAll c1 c2 
	| isEmpty c1
	= c2
	| otherwise
	= let (x,c1') = headTail c1
	  in addAll c1' (add c2 x)

--
main = print $ addAll c1 c2
       where c1 = ['a','b','c']
             c2 = (Snoc (Snoc (Snoc Nil 'd') 'e') 'f')
