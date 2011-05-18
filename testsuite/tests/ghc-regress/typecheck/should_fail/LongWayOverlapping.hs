{-# LANGUAGE TypeFamilies, MultiParamTypeClasses
  , FlexibleContexts, FlexibleInstances, UndecidableInstances
  , TypeSynonymInstances, GeneralizedNewtypeDeriving
  , OverlappingInstances 
  #-}

module LongWayOverlapping where


class M a where 

class M a => XMLG a 

instance M [a] 

instance XMLG [m] where     -- Generates an implication wanted:  forall m. M [m]

class M a => EmbAsChild a b where 
  emb :: b -> [a]


instance EmbAsChild [Char] Bool where 
  emb _ = emb 'c'


  -- This one generates an unsolvable EmbAsChild [Char] Char

-- Original problem is: 
-- [w] EmbAsChild [Char] Char 
-- [w] forall m. M [m]
-- Now, by converting the wanted to given and pushing it inside the implication 
-- we have the following:
-- [g] EmbAsChild [Char] Char 
-- [g] M [Char]   <~~ The superclass of the first given! 
-- [w] M [m] 
-- And now OOPS we can't solve M [m] because we are supposed to delay our choice 
-- as much as possible! 

-- DV:
-- One possible solution is to STOP PUSHING wanteds as givens inside an implication
-- in a checking context. I think it's the best thing to do and I've implemented it.

-- In inference mode that's ok and the error message is very comprehensible, see
-- test case PushedInFlatsOverlap.hs 
