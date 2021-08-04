{-# OPTIONS -fglasgow-exts #-}

module Polymatch () where


import Data.Typeable
import Data.Generics


-- Representation of kids
kids x = gmapQ Kid x -- get all kids
type Kids = [Kid]
data Kid  = forall k. Typeable k => Kid k


-- Build term from a list of kids and the constructor 
fromConstrL :: Data a => Kids -> Constr -> Maybe a
fromConstrL l = unIDL . gunfold k z
 where
  z c = IDL (Just c) l
  k (IDL Nothing _) = IDL Nothing undefined
  k (IDL (Just f) (Kid x:l)) = IDL f' l
   where
    f' = case cast x of
          (Just x') -> Just (f x')
          _         -> Nothing


-- Helper datatype
data IDL x = IDL (Maybe x) Kids
unIDL (IDL mx _) = mx


-- Two sample datatypes
data A = A String deriving (Read, Show, Eq, Data, Typeable)
data B = B String deriving (Read, Show, Eq, Data, Typeable)


-- Mediate between two "left-equal" Either types
f :: (Data a, Data b, Show a, Read b)
  => (a->b) -> Either String a -> Either String b

f g (Right a)    = Right $ g a       -- conversion really needed
-- f g (Left  s) = Left s            -- unappreciated conversion
-- f g s         = s                 -- doesn't typecheck 
-- f g s         = deep_rebuild s    -- too expensive
f g s            = just (shallow_rebuild s) -- perhaps this is Ok?


-- Get rid of maybies
just = maybe (error "tried, but failed.") id


-- Just mentioned for completeness' sake
deep_rebuild :: (Show a, Read b) => a -> b
deep_rebuild = read . show


-- For the record: it's possible.
shallow_rebuild :: (Data a, Data b) => a -> Maybe b
shallow_rebuild a = b 
 where
  b      = fromConstrL (kids a) constr
  constr = indexConstr (dataTypeOf b) (constrIndex (toConstr a))


-- Test cases
a2b (A s) = B s            -- silly conversion
t1 = f a2b (Left "x")      -- prints Left "x"
t2 = f a2b (Right (A "y")) -- prints Right (B "y")
