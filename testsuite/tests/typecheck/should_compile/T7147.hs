{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Prog where

class AddName i d | d->i where 
  addName :: d -> d

class Rec rec struct | rec->struct where
  mapRec :: (struct->struct) -> rec -> rec

-- We got a very bogus siguature for addNameRec in thc 7.6rc1
-- addNameRec :: forall rec struct.
--                Recursive.Rec rec struct
--                -> DefinedNames.AddName (GHC.Prim.Any *) struct
--                -> rec
--                -> rec

addNameRec x = mapRec addName x

instance AddName Int Bool where
  addName = error "urk"
instance AddName Float Char where
  addName = error "urk"

instance Rec Char Bool where
  mapRec = error "urk"
instance Rec Bool Char where
  mapRec = error "urk"


foo1 :: Char -> Char
foo1 = addNameRec 

foo2 :: Bool -> Bool
foo2 = addNameRec 
