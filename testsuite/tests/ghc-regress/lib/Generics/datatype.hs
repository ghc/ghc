{-# OPTIONS -fglasgow-exts #-}

{-

These are simple tests to observe (data)type representations.

-}


module Main where
import Data.Tree
import Data.Generics

-- A simple polymorphic datatype
data Data a =>
     MyDataType a = MyDataType a
                  deriving (Typeable, Data)

-- Some terms and corresponding type representations
myTerm     = undefined :: MyDataType Int
myTypeRep  = typeOf myTerm
myTyCon    = typerepTyCon myTypeRep
myDataType = dataTypeOf myTerm

-- Main function for testing
main = print ( myTypeRep
	     , myTyCon
             , dataTyCon myDataType
	     , dataTyMod myDataType
             )
