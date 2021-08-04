{-# LANGUAGE CPP #-}
{-# OPTIONS -fglasgow-exts #-}

-- These are simple tests to observe (data)type representations.
module Datatype  where

import Test.Tasty.HUnit

import Data.Tree
import Data.Generics

-- A simple polymorphic datatype
data MyDataType a = MyDataType a
                  deriving (Typeable, Data)


-- Some terms and corresponding type representations
myTerm     = undefined :: MyDataType Int
myTypeRep  = typeOf myTerm            -- type representation in Typeable
myDataType = dataTypeOf myTerm        -- datatype representation in Data

#if MIN_VERSION_base(4,5,0)
myTyCon    = typeRepTyCon myTypeRep   -- type constructor via Typeable
myString1  = tyConName myTyCon        -- type constructor via Typeable
myString2  = dataTypeName myDataType  -- type constructor via Data

-- Main function for testing
tests =  show ( myTypeRep
            , ( myDataType
            , ( tyconModule myString1
            , ( tyconUQname myString1
            , ( tyconModule myString2
            , ( tyconUQname myString2
            ))))))
       @?= output

#if __GLASGOW_HASKELL__ >= 709
-- In GHC 7.10 module name is stripped from DataType
output = "(MyDataType Int,(DataType {tycon = \"MyDataType\", datarep = AlgRep [MyDataType]},(\"\",(\"MyDataType\",(\"\",\"MyDataType\")))))"
#else
output = "(MyDataType Int,(DataType {tycon = \"Datatype.MyDataType\", datarep = AlgRep [MyDataType]},(\"\",(\"MyDataType\",(\"Datatype\",\"MyDataType\")))))"
#endif

#else

tests = show ( myTypeRep, myDataType )
        @?= output

#if __GLASGOW_HASKELL__ >= 701
output = "(MyDataType Int,DataType {tycon = \"Datatype.MyDataType\", datarep = AlgRep [MyDataType]})"
#else
output = "(Datatype.MyDataType Int,DataType {tycon = \"Datatype.MyDataType\", datarep = AlgRep [MyDataType]})"
#endif

#endif
