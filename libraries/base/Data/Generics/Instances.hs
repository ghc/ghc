-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Instances
-- Copyright   :  (c) The University of Glasgow, CWI 2001--2004
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- \"Scrap your boilerplate\" --- Generic programming in Haskell 
-- See <http://www.cs.vu.nl/boilerplate/>. The present module
-- instantiates the class Data for Prelude-like datatypes.
--
-----------------------------------------------------------------------------

module Data.Generics.Instances 
where


------------------------------------------------------------------------------

#ifdef __HADDOCK__
import Prelude
#endif

import Data.Generics.Basics

import Data.Typeable
import Data.Int              -- So we can give Data instance for Int8, ...
import Data.Word             -- So we can give Data instance for Word8, ...
import GHC.Real( Ratio(..) ) -- So we can give Data instance for Ratio
import GHC.IOBase	     -- So we can give Data instance for IO, Handle
import GHC.Ptr	     	     -- So we can give Data instance for Ptr
import GHC.Stable	     -- So we can give Data instance for StablePtr

#include "Typeable.h"


 
------------------------------------------------------------------------------
--
--	Instances of the Data class for Prelude-like types.
--	We define top-level definitions for representations.
--
------------------------------------------------------------------------------


falseConstr  = mkDataCon boolDataType "False" Prefix
trueConstr   = mkDataCon boolDataType "True"  Prefix
boolDataType = mkDataType "Prelude.Bool" [falseConstr,trueConstr]

instance Data Bool where
  toConstr False = falseConstr
  toConstr True  = trueConstr
  fromConstr c = case conIndex c of
                   1 -> False
                   2 -> True
                   _ -> error "fromConstr"
  dataTypeOf _ = boolDataType


------------------------------------------------------------------------------


charType = mkStringType "Prelude.Char"

instance Data Char where
  toConstr x = mkStringCon charType [x]
  fromConstr con = case conRep con of
                     (StringCon [x]) -> x
                     _ -> error "fromConstr"
  dataTypeOf _ = charType


------------------------------------------------------------------------------


floatType = mkFloatType "Prelude.Float"

instance Data Float where
  toConstr x = mkFloatCon floatType (realToFrac x)
  fromConstr con = case conRep con of
                     (FloatCon x) -> realToFrac x
                     _ -> error "fromConstr"
  dataTypeOf _ = floatType


------------------------------------------------------------------------------


doubleType = mkFloatType "Prelude.Double"

instance Data Double where
  toConstr = mkFloatCon floatType
  fromConstr con = case conRep con of
                     (FloatCon x) -> x
                     _ -> error "fromConstr"
  dataTypeOf _ = doubleType


------------------------------------------------------------------------------


intType = mkIntType "Prelude.Int"

instance Data Int where
  toConstr x = mkIntCon intType (fromIntegral x)
  fromConstr con = case conRep con of
                     (IntCon x) -> fromIntegral x
                     _ -> error "fromConstr"
  dataTypeOf _ = intType


------------------------------------------------------------------------------


integerType = mkIntType "Prelude.Integer"

instance Data Integer where
  toConstr = mkIntCon integerType
  fromConstr con = case conRep con of
                     (IntCon x) -> x
                     _ -> error "fromConstr"
  dataTypeOf _ = integerType


------------------------------------------------------------------------------


int8Type = mkIntType "Data.Int.Int8"

instance Data Int8 where
  toConstr x = mkIntCon int8Type (fromIntegral x)
  fromConstr con = case conRep con of
                     (IntCon x) -> fromIntegral x
                     _ -> error "fromConstr"
  dataTypeOf _ = int8Type


------------------------------------------------------------------------------


int16Type = mkIntType "Data.Int.Int16"

instance Data Int16 where
  toConstr x = mkIntCon int16Type (fromIntegral x)
  fromConstr con = case conRep con of
                     (IntCon x) -> fromIntegral x
                     _ -> error "fromConstr"
  dataTypeOf _ = int16Type


------------------------------------------------------------------------------


int32Type = mkIntType "Data.Int.Int32"

instance Data Int32 where
  toConstr x = mkIntCon int32Type (fromIntegral x)
  fromConstr con = case conRep con of
                     (IntCon x) -> fromIntegral x
                     _ -> error "fromConstr"
  dataTypeOf _ = int32Type


------------------------------------------------------------------------------


int64Type = mkIntType "Data.Int.Int64"

instance Data Int64 where
  toConstr x = mkIntCon int64Type (fromIntegral x)
  fromConstr con = case conRep con of
                     (IntCon x) -> fromIntegral x
                     _ -> error "fromConstr"
  dataTypeOf _ = int64Type


------------------------------------------------------------------------------


wordType = mkIntType "Data.Word.Word"

instance Data Word where
  toConstr x = mkIntCon wordType (fromIntegral x)
  fromConstr con = case conRep con of
                     (IntCon x) -> fromIntegral x
                     _ -> error "fromConstr"
  dataTypeOf _ = wordType


------------------------------------------------------------------------------


word8Type = mkIntType "Data.Word.Word8"

instance Data Word8 where
  toConstr x = mkIntCon word8Type (fromIntegral x)
  fromConstr con = case conRep con of
                     (IntCon x) -> fromIntegral x
                     _ -> error "fromConstr"
  dataTypeOf _ = word8Type


------------------------------------------------------------------------------


word16Type = mkIntType "Data.Word.Word16"

instance Data Word16 where
  toConstr x = mkIntCon word16Type (fromIntegral x)
  fromConstr con = case conRep con of
                     (IntCon x) -> fromIntegral x
                     _ -> error "fromConstr"
  dataTypeOf _ = word16Type


------------------------------------------------------------------------------


word32Type = mkIntType "Data.Word.Word32"

instance Data Word32 where
  toConstr x = mkIntCon word32Type (fromIntegral x)
  fromConstr con = case conRep con of
                     (IntCon x) -> fromIntegral x
                     _ -> error "fromConstr"
  dataTypeOf _ = word32Type


------------------------------------------------------------------------------


word64Type = mkIntType "Data.Word.Word64"

instance Data Word64 where
  toConstr x = mkIntCon word64Type (fromIntegral x)
  fromConstr con = case conRep con of
                     (IntCon x) -> fromIntegral x
                     _ -> error "fromConstr"
  dataTypeOf _ = word64Type


------------------------------------------------------------------------------


ratioConstr = mkDataCon ratioDataType ":%" Infix
ratioDataType = mkDataType "GHC.Real.Ratio" [ratioConstr]

instance (Data a, Integral a) => Data (Ratio a) where
  toConstr _ = ratioConstr
  fromConstr c | conIndex c == 1 = undefined :% undefined
  fromConstr _ = error "fromConstr"
  dataTypeOf _ = ratioDataType


------------------------------------------------------------------------------


nilConstr    = mkDataCon listDataType "[]"  Prefix
consConstr   = mkDataCon listDataType "(:)" Infix
listDataType = mkDataType "Prelude.[]" [nilConstr,consConstr]

instance Data a => Data [a] where
  gfoldl f z []     = z []
  gfoldl f z (x:xs) = z (:) `f` x `f` xs
  toConstr []    = nilConstr
  toConstr (_:_) = consConstr
  fromConstr c = case conIndex c of
                   1 -> []
                   2 -> undefined:undefined
                   _ -> error "fromConstr"
  dataTypeOf _ = listDataType
  cast0to1   = cast1

--
-- The gmaps are given as an illustration.
-- This shows that the gmaps for lists are different from list maps.
--
  gmapT  f   []     = []
  gmapT  f   (x:xs) = (f x:f xs)
  gmapQ  f   []     = []
  gmapQ  f   (x:xs) = [f x,f xs]
  gmapM  f   []     = return []
  gmapM  f   (x:xs) = f x >>= \x' -> f xs >>= \xs' -> return (x':xs')


------------------------------------------------------------------------------


nothingConstr = mkDataCon maybeDataType "Nothing" Prefix
justConstr    = mkDataCon maybeDataType "Just"    Prefix
maybeDataType = mkDataType "Prelude.Maybe" [nothingConstr,justConstr]

instance Data a => Data (Maybe a) where
  gfoldl f z Nothing  = z Nothing
  gfoldl f z (Just x) = z Just `f` x
  toConstr Nothing  = nothingConstr
  toConstr (Just _) = justConstr
  fromConstr c = case conIndex c of
                   1 -> Nothing
                   2 -> Just undefined
                   _ -> error "fromConstr"
  dataTypeOf _ = maybeDataType
  cast0to1   = cast1


------------------------------------------------------------------------------


ltConstr         = mkDataCon orderingDataType "LT" Prefix
eqConstr         = mkDataCon orderingDataType "EQ" Prefix
gtConstr         = mkDataCon orderingDataType "GT" Prefix
orderingDataType = mkDataType "Prelude.Ordering" [ltConstr,eqConstr,gtConstr]

instance Data Ordering where
  gfoldl f z LT  = z LT
  gfoldl f z EQ  = z EQ
  gfoldl f z GT  = z GT
  toConstr LT  = ltConstr
  toConstr EQ  = eqConstr
  toConstr GT  = gtConstr
  fromConstr c = case conIndex c of
                   1 -> LT
                   2 -> EQ
                   3 -> GT
                   _ -> error "fromConstr"
  dataTypeOf _ = orderingDataType


------------------------------------------------------------------------------


leftConstr     = mkDataCon eitherDataType "Left"  Prefix
rightConstr    = mkDataCon eitherDataType "Right" Prefix
eitherDataType = mkDataType "Prelude.Either" [leftConstr,rightConstr]

instance (Data a, Data b) => Data (Either a b) where
  gfoldl f z (Left a)   = z Left  `f` a
  gfoldl f z (Right a)  = z Right `f` a
  toConstr (Left _)  = leftConstr
  toConstr (Right _) = rightConstr
  fromConstr c = case conIndex c of
                   1 -> Left undefined
                   2 -> Right undefined
                   _ -> error "fromConstr"
  dataTypeOf _ = eitherDataType
  cast0to2   = cast2


------------------------------------------------------------------------------


--
-- A last resort for functions
--

instance (Data a, Data b) => Data (a -> b) where
  toConstr _   = error "toConstr"
  fromConstr _ = error "fromConstr"
  dataTypeOf _ = mkNorepType "Prelude.(->)"
  cast0to2     = cast2


------------------------------------------------------------------------------


tuple0Constr = mkDataCon tuple0DataType "()" Prefix
tuple0DataType = mkDataType "Prelude.()" [tuple0Constr]

instance Data () where
  toConstr _ = tuple0Constr
  fromConstr c | conIndex c == 1 = ()  
  fromConstr _ = error "fromConstr"
  dataTypeOf _ = tuple0DataType


------------------------------------------------------------------------------


tuple2Constr = mkDataCon tuple2DataType "(,)" Infix
tuple2DataType = mkDataType "Prelude.(,)" [tuple2Constr]

instance (Data a, Data b) => Data (a,b) where
  gfoldl f z (a,b) = z (,) `f` a `f` b
  toConstr _ = tuple2Constr
  fromConstr c | conIndex c == 1 = (undefined,undefined)
  fromConstr _ = error "fromConstr"
  dataTypeOf _ = tuple2DataType
  cast0to2   = cast2


------------------------------------------------------------------------------


tuple3Constr = mkDataCon tuple3DataType "(,,)" Infix
tuple3DataType = mkDataType "Prelude.(,)" [tuple3Constr]

instance (Data a, Data b, Data c) => Data (a,b,c) where
  gfoldl f z (a,b,c) = z (,,) `f` a `f` b `f` c
  toConstr _ = tuple3Constr
  fromConstr c | conIndex c == 1 = (undefined,undefined,undefined)
  fromConstr _ = error "fromConstr"
  dataTypeOf _ = tuple3DataType


------------------------------------------------------------------------------


tuple4Constr = mkDataCon tuple4DataType "(,,,)" Infix
tuple4DataType = mkDataType "Prelude.(,,,)" [tuple4Constr]

instance (Data a, Data b, Data c, Data d)
         => Data (a,b,c,d) where
  gfoldl f z (a,b,c,d) = z (,,,) `f` a `f` b `f` c `f` d
  toConstr _ = tuple4Constr
  fromConstr c = case conIndex c of
                   1 -> (undefined,undefined,undefined,undefined)
                   _ -> error "fromConstr"
  dataTypeOf _ = tuple4DataType


------------------------------------------------------------------------------


tuple5Constr = mkDataCon tuple5DataType "(,,,,)" Infix
tuple5DataType = mkDataType "Prelude.(,,,,)" [tuple5Constr]

instance (Data a, Data b, Data c, Data d, Data e)
         => Data (a,b,c,d,e) where
  gfoldl f z (a,b,c,d,e) = z (,,,,) `f` a `f` b `f` c `f` d `f` e
  toConstr _ = tuple5Constr
  fromConstr c = case conIndex c of
                   1 -> (undefined,undefined,undefined,undefined,undefined)
                   _ -> error "fromConstr"
  dataTypeOf _ = tuple5DataType


------------------------------------------------------------------------------


tuple6Constr = mkDataCon tuple6DataType "(,,,,,)" Infix
tuple6DataType = mkDataType "Prelude.(,,,,,)" [tuple6Constr]

instance (Data a, Data b, Data c, Data d, Data e, Data f)
         => Data (a,b,c,d,e,f) where
  gfoldl f z (a,b,c,d,e,f') = z (,,,,,) `f` a `f` b `f` c `f` d `f` e `f` f'
  toConstr _ = tuple6Constr
  fromConstr c =
    case conIndex c of
           1 -> (undefined,undefined,undefined,undefined,undefined,undefined)
           _ -> error "fromConstr"
  dataTypeOf _ = tuple6DataType


------------------------------------------------------------------------------


tuple7Constr = mkDataCon tuple7DataType "(,,,,,,)" Infix
tuple7DataType = mkDataType "Prelude.(,,,,,,)" [tuple7Constr]

instance (Data a, Data b, Data c, Data d, Data e, Data f, Data g)
         => Data (a,b,c,d,e,f,g) where
  gfoldl f z (a,b,c,d,e,f',g) =
    z (,,,,,,) `f` a `f` b `f` c `f` d `f` e `f` f' `f` g
  toConstr _ = tuple7Constr
  fromConstr c = case conIndex c of
   1 -> (undefined,undefined,undefined,undefined,undefined,undefined,undefined)
   _ -> error "fromConstr"
  dataTypeOf _ = tuple7DataType


------------------------------------------------------------------------------


instance Data TypeRep where
  toConstr _   = error "toConstr"
  fromConstr _ = error "fromConstr"
  dataTypeOf _ = mkNorepType "Data.Typeable.TypeRep"


------------------------------------------------------------------------------


instance Data TyCon where
  toConstr _   = error "toConstr"
  fromConstr _ = error "fromConstr"
  dataTypeOf _ = mkNorepType "Data.Typeable.TyCon"


------------------------------------------------------------------------------


INSTANCE_TYPEABLE0(DataType,dataTypeTc,"DataType")

instance Data DataType where
  toConstr _   = error "toConstr"
  fromConstr _ = error "fromConstr"
  dataTypeOf _ = mkNorepType "Data.Generics.Basics.DataType"


------------------------------------------------------------------------------


instance Typeable a => Data (IO a) where
  toConstr _   = error "toConstr"
  fromConstr _ = error "fromConstr"
  dataTypeOf _ = mkNorepType "GHC.IOBase.IO"


------------------------------------------------------------------------------


instance Data Handle where
  toConstr _   = error "toConstr"
  fromConstr _ = error "fromConstr"
  dataTypeOf _ = mkNorepType "GHC.IOBase.Handle"


------------------------------------------------------------------------------


instance Typeable a => Data (Ptr a) where
  toConstr _   = error "toConstr"
  fromConstr _ = error "fromConstr"
  dataTypeOf _ = mkNorepType "GHC.Ptr.Ptr"


------------------------------------------------------------------------------


instance Typeable a => Data (StablePtr a) where
  toConstr _   = error "toConstr"
  fromConstr _ = error "fromConstr"
  dataTypeOf _ = mkNorepType "GHC.Stable.StablePtr"


------------------------------------------------------------------------------


instance Typeable a => Data (IORef a) where
  toConstr _   = error "toConstr"
  fromConstr _ = error "fromConstr"
  dataTypeOf _ = mkNorepType "GHC.IOBase.IORef"


------------------------------------------------------------------------------
