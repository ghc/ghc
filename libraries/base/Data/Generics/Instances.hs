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
-- (This module does not export anything. It really just defines instances.)
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
import GHC.ForeignPtr	     -- So we can give Data instance for ForeignPtr
import GHC.Stable	     -- So we can give Data instance for StablePtr
import GHC.ST	     	     -- So we can give Data instance for ST
import GHC.Conc		     -- So we can give Data instance for MVar & Co.
import GHC.Arr		     -- So we can give Data instance for Array

#include "Typeable.h"


 
------------------------------------------------------------------------------
--
--	Instances of the Data class for Prelude-like types.
--	We define top-level definitions for representations.
--
------------------------------------------------------------------------------


falseConstr  = mkConstr boolDataType "False" [] Prefix
trueConstr   = mkConstr boolDataType "True"  [] Prefix
boolDataType = mkDataType "Prelude.Bool" [falseConstr,trueConstr]


instance Data Bool where
  toConstr False = falseConstr
  toConstr True  = trueConstr
  gunfold k z c  = case constrIndex c of
                     1 -> z False
                     2 -> z True
                     _ -> error "gunfold"
  dataTypeOf _ = boolDataType


------------------------------------------------------------------------------


charType = mkStringType "Prelude.Char"

instance Data Char where
  toConstr x = mkStringConstr charType [x]
  gunfold k z c = case constrRep c of
                    (StringConstr [x]) -> z x
                    _ -> error "gunfold"
  dataTypeOf _ = charType


------------------------------------------------------------------------------


floatType = mkFloatType "Prelude.Float"

instance Data Float where
  toConstr x = mkFloatConstr floatType (realToFrac x)
  gunfold k z c = case constrRep c of
                    (FloatConstr x) -> z (realToFrac x)
                    _ -> error "gunfold"
  dataTypeOf _ = floatType


------------------------------------------------------------------------------


doubleType = mkFloatType "Prelude.Double"

instance Data Double where
  toConstr = mkFloatConstr floatType
  gunfold k z c = case constrRep c of
                    (FloatConstr x) -> z x
                    _ -> error "gunfold"
  dataTypeOf _ = doubleType


------------------------------------------------------------------------------


intType = mkIntType "Prelude.Int"

instance Data Int where
  toConstr x = mkIntConstr intType (fromIntegral x)
  gunfold k z c = case constrRep c of
                    (IntConstr x) -> z (fromIntegral x)
                    _ -> error "gunfold"
  dataTypeOf _ = intType


------------------------------------------------------------------------------


integerType = mkIntType "Prelude.Integer"

instance Data Integer where
  toConstr = mkIntConstr integerType
  gunfold k z c = case constrRep c of
                    (IntConstr x) -> z x
                    _ -> error "gunfold"
  dataTypeOf _ = integerType


------------------------------------------------------------------------------


int8Type = mkIntType "Data.Int.Int8"

instance Data Int8 where
  toConstr x = mkIntConstr int8Type (fromIntegral x)
  gunfold k z c = case constrRep c of
                    (IntConstr x) -> z (fromIntegral x)
                    _ -> error "gunfold"
  dataTypeOf _ = int8Type


------------------------------------------------------------------------------


int16Type = mkIntType "Data.Int.Int16"

instance Data Int16 where
  toConstr x = mkIntConstr int16Type (fromIntegral x)
  gunfold k z c = case constrRep c of
                    (IntConstr x) -> z (fromIntegral x)
                    _ -> error "gunfold"
  dataTypeOf _ = int16Type


------------------------------------------------------------------------------


int32Type = mkIntType "Data.Int.Int32"

instance Data Int32 where
  toConstr x = mkIntConstr int32Type (fromIntegral x)
  gunfold k z c = case constrRep c of
                    (IntConstr x) -> z (fromIntegral x)
                    _ -> error "gunfold"
  dataTypeOf _ = int32Type


------------------------------------------------------------------------------


int64Type = mkIntType "Data.Int.Int64"

instance Data Int64 where
  toConstr x = mkIntConstr int64Type (fromIntegral x)
  gunfold k z c = case constrRep c of
                    (IntConstr x) -> z (fromIntegral x)
                    _ -> error "gunfold"
  dataTypeOf _ = int64Type


------------------------------------------------------------------------------


wordType = mkIntType "Data.Word.Word"

instance Data Word where
  toConstr x = mkIntConstr wordType (fromIntegral x)
  gunfold k z c = case constrRep c of
                    (IntConstr x) -> z (fromIntegral x)
                    _ -> error "gunfold"
  dataTypeOf _ = wordType


------------------------------------------------------------------------------


word8Type = mkIntType "Data.Word.Word8"

instance Data Word8 where
  toConstr x = mkIntConstr word8Type (fromIntegral x)
  gunfold k z c = case constrRep c of
                    (IntConstr x) -> z (fromIntegral x)
                    _ -> error "gunfold"
  dataTypeOf _ = word8Type


------------------------------------------------------------------------------


word16Type = mkIntType "Data.Word.Word16"

instance Data Word16 where
  toConstr x = mkIntConstr word16Type (fromIntegral x)
  gunfold k z c = case constrRep c of
                    (IntConstr x) -> z (fromIntegral x)
                    _ -> error "gunfold"
  dataTypeOf _ = word16Type


------------------------------------------------------------------------------


word32Type = mkIntType "Data.Word.Word32"

instance Data Word32 where
  toConstr x = mkIntConstr word32Type (fromIntegral x)
  gunfold k z c = case constrRep c of
                    (IntConstr x) -> z (fromIntegral x)
                    _ -> error "gunfold"
  dataTypeOf _ = word32Type


------------------------------------------------------------------------------


word64Type = mkIntType "Data.Word.Word64"

instance Data Word64 where
  toConstr x = mkIntConstr word64Type (fromIntegral x)
  gunfold k z c = case constrRep c of
                    (IntConstr x) -> z (fromIntegral x)
                    _ -> error "gunfold"
  dataTypeOf _ = word64Type


------------------------------------------------------------------------------


ratioConstr = mkConstr ratioDataType ":%" [] Infix
ratioDataType = mkDataType "GHC.Real.Ratio" [ratioConstr]

instance (Data a, Integral a) => Data (Ratio a) where
  toConstr _ = ratioConstr
  gunfold k z c | constrIndex c == 1 = k (k (z (:%)))
  gunfold _ _ _ = error "gunfold"
  dataTypeOf _  = ratioDataType


------------------------------------------------------------------------------


nilConstr    = mkConstr listDataType "[]" [] Prefix
consConstr   = mkConstr listDataType "(:)" [] Infix
listDataType = mkDataType "Prelude.[]" [nilConstr,consConstr]

instance Data a => Data [a] where
  gfoldl f z []     = z []
  gfoldl f z (x:xs) = z (:) `f` x `f` xs
  toConstr []    = nilConstr
  toConstr (_:_) = consConstr
  gunfold k z c = case constrIndex c of
                    1 -> z []
                    2 -> k (k (z (:)))
                    _ -> error "gunfold"
  dataTypeOf _ = listDataType
  dataCast1    = gcast1

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


nothingConstr = mkConstr maybeDataType "Nothing" [] Prefix
justConstr    = mkConstr maybeDataType "Just"    [] Prefix
maybeDataType = mkDataType "Prelude.Maybe" [nothingConstr,justConstr]

instance Data a => Data (Maybe a) where
  gfoldl f z Nothing  = z Nothing
  gfoldl f z (Just x) = z Just `f` x
  toConstr Nothing  = nothingConstr
  toConstr (Just _) = justConstr
  gunfold k z c = case constrIndex c of
                    1 -> z Nothing
                    2 -> k (z Just)
                    _ -> error "gunfold"
  dataTypeOf _ = maybeDataType
  dataCast1    = gcast1


------------------------------------------------------------------------------


ltConstr         = mkConstr orderingDataType "LT" [] Prefix
eqConstr         = mkConstr orderingDataType "EQ" [] Prefix
gtConstr         = mkConstr orderingDataType "GT" [] Prefix
orderingDataType = mkDataType "Prelude.Ordering" [ltConstr,eqConstr,gtConstr]

instance Data Ordering where
  gfoldl f z LT  = z LT
  gfoldl f z EQ  = z EQ
  gfoldl f z GT  = z GT
  toConstr LT  = ltConstr
  toConstr EQ  = eqConstr
  toConstr GT  = gtConstr
  gunfold k z c = case constrIndex c of
                    1 -> z LT
                    2 -> z EQ
                    3 -> z GT
                    _ -> error "gunfold"
  dataTypeOf _ = orderingDataType


------------------------------------------------------------------------------


leftConstr     = mkConstr eitherDataType "Left"  [] Prefix
rightConstr    = mkConstr eitherDataType "Right" [] Prefix
eitherDataType = mkDataType "Prelude.Either" [leftConstr,rightConstr]

instance (Data a, Data b) => Data (Either a b) where
  gfoldl f z (Left a)   = z Left  `f` a
  gfoldl f z (Right a)  = z Right `f` a
  toConstr (Left _)  = leftConstr
  toConstr (Right _) = rightConstr
  gunfold k z c = case constrIndex c of
                    1 -> k (z Left)
                    2 -> k (z Right)
                    _ -> error "gunfold"
  dataTypeOf _ = eitherDataType
  dataCast2    = gcast2


------------------------------------------------------------------------------


--
-- A last resort for functions
--

instance (Data a, Data b) => Data (a -> b) where
  toConstr _   = error "toConstr"
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = mkNorepType "Prelude.(->)"
  dataCast2    = gcast2


------------------------------------------------------------------------------


tuple0Constr = mkConstr tuple0DataType "()" [] Prefix
tuple0DataType = mkDataType "Prelude.()" [tuple0Constr]

instance Data () where
  toConstr _    = tuple0Constr
  gunfold k z c | constrIndex c == 1 = z ()  
  gunfold _ _ _ = error "gunfold"
  dataTypeOf _  = tuple0DataType


------------------------------------------------------------------------------


tuple2Constr = mkConstr tuple2DataType "(,)" [] Infix
tuple2DataType = mkDataType "Prelude.(,)" [tuple2Constr]

instance (Data a, Data b) => Data (a,b) where
  gfoldl f z (a,b) = z (,) `f` a `f` b
  toConstr _    = tuple2Constr
  gunfold k z c | constrIndex c == 1 = k (k (z (,)))
  gunfold _ _ _ = error "gunfold"
  dataTypeOf _  = tuple2DataType
  dataCast2     = gcast2


------------------------------------------------------------------------------


tuple3Constr = mkConstr tuple3DataType "(,,)" [] Infix
tuple3DataType = mkDataType "Prelude.(,)" [tuple3Constr]

instance (Data a, Data b, Data c) => Data (a,b,c) where
  gfoldl f z (a,b,c) = z (,,) `f` a `f` b `f` c
  toConstr _    = tuple3Constr
  gunfold k z c | constrIndex c == 1 = k (k (k (z (,,))))
  gunfold _ _ _ = error "gunfold"
  dataTypeOf _  = tuple3DataType


------------------------------------------------------------------------------


tuple4Constr = mkConstr tuple4DataType "(,,,)" [] Infix
tuple4DataType = mkDataType "Prelude.(,,,)" [tuple4Constr]

instance (Data a, Data b, Data c, Data d)
         => Data (a,b,c,d) where
  gfoldl f z (a,b,c,d) = z (,,,) `f` a `f` b `f` c `f` d
  toConstr _ = tuple4Constr
  gunfold k z c = case constrIndex c of
                    1 -> k (k (k (k (z (,,,)))))
                    _ -> error "gunfold"
  dataTypeOf _ = tuple4DataType


------------------------------------------------------------------------------


tuple5Constr = mkConstr tuple5DataType "(,,,,)" [] Infix
tuple5DataType = mkDataType "Prelude.(,,,,)" [tuple5Constr]

instance (Data a, Data b, Data c, Data d, Data e)
         => Data (a,b,c,d,e) where
  gfoldl f z (a,b,c,d,e) = z (,,,,) `f` a `f` b `f` c `f` d `f` e
  toConstr _ = tuple5Constr
  gunfold k z c = case constrIndex c of
                    1 -> k (k (k (k (k (z (,,,,))))))
                    _ -> error "gunfold"
  dataTypeOf _ = tuple5DataType


------------------------------------------------------------------------------


tuple6Constr = mkConstr tuple6DataType "(,,,,,)" [] Infix
tuple6DataType = mkDataType "Prelude.(,,,,,)" [tuple6Constr]

instance (Data a, Data b, Data c, Data d, Data e, Data f)
         => Data (a,b,c,d,e,f) where
  gfoldl f z (a,b,c,d,e,f') = z (,,,,,) `f` a `f` b `f` c `f` d `f` e `f` f'
  toConstr _ = tuple6Constr
  gunfold k z c = case constrIndex c of
                    1 -> k (k (k (k (k (k (z (,,,,,)))))))
                    _ -> error "gunfold"
  dataTypeOf _ = tuple6DataType


------------------------------------------------------------------------------


tuple7Constr = mkConstr tuple7DataType "(,,,,,,)" [] Infix
tuple7DataType = mkDataType "Prelude.(,,,,,,)" [tuple7Constr]

instance (Data a, Data b, Data c, Data d, Data e, Data f, Data g)
         => Data (a,b,c,d,e,f,g) where
  gfoldl f z (a,b,c,d,e,f',g) =
    z (,,,,,,) `f` a `f` b `f` c `f` d `f` e `f` f' `f` g
  toConstr _ = tuple7Constr
  gunfold k z c = case constrIndex c of
                    1 -> k (k (k (k (k (k (k (z (,,,,,,))))))))
                    _ -> error "gunfold"
  dataTypeOf _ = tuple7DataType


------------------------------------------------------------------------------


instance Data TypeRep where
  toConstr _   = error "toConstr"
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = mkNorepType "Data.Typeable.TypeRep"


------------------------------------------------------------------------------


instance Data TyCon where
  toConstr _   = error "toConstr"
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = mkNorepType "Data.Typeable.TyCon"


------------------------------------------------------------------------------


INSTANCE_TYPEABLE0(DataType,dataTypeTc,"DataType")

instance Data DataType where
  toConstr _   = error "toConstr"
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = mkNorepType "Data.Generics.Basics.DataType"


------------------------------------------------------------------------------


instance Typeable a => Data (IO a) where
  toConstr _   = error "toConstr"
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = mkNorepType "GHC.IOBase.IO"


------------------------------------------------------------------------------


instance Data Handle where
  toConstr _   = error "toConstr"
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = mkNorepType "GHC.IOBase.Handle"


------------------------------------------------------------------------------


instance Typeable a => Data (Ptr a) where
  toConstr _   = error "toConstr"
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = mkNorepType "GHC.Ptr.Ptr"


------------------------------------------------------------------------------


instance Typeable a => Data (StablePtr a) where
  toConstr _   = error "toConstr"
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = mkNorepType "GHC.Stable.StablePtr"


------------------------------------------------------------------------------


instance Typeable a => Data (IORef a) where
  toConstr _   = error "toConstr"
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = mkNorepType "GHC.IOBase.IORef"


------------------------------------------------------------------------------


instance Typeable a => Data (ForeignPtr a) where
  toConstr _   = error "toConstr"
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = mkNorepType "GHC.ForeignPtr.ForeignPtr"


------------------------------------------------------------------------------


instance (Typeable s, Typeable a) => Data (ST s a) where
  toConstr _   = error "toConstr"
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = mkNorepType "GHC.ST.ST"


------------------------------------------------------------------------------


instance Data ThreadId where
  toConstr _   = error "toConstr"
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = mkNorepType "GHC.Conc.ThreadId"


------------------------------------------------------------------------------


instance Typeable a => Data (TVar a) where
  toConstr _   = error "toConstr"
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = mkNorepType "GHC.Conc.TVar"


------------------------------------------------------------------------------


instance Typeable a => Data (MVar a) where
  toConstr _   = error "toConstr"
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = mkNorepType "GHC.Conc.MVar"


------------------------------------------------------------------------------


instance Typeable a => Data (STM a) where
  toConstr _   = error "toConstr"
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = mkNorepType "GHC.Conc.STM"


------------------------------------------------------------------------------
-- The Data instance for Array preserves data abstraction at the cost of inefficiency.
-- We omit reflection services for the sake of data abstraction.
instance (Typeable a, Data b, Ix a) => Data (Array a b)
 where
  gfoldl f z a = z (listArray (bounds a)) `f` (elems a)
  toConstr _   = error "toConstr"
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = mkNorepType "Data.Array.Array"

