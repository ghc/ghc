{-# OPTIONS -fno-implicit-prelude #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Dynamic
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- The Dynamic interface provides basic support for dynamic types.
-- 
-- Operations for injecting values of arbitrary type into
-- a dynamically typed value, Dynamic, are provided, together
-- with operations for converting dynamic values into a concrete
-- (monomorphic) type.
-- 
-- The Dynamic implementation provided is closely based on code
-- contained in Hugs library of the same name.
-- 
-----------------------------------------------------------------------------

module Data.Dynamic
	(
	-- dynamic type
	  Dynamic	-- abstract, instance of: Show, Typeable
	, toDyn		-- :: Typeable a => a -> Dynamic
	, fromDyn	-- :: Typeable a => Dynamic -> a -> a
	, fromDynamic	-- :: Typeable a => Dynamic -> Maybe a
	
	-- type representation

	, Typeable(
	     typeOf)	-- :: a -> TypeRep

	  -- Dynamic defines Typeable instances for the following
	-- Prelude types: [a], (), (a,b), (a,b,c), (a,b,c,d),
	-- (a,b,c,d,e), (a->b), (Array a b), Bool, Char,
	-- (Complex a), Double, (Either a b), Float, Handle,
	-- Int, Integer, (IO a), (Maybe a), Ordering

	, TypeRep	-- abstract, instance of: Eq, Show, Typeable
	, TyCon		-- abstract, instance of: Eq, Show, Typeable

	-- type representation constructors/operators:
	, mkTyCon	-- :: String  -> TyCon
	, mkAppTy	-- :: TyCon   -> [TypeRep] -> TypeRep
	, mkFunTy	-- :: TypeRep -> TypeRep   -> TypeRep
	, applyTy	-- :: TypeRep -> TypeRep   -> Maybe TypeRep

	-- 
	-- let fTy = mkTyCon "Foo" in show (mkAppTy (mkTyCon ",,")
	--                                 [fTy,fTy,fTy])
	-- 
	-- returns "(Foo,Foo,Foo)"
	--
	-- The TypeRep Show instance promises to print tuple types
	-- correctly. Tuple type constructors are specified by a 
	-- sequence of commas, e.g., (mkTyCon ",,,,") returns
	-- the 5-tuple tycon.
	) where


import Data.Maybe
import Data.Either
import Data.Int
import Data.Word
import Foreign.Ptr
import Foreign.StablePtr

#ifdef __GLASGOW_HASKELL__
import GHC.Base
import GHC.Show
import GHC.Err
import GHC.Num
import GHC.Float
import GHC.IOBase
#endif

#ifdef __GLASGOW_HASKELL__
unsafeCoerce :: a -> b
unsafeCoerce = unsafeCoerce#
#endif

#include "Dynamic.h"

-- The dynamic type is represented by Dynamic, carrying
-- the dynamic value along with its type representation:

data Dynamic = Dynamic TypeRep Obj

instance Show Dynamic where
   -- the instance just prints the type representation.
   showsPrec _ (Dynamic t _) = 
          showString "<<" . 
	  showsPrec 0 t   . 
	  showString ">>"

data Obj = Obj  
 -- dummy type to hold the dynamically typed value.

data TypeRep
 = App TyCon   [TypeRep]
 | Fun TypeRep TypeRep
   deriving ( Eq )

instance Show TypeRep where
  showsPrec p (App tycon tys) =
    case tys of
      [] -> showsPrec p tycon
      [x] | tycon == listTc    -> showChar '[' . shows x . showChar ']'
      xs  
        | isTupleTyCon tycon -> showTuple tycon xs
	| otherwise	     ->
	    showParen (p > 9) $
   	    showsPrec p tycon . 
	    showChar ' '      . 
	    showArgs tys

  showsPrec p (Fun f a) =
     showParen (p > 8) $
     showsPrec 9 f . showString " -> " . showsPrec 8 a

-- type constructors are 
data TyCon = TyCon Int String

instance Eq TyCon where
  (TyCon t1 _) == (TyCon t2 _) = t1 == t2

instance Show TyCon where
  showsPrec _ (TyCon _ s) = showString s

-- Operations for going to and from Dynamic:

toDyn :: Typeable a => a -> Dynamic
toDyn v = Dynamic (typeOf v) (unsafeCoerce v)

fromDyn :: Typeable a => Dynamic -> a -> a
fromDyn (Dynamic t v) def
  | typeOf def == t = unsafeCoerce v
  | otherwise       = def

fromDynamic :: Typeable a => Dynamic -> Maybe a
fromDynamic (Dynamic t v) =
  case unsafeCoerce v of 
    r | t == typeOf r -> Just r
      | otherwise     -> Nothing

-- To make it possible to convert values with user-defined types
-- into type Dynamic, we need a systematic way of getting
-- the type representation of an arbitrary type. A type
-- class provides just the ticket,

class Typeable a where
  typeOf :: a -> TypeRep

-- NOTE: The argument to the overloaded `typeOf' is only
-- used to carry type information, and Typeable instances
-- should *never* *ever* look at its value.

isTupleTyCon :: TyCon -> Bool
isTupleTyCon (TyCon _ (',':_)) = True
isTupleTyCon _		       = False

-- If we enforce the restriction that there is only one
-- @TyCon@ for a type & it is shared among all its uses,
-- we can map them onto Ints very simply. The benefit is,
-- of course, that @TyCon@s can then be compared efficiently.

-- Provided the implementor of other @Typeable@ instances
-- takes care of making all the @TyCon@s CAFs (toplevel constants),
-- this will work. 

-- If this constraint does turn out to be a sore thumb, changing
-- the Eq instance for TyCons is trivial.

mkTyCon :: String -> TyCon
mkTyCon str = unsafePerformIO $ do
   v <- readIORef uni
   writeIORef uni (v+1)
   return (TyCon v str)

{-# NOINLINE uni #-}
uni :: IORef Int
uni = unsafePerformIO ( newIORef 0 )

-- Some (Show.TypeRep) helpers:

showArgs :: Show a => [a] -> ShowS
showArgs [] = id
showArgs [a] = showsPrec 10 a
showArgs (a:as) = showsPrec 10 a . showString " " . showArgs as 

showTuple :: TyCon -> [TypeRep] -> ShowS
showTuple (TyCon _ str) args = showChar '(' . go str args
 where
  go [] [a] = showsPrec 10 a . showChar ')'
  go _  []  = showChar ')' -- a failure condition, really.
  go (',':xs) (a:as) = showsPrec 10 a . showChar ',' . go xs as
  go _ _   = showChar ')'


mkAppTy  :: TyCon   -> [TypeRep] -> TypeRep
mkAppTy tyc args = App tyc args

mkFunTy  :: TypeRep -> TypeRep   -> TypeRep
mkFunTy f a = Fun f a

-- Auxillary functions

-- (f::(a->b)) `dynApply` (x::a) = (f a)::b
dynApply :: Dynamic -> Dynamic -> Maybe Dynamic
dynApply (Dynamic t1 f) (Dynamic t2 x) =
  case applyTy t1 t2 of
    Just t3 -> Just (Dynamic t3 ((unsafeCoerce f) x))
    Nothing -> Nothing

dynApp :: Dynamic -> Dynamic -> Dynamic
dynApp f x = case dynApply f x of 
             Just r -> r
             Nothing -> error ("Type error in dynamic application.\n" ++
                               "Can't apply function " ++ show f ++
                               " to argument " ++ show x)

applyTy :: TypeRep -> TypeRep -> Maybe TypeRep
applyTy (Fun t1 t2) t3
  | t1 == t3    = Just t2
applyTy _ _     = Nothing

-- Prelude types

listTc :: TyCon
listTc = mkTyCon "[]"

instance Typeable a => Typeable [a] where
  typeOf ls = mkAppTy listTc [typeOf ((undefined:: [a] -> a) ls)]

unitTc :: TyCon
unitTc = mkTyCon "()"

instance Typeable () where
  typeOf _ = mkAppTy unitTc []

tup2Tc :: TyCon
tup2Tc = mkTyCon ","

instance (Typeable a, Typeable b) => Typeable (a,b) where
  typeOf tu = mkAppTy tup2Tc [typeOf ((undefined :: (a,b) -> a) tu),
			      typeOf ((undefined :: (a,b) -> b) tu)]

tup3Tc :: TyCon
tup3Tc = mkTyCon ",,"

instance ( Typeable a , Typeable b , Typeable c) => Typeable (a,b,c) where
  typeOf tu = mkAppTy tup3Tc [typeOf ((undefined :: (a,b,c) -> a) tu),
			      typeOf ((undefined :: (a,b,c) -> b) tu),
			      typeOf ((undefined :: (a,b,c) -> c) tu)]

tup4Tc :: TyCon
tup4Tc = mkTyCon ",,,"

instance ( Typeable a
	 , Typeable b
	 , Typeable c
	 , Typeable d) => Typeable (a,b,c,d) where
  typeOf tu = mkAppTy tup4Tc [typeOf ((undefined :: (a,b,c,d) -> a) tu),
			      typeOf ((undefined :: (a,b,c,d) -> b) tu),
			      typeOf ((undefined :: (a,b,c,d) -> c) tu),
			      typeOf ((undefined :: (a,b,c,d) -> d) tu)]

tup5Tc :: TyCon
tup5Tc = mkTyCon ",,,,"

instance ( Typeable a
	 , Typeable b
	 , Typeable c
	 , Typeable d
	 , Typeable e) => Typeable (a,b,c,d,e) where
  typeOf tu = mkAppTy tup5Tc [typeOf ((undefined :: (a,b,c,d,e) -> a) tu),
			      typeOf ((undefined :: (a,b,c,d,e) -> b) tu),
			      typeOf ((undefined :: (a,b,c,d,e) -> c) tu),
			      typeOf ((undefined :: (a,b,c,d,e) -> d) tu),
			      typeOf ((undefined :: (a,b,c,d,e) -> e) tu)]

instance (Typeable a, Typeable b) => Typeable (a -> b) where
  typeOf f = mkFunTy (typeOf ((undefined :: (a -> b) -> a) f))
		     (typeOf ((undefined :: (a -> b) -> b) f))

INSTANCE_TYPEABLE0(Bool,boolTc,"Bool")
INSTANCE_TYPEABLE0(Char,charTc,"Char")
INSTANCE_TYPEABLE0(Float,floatTc,"Float")
INSTANCE_TYPEABLE0(Double,doubleTc,"Double")
INSTANCE_TYPEABLE0(Int,intTc,"Int")
INSTANCE_TYPEABLE0(Integer,integerTc,"Integer")
INSTANCE_TYPEABLE2(Either,eitherTc,"Either")
INSTANCE_TYPEABLE1(IO,ioTc,"IO")
INSTANCE_TYPEABLE1(Maybe,maybeTc,"Maybe")
INSTANCE_TYPEABLE0(Ordering,orderingTc,"Ordering")
INSTANCE_TYPEABLE0(Handle,handleTc,"Handle")
INSTANCE_TYPEABLE1(Ptr,ptrTc,"Ptr")
INSTANCE_TYPEABLE1(StablePtr,stablePtrTc,"StablePtr")

INSTANCE_TYPEABLE0(Int8,int8Tc, "Int8")
INSTANCE_TYPEABLE0(Int16,int16Tc,"Int16")
INSTANCE_TYPEABLE0(Int32,int32Tc,"Int32")
INSTANCE_TYPEABLE0(Int64,int64Tc,"Int64")

INSTANCE_TYPEABLE0(Word8,word8Tc, "Word8" )
INSTANCE_TYPEABLE0(Word16,word16Tc,"Word16")
INSTANCE_TYPEABLE0(Word32,word32Tc,"Word32")
INSTANCE_TYPEABLE0(Word64,word64Tc,"Word64")

INSTANCE_TYPEABLE0(TyCon,tyconTc,"TyCon")
INSTANCE_TYPEABLE0(TypeRep,typeRepTc,"TypeRep")
INSTANCE_TYPEABLE0(Dynamic,dynamicTc,"Dynamic")
