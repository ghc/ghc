module PreludeBuiltin (
	Char(..),
	Int(..),
	_Word(..),
	_Addr(..),
	Float(..),
	Double(..),
	_State(..),
	_MallocPtr(..),
	Integer(..),
	_Return2GMPs(..),
	_ReturnIntAndGMP(..),
	StateAndPtr#(..),
	StateAndChar#(..),
	StateAndInt#(..),
	StateAndWord#(..),
	StateAndFloat#(..),
	StateAndDouble#(..),
	StateAndAddr#(..),
	StateAndMallocPtr#(..),
	StateAndArray#(..),
	StateAndMutableArray#(..),
	StateAndByteArray#(..),
	StateAndMutableByteArray#(..),
	StateAndSynchVar#(..),
#ifndef __PARALLEL_HASKELL__
	_StablePtr(..),
	StateAndStablePtr#(..),
#endif
	_CMP_TAG(..),
	List(..),
	Tuple0(..),
	Tuple2(..),
	Tuple3(..),
	Tuple4(..),
	Tuple5(..),
	Tuple6(..),
	Tuple7(..),
	Tuple8(..),
	Tuple9(..),
	Tuple10(..),
	Tuple11(..),
	Tuple12(..),
	Tuple13(..),
	Tuple14(..),
	Tuple15(..),
	Tuple16(..),
	Tuple17(..),
	Tuple18(..),
	Tuple19(..),
	Tuple20(..),
	Tuple21(..),
	Tuple22(..),
	Tuple23(..),
	Tuple24(..),
	Tuple25(..),
	Tuple26(..),
	Tuple27(..),
	Tuple28(..),
	Tuple29(..),
	Tuple30(..),
	Tuple31(..),
	Tuple32(..),
	Bin(..),
	_Lift(..)
    ) where

-- *********
-- This follows the organisation of compiler/prelude/TysWiredIn.lhs
-- (which follows the state-interface.verb document).
-- *********

-- instances are all done carefully, by hand (in the prelude/I*.hs files)

----------------------------------------------------

data Char   = C# Char#
data Int    = I# Int#
data _Word  = W# Word#
data _Addr  = A# Addr#
data Float  = F# Float#
data Double = D# Double#

data _State a = S# (State# a)

----------------------------------------------------

-- Array is from PreludeArray & so can't be defined here.
-- _ByteArray, _MutableArray & _MutableByteArray are from PreludeGlaArray ...

data _MallocPtr = _MallocPtr MallocPtr#
#ifndef __PARALLEL_HASKELL__
data _StablePtr a = _StablePtr (StablePtr# a)
#endif

----------------------------------------------------

data Integer = J# Int# Int# ByteArray#
	-- corresponds to GNU multi-prec arith pkg rep

-- Old:
--   Oddly enough, the Return<blah>GMP<blah> don't need definitions,
--   because they are only used in "returning" (and so are never
--   actually built).
-- New:
--   Unless you want to write your own code using the Integer primops
--   -- at which point it becomes essential to have these types. ADR

data _Return2GMPs = _Return2GMPs Int# Int# ByteArray# Int# Int# ByteArray#
data _ReturnIntAndGMP = _ReturnIntAndGMP Int# Int# Int# ByteArray#

----------------------------------------------------

data StateAndPtr#    s elt = StateAndPtr#    (State# s) elt 

data StateAndChar#   s     = StateAndChar#   (State# s) Char# 
data StateAndInt#    s     = StateAndInt#    (State# s) Int# 
data StateAndWord#   s     = StateAndWord#   (State# s) Word#
data StateAndFloat#  s     = StateAndFloat#  (State# s) Float# 
data StateAndDouble# s     = StateAndDouble# (State# s) Double#  
data StateAndAddr#   s     = StateAndAddr#   (State# s) Addr#

#ifndef __PARALLEL_HASKELL__
data StateAndStablePtr# s a = StateAndStablePtr# (State# s) (StablePtr# a)
#endif
data StateAndMallocPtr# s   = StateAndMallocPtr# (State# s) MallocPtr#

data StateAndArray#            s elt = StateAndArray#        (State# s) (Array# elt) 
data StateAndMutableArray#     s elt = StateAndMutableArray# (State# s) (MutableArray# s elt)  
data StateAndByteArray#        s = StateAndByteArray#        (State# s) ByteArray# 
data StateAndMutableByteArray# s = StateAndMutableByteArray# (State# s) (MutableByteArray# s)

data StateAndSynchVar# s elt = StateAndSynchVar# (State# s) (SynchVar# s elt)

----------------------------------------------------
-- Bool: is from PreludeCore

data _CMP_TAG = _LT | _EQ | _GT -- for derived comparisons

----------------------------------------------------

data List a = Nil | a : (List a)

----------------------------------------------------

data Tuple0  				     = Tup0
data Tuple2  a b			     = Tup2  a b
data Tuple3  a b c			     = Tup3  a b c
data Tuple4  a b c d			     = Tup4  a b c d
data Tuple5  a b c d e			     = Tup5  a b c d e
data Tuple6  a b c d e f		     = Tup6  a b c d e f
data Tuple7  a b c d e f g  	    	     = Tup7  a b c d e f g
data Tuple8  a b c d e f g h 	    	     = Tup8  a b c d e f g h
data Tuple9  a b c d e f g h i 	    	     = Tup9  a b c d e f g h i
data Tuple10 a b c d e f g h i j    	     = Tup10 a b c d e f g h i j
data Tuple11 a b c d e f g h i j k  	     = Tup11 a b c d e f g h i j k
data Tuple12 a b c d e f g h i j k l 	     = Tup12 a b c d e f g h i j k l
data Tuple13 a b c d e f g h i j k l m 	     = Tup13 a b c d e f g h i j k l m
data Tuple14 a b c d e f g h i j k l m n     = Tup14 a b c d e f g h i j k l m n
data Tuple15 a b c d e f g h i j k l m n o   = Tup15 a b c d e f g h i j k l m n o
data Tuple16 a b c d e f g h i j k l m n o p = Tup16 a b c d e f g h i j k l m n o p
data Tuple17 a b c d e f g h i j k l m n o p q
 = Tup17 a b c d e f g h i j k l m n o p q
data Tuple18 a b c d e f g h i j k l m n o p q r
 = Tup18 a b c d e f g h i j k l m n o p q r
data Tuple19 a b c d e f g h i j k l m n o p q r s
 = Tup19 a b c d e f g h i j k l m n o p q r s
data Tuple20 a b c d e f g h i j k l m n o p q r s t
 = Tup20 a b c d e f g h i j k l m n o p q r s t
data Tuple21 a b c d e f g h i j k l m n o p q r s t u
 = Tup21 a b c d e f g h i j k l m n o p q r s t u
data Tuple22 a b c d e f g h i j k l m n o p q r s t u v
 = Tup22 a b c d e f g h i j k l m n o p q r s t u v
data Tuple23 a b c d e f g h i j k l m n o p q r s t u v w
 = Tup23 a b c d e f g h i j k l m n o p q r s t u v w
data Tuple24 a b c d e f g h i j k l m n o p q r s t u v w x
 = Tup24 a b c d e f g h i j k l m n o p q r s t u v w x
data Tuple25 a b c d e f g h i j k l m n o p q r s t u v w x y
 = Tup25 a b c d e f g h i j k l m n o p q r s t u v w x y
data Tuple26 a b c d e f g h i j k l m n o p q r s t u v w x y z
 = Tup26 a b c d e f g h i j k l m n o p q r s t u v w x y z
data Tuple27 a b c d e f g h i j k l m n o p q r s t u v w x y z a_
 = Tup27 a b c d e f g h i j k l m n o p q r s t u v w x y z a_
data Tuple28 a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_
 = Tup28 a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_
data Tuple29 a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_
 = Tup29 a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_
data Tuple30 a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_
 = Tup30 a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_
data Tuple31 a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_
 = Tup31 a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_
data Tuple32 a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_
 = Tup32 a b c d e f g h i j k l m n o p q r s t u v w x y z a_ b_ c_ d_ e_ f_

----------------------------------------------------
-- Ratio: in PreludeRatio

data Bin = Bin_____________ -- placeholder only

-- IO things: in PreludeIO

--{-# SPECIALIZE data _Lift (State# _RealWorld) #-}

data _Lift a = _Lift a
