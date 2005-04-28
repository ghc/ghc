%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[ConFold]{Constant Folder}

Conceptually, constant folding should be parameterized with the kind
of target machine to get identical behaviour during compilation time
and runtime. We cheat a little bit here...

ToDo:
   check boundaries before folding, e.g. we can fold the Float addition
   (i1 + i2) only if it results	in a valid Float.

\begin{code}

{-# OPTIONS -optc-DNON_POSIX_SOURCE #-}

module PrelRules ( primOpRules, builtinRules ) where

#include "HsVersions.h"

import CoreSyn
import Id		( mkWildId, isPrimOpId_maybe )
import Literal		( Literal(..), mkMachInt, mkMachWord
			, literalType
			, word2IntLit, int2WordLit
			, narrow8IntLit, narrow16IntLit, narrow32IntLit
			, narrow8WordLit, narrow16WordLit, narrow32WordLit
			, char2IntLit, int2CharLit
			, float2IntLit, int2FloatLit, double2IntLit, int2DoubleLit
			, float2DoubleLit, double2FloatLit
			)
import PrimOp		( PrimOp(..), primOpOcc )
import TysWiredIn	( boolTy, trueDataConId, falseDataConId )
import TyCon		( tyConDataCons_maybe, isEnumerationTyCon, isNewTyCon )
import DataCon		( dataConTag, dataConTyCon, dataConWorkId, fIRST_TAG )
import CoreUtils	( cheapEqExpr, exprIsConApp_maybe )
import Type		( tyConAppTyCon, coreEqType )
import OccName		( occNameUserString)
import PrelNames	( unpackCStringFoldrName, unpackCStringFoldrIdKey, hasKey,
			  eqStringName, unpackCStringIdKey )
import Maybes		( orElse )
import Name		( Name )
import Outputable
import FastString
import StaticFlags      ( opt_SimplExcessPrecision )

import DATA_BITS	( Bits(..) )
#if __GLASGOW_HASKELL__ >= 500
import DATA_WORD	( Word )
#else
import DATA_WORD	( Word64 )
#endif
\end{code}


\begin{code}
primOpRules :: PrimOp -> Name -> [CoreRule]
primOpRules op op_name = primop_rule op
  where
    rule_name = mkFastString (occNameUserString (primOpOcc op))
    rule_name_case = rule_name `appendFS` FSLIT("->case")

	-- A useful shorthand
    one_rule rule_fn = [BuiltinRule { ru_name = rule_name, 
				      ru_fn = op_name, 
				      ru_try = rule_fn }]
    case_rule rule_fn = [BuiltinRule { ru_name = rule_name_case, 
				       ru_fn = op_name, 
				       ru_try = rule_fn }]

    -- ToDo:	something for integer-shift ops?
    --		NotOp

    primop_rule TagToEnumOp = one_rule tagToEnumRule
    primop_rule DataToTagOp = one_rule dataToTagRule

	-- Int operations
    primop_rule IntAddOp    = one_rule (twoLits (intOp2     (+)))
    primop_rule IntSubOp    = one_rule (twoLits (intOp2     (-)))
    primop_rule IntMulOp    = one_rule (twoLits (intOp2     (*)))
    primop_rule IntQuotOp   = one_rule (twoLits (intOp2Z    quot))
    primop_rule IntRemOp    = one_rule (twoLits (intOp2Z    rem))
    primop_rule IntNegOp    = one_rule (oneLit  negOp)

	-- Word operations
#if __GLASGOW_HASKELL__ >= 500
    primop_rule WordAddOp   = one_rule (twoLits (wordOp2    (+)))
    primop_rule WordSubOp   = one_rule (twoLits (wordOp2    (-)))
    primop_rule WordMulOp   = one_rule (twoLits (wordOp2    (*)))
#endif
    primop_rule WordQuotOp  = one_rule (twoLits (wordOp2Z   quot))
    primop_rule WordRemOp   = one_rule (twoLits (wordOp2Z   rem))
#if __GLASGOW_HASKELL__ >= 407
    primop_rule AndOp       = one_rule (twoLits (wordBitOp2 (.&.)))
    primop_rule OrOp        = one_rule (twoLits (wordBitOp2 (.|.)))
    primop_rule XorOp       = one_rule (twoLits (wordBitOp2 xor))
#endif

	-- coercions
    primop_rule Word2IntOp 	= one_rule (oneLit (litCoerce word2IntLit))
    primop_rule Int2WordOp 	= one_rule (oneLit (litCoerce int2WordLit))
    primop_rule Narrow8IntOp 	= one_rule (oneLit (litCoerce narrow8IntLit))
    primop_rule Narrow16IntOp 	= one_rule (oneLit (litCoerce narrow16IntLit))
    primop_rule Narrow32IntOp 	= one_rule (oneLit (litCoerce narrow32IntLit))
    primop_rule Narrow8WordOp 	= one_rule (oneLit (litCoerce narrow8WordLit))
    primop_rule Narrow16WordOp 	= one_rule (oneLit (litCoerce narrow16WordLit))
    primop_rule Narrow32WordOp 	= one_rule (oneLit (litCoerce narrow32WordLit))
    primop_rule OrdOp   	= one_rule (oneLit (litCoerce char2IntLit))
    primop_rule ChrOp    	= one_rule (oneLit (litCoerce int2CharLit))
    primop_rule Float2IntOp	= one_rule (oneLit (litCoerce float2IntLit))
    primop_rule Int2FloatOp	= one_rule (oneLit (litCoerce int2FloatLit))
    primop_rule Double2IntOp	= one_rule (oneLit (litCoerce double2IntLit))
    primop_rule Int2DoubleOp	= one_rule (oneLit (litCoerce int2DoubleLit))
	-- SUP: Not sure what the standard says about precision in the following 2 cases
    primop_rule Float2DoubleOp 	= one_rule (oneLit (litCoerce float2DoubleLit))
    primop_rule Double2FloatOp 	= one_rule (oneLit (litCoerce double2FloatLit))

	-- Float
    primop_rule FloatAddOp   = one_rule (twoLits (floatOp2  (+)))
    primop_rule FloatSubOp   = one_rule (twoLits (floatOp2  (-)))
    primop_rule FloatMulOp   = one_rule (twoLits (floatOp2  (*)))
    primop_rule FloatDivOp   = one_rule (twoLits (floatOp2Z (/)))
    primop_rule FloatNegOp   = one_rule (oneLit  negOp)

	-- Double
    primop_rule DoubleAddOp   = one_rule (twoLits (doubleOp2  (+)))
    primop_rule DoubleSubOp   = one_rule (twoLits (doubleOp2  (-)))
    primop_rule DoubleMulOp   = one_rule (twoLits (doubleOp2  (*)))
    primop_rule DoubleDivOp   = one_rule (twoLits (doubleOp2Z (/)))
    primop_rule DoubleNegOp   = one_rule (oneLit  negOp)

	-- Relational operators
    primop_rule IntEqOp  = one_rule (relop (==)) ++ case_rule (litEq True)
    primop_rule IntNeOp  = one_rule (relop (/=)) ++ case_rule (litEq False)
    primop_rule CharEqOp = one_rule (relop (==)) ++ case_rule (litEq True)
    primop_rule CharNeOp = one_rule (relop (/=)) ++ case_rule (litEq False)

    primop_rule IntGtOp		= one_rule (relop (>))
    primop_rule IntGeOp		= one_rule (relop (>=))
    primop_rule IntLeOp		= one_rule (relop (<=))
    primop_rule IntLtOp		= one_rule (relop (<))

    primop_rule CharGtOp	= one_rule (relop (>))
    primop_rule CharGeOp	= one_rule (relop (>=))
    primop_rule CharLeOp	= one_rule (relop (<=))
    primop_rule CharLtOp	= one_rule (relop (<))

    primop_rule FloatGtOp	= one_rule (relop (>))
    primop_rule FloatGeOp	= one_rule (relop (>=))
    primop_rule FloatLeOp	= one_rule (relop (<=))
    primop_rule FloatLtOp	= one_rule (relop (<))
    primop_rule FloatEqOp	= one_rule (relop (==))
    primop_rule FloatNeOp	= one_rule (relop (/=))

    primop_rule DoubleGtOp	= one_rule (relop (>))
    primop_rule DoubleGeOp	= one_rule (relop (>=))
    primop_rule DoubleLeOp	= one_rule (relop (<=))
    primop_rule DoubleLtOp	= one_rule (relop (<))
    primop_rule DoubleEqOp	= one_rule (relop (==))
    primop_rule DoubleNeOp	= one_rule (relop (/=))

    primop_rule WordGtOp	= one_rule (relop (>))
    primop_rule WordGeOp	= one_rule (relop (>=))
    primop_rule WordLeOp	= one_rule (relop (<=))
    primop_rule WordLtOp	= one_rule (relop (<))
    primop_rule WordEqOp	= one_rule (relop (==))
    primop_rule WordNeOp	= one_rule (relop (/=))

    primop_rule other		= []


    relop cmp = twoLits (cmpOp (\ord -> ord `cmp` EQ))
	-- Cunning.  cmpOp compares the values to give an Ordering.
	-- It applies its argument to that ordering value to turn
	-- the ordering into a boolean value.  (`cmp` EQ) is just the job.
\end{code}

%************************************************************************
%*									*
\subsection{Doing the business}
%*									*
%************************************************************************

ToDo: the reason these all return Nothing is because there used to be
the possibility of an argument being a litlit.  Litlits are now gone,
so this could be cleaned up.

\begin{code}
--------------------------
litCoerce :: (Literal -> Literal) -> Literal -> Maybe CoreExpr
litCoerce fn lit = Just (Lit (fn lit))

--------------------------
cmpOp :: (Ordering -> Bool) -> Literal -> Literal -> Maybe CoreExpr
cmpOp cmp l1 l2
  = go l1 l2
  where
    done res | cmp res   = Just trueVal
	     | otherwise = Just falseVal

	-- These compares are at different types
    go (MachChar i1)   (MachChar i2)   = done (i1 `compare` i2)
    go (MachInt i1)    (MachInt i2)    = done (i1 `compare` i2)
    go (MachInt64 i1)  (MachInt64 i2)  = done (i1 `compare` i2)
    go (MachWord i1)   (MachWord i2)   = done (i1 `compare` i2)
    go (MachWord64 i1) (MachWord64 i2) = done (i1 `compare` i2)
    go (MachFloat i1)  (MachFloat i2)  = done (i1 `compare` i2)
    go (MachDouble i1) (MachDouble i2) = done (i1 `compare` i2)
    go l1	       l2	       = Nothing

--------------------------

negOp (MachFloat 0.0) = Nothing  -- can't represent -0.0 as a Rational
negOp (MachFloat f)   = Just (mkFloatVal (-f))
negOp (MachDouble 0.0) = Nothing
negOp (MachDouble d)   = Just (mkDoubleVal (-d))
negOp (MachInt i)      = intResult (-i)
negOp l		       = Nothing

--------------------------
intOp2 op (MachInt i1) (MachInt i2) = intResult (i1 `op` i2)
intOp2 op l1	       l2	    = Nothing		-- Could find LitLit

intOp2Z op (MachInt i1) (MachInt i2)
  | i2 /= 0 = Just (mkIntVal (i1 `op` i2))
intOp2Z op l1 l2 = Nothing		-- LitLit or zero dividend

--------------------------
#if __GLASGOW_HASKELL__ >= 500
wordOp2 op (MachWord w1) (MachWord w2)
  = wordResult (w1 `op` w2)
wordOp2 op l1 l2 = Nothing		-- Could find LitLit
#endif

wordOp2Z op (MachWord w1) (MachWord w2)
  | w2 /= 0 = Just (mkWordVal (w1 `op` w2))
wordOp2Z op l1 l2 = Nothing	-- LitLit or zero dividend

#if __GLASGOW_HASKELL__ >= 500
wordBitOp2 op l1@(MachWord w1) l2@(MachWord w2)
  = Just (mkWordVal (w1 `op` w2))
#else
-- Integer is not an instance of Bits, so we operate on Word64
wordBitOp2 op l1@(MachWord w1) l2@(MachWord w2)
  = Just (mkWordVal ((fromIntegral::Word64->Integer) (fromIntegral w1 `op` fromIntegral w2)))
#endif
wordBitOp2 op l1 l2 = Nothing		-- Could find LitLit

--------------------------
floatOp2  op (MachFloat f1) (MachFloat f2)
  = Just (mkFloatVal (f1 `op` f2))
floatOp2  op l1 l2 = Nothing

floatOp2Z op (MachFloat f1) (MachFloat f2)
  | f2 /= 0   = Just (mkFloatVal (f1 `op` f2))
floatOp2Z op l1 l2 = Nothing

--------------------------
doubleOp2  op (MachDouble f1) (MachDouble f2)
  = Just (mkDoubleVal (f1 `op` f2))
doubleOp2 op l1 l2 = Nothing

doubleOp2Z op (MachDouble f1) (MachDouble f2)
  | f2 /= 0   = Just (mkDoubleVal (f1 `op` f2))
doubleOp2Z op l1 l2 = Nothing


--------------------------
	-- This stuff turns
	--	n ==# 3#
	-- into
	--	case n of
	--	  3# -> True
	--	  m  -> False
	--
	-- This is a Good Thing, because it allows case-of case things
	-- to happen, and case-default absorption to happen.  For
	-- example:
	--
	--	if (n ==# 3#) || (n ==# 4#) then e1 else e2
	-- will transform to
	--	case n of
	--	  3# -> e1
	--	  4# -> e1
	--	  m  -> e2
	-- (modulo the usual precautions to avoid duplicating e1)

litEq :: Bool		-- True <=> equality, False <=> inequality
      -> RuleFun
litEq is_eq [Lit lit, expr] = do_lit_eq is_eq lit expr
litEq is_eq [expr, Lit lit] = do_lit_eq is_eq lit expr
litEq is_eq other	    = Nothing

do_lit_eq is_eq lit expr
  = Just (Case expr (mkWildId (literalType lit)) boolTy
		[(DEFAULT,    [], val_if_neq),
		 (LitAlt lit, [], val_if_eq)])
  where
    val_if_eq  | is_eq     = trueVal
	       | otherwise = falseVal
    val_if_neq | is_eq     = falseVal
	       | otherwise = trueVal

-- Note that we *don't* warn the user about overflow. It's not done at
-- runtime either, and compilation of completely harmless things like
--    ((124076834 :: Word32) + (2147483647 :: Word32))
-- would yield a warning. Instead we simply squash the value into the
-- Int range, but not in a way suitable for cross-compiling... :-(
intResult :: Integer -> Maybe CoreExpr
intResult result
  = Just (mkIntVal (toInteger (fromInteger result :: Int)))

#if __GLASGOW_HASKELL__ >= 500
wordResult :: Integer -> Maybe CoreExpr
wordResult result
  = Just (mkWordVal (toInteger (fromInteger result :: Word)))
#endif
\end{code}


%************************************************************************
%*									*
\subsection{Vaguely generic functions
%*									*
%************************************************************************

\begin{code}
type RuleFun = [CoreExpr] -> Maybe CoreExpr

twoLits :: (Literal -> Literal -> Maybe CoreExpr) -> RuleFun
twoLits rule [Lit l1, Lit l2] = rule (convFloating l1) (convFloating l2)
twoLits rule _                = Nothing

oneLit :: (Literal -> Maybe CoreExpr) -> RuleFun
oneLit rule [Lit l1] = rule (convFloating l1)
oneLit rule _        = Nothing

-- When excess precision is not requested, cut down the precision of the
-- Rational value to that of Float/Double. We confuse host architecture
-- and target architecture here, but it's convenient (and wrong :-).
convFloating :: Literal -> Literal
convFloating (MachFloat  f) | not opt_SimplExcessPrecision =
   MachFloat  (toRational ((fromRational f) :: Float ))
convFloating (MachDouble d) | not opt_SimplExcessPrecision =
   MachDouble (toRational ((fromRational d) :: Double))
convFloating l = l


trueVal       = Var trueDataConId
falseVal      = Var falseDataConId
mkIntVal    i = Lit (mkMachInt  i)
mkWordVal   w = Lit (mkMachWord w)
mkFloatVal  f = Lit (convFloating (MachFloat  f))
mkDoubleVal d = Lit (convFloating (MachDouble d))
\end{code}

						
%************************************************************************
%*									*
\subsection{Special rules for seq, tagToEnum, dataToTag}
%*									*
%************************************************************************

\begin{code}
tagToEnumRule [Type ty, Lit (MachInt i)]
  = ASSERT( isEnumerationTyCon tycon ) 
    case filter correct_tag (tyConDataCons_maybe tycon `orElse` []) of


	[]	  -> Nothing	-- Abstract type
	(dc:rest) -> ASSERT( null rest )
		     Just (Var (dataConWorkId dc))
  where 
    correct_tag dc = (dataConTag dc - fIRST_TAG) == tag
    tag   = fromInteger i
    tycon = tyConAppTyCon ty

tagToEnumRule other = Nothing
\end{code}

For dataToTag#, we can reduce if either 
	
	(a) the argument is a constructor
	(b) the argument is a variable whose unfolding is a known constructor

\begin{code}
dataToTagRule [Type ty1, Var tag_to_enum `App` Type ty2 `App` tag]
  | Just TagToEnumOp <- isPrimOpId_maybe tag_to_enum
  , ty1 `coreEqType` ty2
  = Just tag	-- dataToTag (tagToEnum x)   ==>   x

dataToTagRule [_, val_arg]
  | Just (dc,_) <- exprIsConApp_maybe val_arg
  = ASSERT( not (isNewTyCon (dataConTyCon dc)) )
    Just (mkIntVal (toInteger (dataConTag dc - fIRST_TAG)))

dataToTagRule other = Nothing
\end{code}

%************************************************************************
%*									*
\subsection{Built in rules}
%*									*
%************************************************************************

\begin{code}
builtinRules :: [CoreRule]
-- Rules for non-primops that can't be expressed using a RULE pragma
builtinRules
  = [ BuiltinRule FSLIT("AppendLitString") unpackCStringFoldrName match_append_lit,
      BuiltinRule FSLIT("EqString") eqStringName match_eq_string
    ]


-- The rule is this:
-- 	unpackFoldrCString# "foo" c (unpackFoldrCString# "baz" c n)  =  unpackFoldrCString# "foobaz" c n

match_append_lit [Type ty1,
		   Lit (MachStr s1),
		   c1,
		   Var unpk `App` Type ty2 
		  	    `App` Lit (MachStr s2)
		  	    `App` c2
		  	    `App` n
		  ]
  | unpk `hasKey` unpackCStringFoldrIdKey && 
    c1 `cheapEqExpr` c2
  = ASSERT( ty1 `coreEqType` ty2 )
    Just (Var unpk `App` Type ty1
		   `App` Lit (MachStr (s1 `appendFS` s2))
		   `App` c1
		   `App` n)

match_append_lit other = Nothing

-- The rule is this:
-- 	eqString (unpackCString# (Lit s1)) (unpackCString# (Lit s2) = s1==s2

match_eq_string [Var unpk1 `App` Lit (MachStr s1),
		 Var unpk2 `App` Lit (MachStr s2)]
  | unpk1 `hasKey` unpackCStringIdKey,
    unpk2 `hasKey` unpackCStringIdKey
  = Just (if s1 == s2 then trueVal else falseVal)

match_eq_string other = Nothing
\end{code}		
