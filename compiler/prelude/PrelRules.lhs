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
import Id		( mkWildId, idUnfolding )
import Literal		( Literal(..), mkMachInt, mkMachWord
			, literalType
			, word2IntLit, int2WordLit
			, narrow8IntLit, narrow16IntLit, narrow32IntLit
			, narrow8WordLit, narrow16WordLit, narrow32WordLit
			, char2IntLit, int2CharLit
			, float2IntLit, int2FloatLit, double2IntLit, int2DoubleLit
			, float2DoubleLit, double2FloatLit
			)
import PrimOp		( PrimOp(..), tagToEnumKey )
import TysWiredIn	( boolTy, trueDataConId, falseDataConId )
import TyCon		( tyConDataCons_maybe, isEnumerationTyCon, isNewTyCon )
import DataCon		( dataConTag, dataConTyCon, dataConWorkId, fIRST_TAG )
import CoreUtils	( cheapEqExpr, exprIsConApp_maybe )
import Type		( tyConAppTyCon, coreEqType )
import OccName		( occNameFS )
import PrelNames	( unpackCStringFoldrName, unpackCStringFoldrIdKey, hasKey,
			  eqStringName, unpackCStringIdKey, inlineIdName )
import Maybes		( orElse )
import Name		( Name, nameOccName )
import Outputable
import FastString
import StaticFlags      ( opt_SimplExcessPrecision )

import Data.Bits as Bits	( Bits(..), shiftL, shiftR )
	-- shiftL and shiftR were not always methods of Bits
import Data.Word	( Word )
\end{code}


Note [Constant folding]
~~~~~~~~~~~~~~~~~~~~~~~
primOpRules generates the rewrite rules for each primop
These rules do what is often called "constant folding"
E.g. the rules for +# might say
	     4 +# 5 = 9
Well, of course you'd need a lot of rules if you did it 
like that, so we use a BuiltinRule instead, so that we
can match in any two literal values.  So the rule is really
more like
	     (Lit 4) +# (Lit y) = Lit (x+#y)
where the (+#) on the rhs is done at compile time

That is why these rules are built in here.  Other rules
which don't need to be built in are in GHC.Base. For 
example:
	x +# 0 = x


\begin{code}
primOpRules :: PrimOp -> Name -> [CoreRule]
primOpRules op op_name = primop_rule op
  where
	-- A useful shorthand
    one_lit   = oneLit  op_name
    two_lits  = twoLits op_name
    relop cmp = two_lits (cmpOp (\ord -> ord `cmp` EQ))
	-- Cunning.  cmpOp compares the values to give an Ordering.
	-- It applies its argument to that ordering value to turn
	-- the ordering into a boolean value.  (`cmp` EQ) is just the job.

    -- ToDo:	something for integer-shift ops?
    --		NotOp

    primop_rule TagToEnumOp = mkBasicRule op_name 2 tagToEnumRule
    primop_rule DataToTagOp = mkBasicRule op_name 2 dataToTagRule

	-- Int operations
    primop_rule IntAddOp    = two_lits (intOp2     (+))
    primop_rule IntSubOp    = two_lits (intOp2     (-))
    primop_rule IntMulOp    = two_lits (intOp2     (*))
    primop_rule IntQuotOp   = two_lits (intOp2Z    quot)
    primop_rule IntRemOp    = two_lits (intOp2Z    rem)
    primop_rule IntNegOp    = one_lit  negOp
    primop_rule ISllOp      = two_lits (intShiftOp2 Bits.shiftL)
    primop_rule ISraOp      = two_lits (intShiftOp2 Bits.shiftR)
    primop_rule ISrlOp      = two_lits (intShiftOp2 shiftRightLogical)

	-- Word operations
    primop_rule WordAddOp   = two_lits (wordOp2    (+))
    primop_rule WordSubOp   = two_lits (wordOp2    (-))
    primop_rule WordMulOp   = two_lits (wordOp2    (*))
    primop_rule WordQuotOp  = two_lits (wordOp2Z   quot)
    primop_rule WordRemOp   = two_lits (wordOp2Z   rem)
    primop_rule AndOp       = two_lits (wordBitOp2 (.&.))
    primop_rule OrOp        = two_lits (wordBitOp2 (.|.))
    primop_rule XorOp       = two_lits (wordBitOp2 xor)
    primop_rule SllOp       = two_lits (wordShiftOp2 Bits.shiftL)
    primop_rule SrlOp       = two_lits (wordShiftOp2 shiftRightLogical)

	-- coercions
    primop_rule Word2IntOp 	= one_lit (litCoerce word2IntLit)
    primop_rule Int2WordOp 	= one_lit (litCoerce int2WordLit)
    primop_rule Narrow8IntOp 	= one_lit (litCoerce narrow8IntLit)
    primop_rule Narrow16IntOp 	= one_lit (litCoerce narrow16IntLit)
    primop_rule Narrow32IntOp 	= one_lit (litCoerce narrow32IntLit)
    primop_rule Narrow8WordOp 	= one_lit (litCoerce narrow8WordLit)
    primop_rule Narrow16WordOp 	= one_lit (litCoerce narrow16WordLit)
    primop_rule Narrow32WordOp 	= one_lit (litCoerce narrow32WordLit)
    primop_rule OrdOp   	= one_lit (litCoerce char2IntLit)
    primop_rule ChrOp    	= one_lit (litCoerce int2CharLit)
    primop_rule Float2IntOp	= one_lit (litCoerce float2IntLit)
    primop_rule Int2FloatOp	= one_lit (litCoerce int2FloatLit)
    primop_rule Double2IntOp	= one_lit (litCoerce double2IntLit)
    primop_rule Int2DoubleOp	= one_lit (litCoerce int2DoubleLit)
	-- SUP: Not sure what the standard says about precision in the following 2 cases
    primop_rule Float2DoubleOp 	= one_lit (litCoerce float2DoubleLit)
    primop_rule Double2FloatOp 	= one_lit (litCoerce double2FloatLit)

	-- Float
    primop_rule FloatAddOp   = two_lits (floatOp2  (+))
    primop_rule FloatSubOp   = two_lits (floatOp2  (-))
    primop_rule FloatMulOp   = two_lits (floatOp2  (*))
    primop_rule FloatDivOp   = two_lits (floatOp2Z (/))
    primop_rule FloatNegOp   = one_lit  negOp

	-- Double
    primop_rule DoubleAddOp   = two_lits (doubleOp2  (+))
    primop_rule DoubleSubOp   = two_lits (doubleOp2  (-))
    primop_rule DoubleMulOp   = two_lits (doubleOp2  (*))
    primop_rule DoubleDivOp   = two_lits (doubleOp2Z (/))
    primop_rule DoubleNegOp   = one_lit  negOp

	-- Relational operators
    primop_rule IntEqOp  = relop (==) ++ litEq op_name True
    primop_rule IntNeOp  = relop (/=) ++ litEq op_name False
    primop_rule CharEqOp = relop (==) ++ litEq op_name True
    primop_rule CharNeOp = relop (/=) ++ litEq op_name False

    primop_rule IntGtOp		= relop (>)
    primop_rule IntGeOp		= relop (>=)
    primop_rule IntLeOp		= relop (<=)
    primop_rule IntLtOp		= relop (<)

    primop_rule CharGtOp	= relop (>)
    primop_rule CharGeOp	= relop (>=)
    primop_rule CharLeOp	= relop (<=)
    primop_rule CharLtOp	= relop (<)

    primop_rule FloatGtOp	= relop (>)
    primop_rule FloatGeOp	= relop (>=)
    primop_rule FloatLeOp	= relop (<=)
    primop_rule FloatLtOp	= relop (<)
    primop_rule FloatEqOp	= relop (==)
    primop_rule FloatNeOp	= relop (/=)

    primop_rule DoubleGtOp	= relop (>)
    primop_rule DoubleGeOp	= relop (>=)
    primop_rule DoubleLeOp	= relop (<=)
    primop_rule DoubleLtOp	= relop (<)
    primop_rule DoubleEqOp	= relop (==)
    primop_rule DoubleNeOp	= relop (/=)

    primop_rule WordGtOp	= relop (>)
    primop_rule WordGeOp	= relop (>=)
    primop_rule WordLeOp	= relop (<=)
    primop_rule WordLtOp	= relop (<)
    primop_rule WordEqOp	= relop (==)
    primop_rule WordNeOp	= relop (/=)

    primop_rule other		= []


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

negOp :: Literal -> Maybe CoreExpr	-- Negate
negOp (MachFloat 0.0)  = Nothing  -- can't represent -0.0 as a Rational
negOp (MachFloat f)    = Just (mkFloatVal (-f))
negOp (MachDouble 0.0) = Nothing
negOp (MachDouble d)   = Just (mkDoubleVal (-d))
negOp (MachInt i)      = intResult (-i)
negOp l		       = Nothing

--------------------------
intOp2 :: (Integer->Integer->Integer) -> Literal -> Literal -> Maybe CoreExpr
intOp2 op (MachInt i1) (MachInt i2) = intResult (i1 `op` i2)
intOp2 op l1	       l2	    = Nothing		-- Could find LitLit

intOp2Z :: (Integer->Integer->Integer) -> Literal -> Literal -> Maybe CoreExpr
-- Like intOp2, but Nothing if i2=0
intOp2Z op (MachInt i1) (MachInt i2)
  | i2 /= 0 = intResult (i1 `op` i2)
intOp2Z op l1 l2 = Nothing		-- LitLit or zero dividend

intShiftOp2 :: (Integer->Int->Integer) -> Literal -> Literal -> Maybe CoreExpr
	-- Shifts take an Int; hence second arg of op is Int
intShiftOp2 op (MachInt i1) (MachInt i2) = intResult (i1 `op` fromInteger i2)
intShiftOp2 op l1           l2           = Nothing 

shiftRightLogical :: Integer -> Int -> Integer
-- Shift right, putting zeros in rather than sign-propagating as Bits.shiftR would do
-- Do this by converting to Word and back.  Obviously this won't work for big 
-- values, but its ok as we use it here
shiftRightLogical x n = fromIntegral (fromInteger x `shiftR` n :: Word)


--------------------------
wordOp2 :: (Integer->Integer->Integer) -> Literal -> Literal -> Maybe CoreExpr
wordOp2 op (MachWord w1) (MachWord w2)
  = wordResult (w1 `op` w2)
wordOp2 op l1 l2 = Nothing		-- Could find LitLit

wordOp2Z :: (Integer->Integer->Integer) -> Literal -> Literal -> Maybe CoreExpr
wordOp2Z op (MachWord w1) (MachWord w2)
  | w2 /= 0 = wordResult (w1 `op` w2)
wordOp2Z op l1 l2 = Nothing	-- LitLit or zero dividend

wordBitOp2 op l1@(MachWord w1) l2@(MachWord w2)
  = wordResult (w1 `op` w2)
wordBitOp2 op l1 l2 = Nothing		-- Could find LitLit

wordShiftOp2 :: (Integer->Int->Integer) -> Literal -> Literal -> Maybe CoreExpr
	-- Shifts take an Int; hence second arg of op is Int
wordShiftOp2 op (MachWord x) (MachInt n) 
  = wordResult (x `op` fromInteger n)
	-- Do the shift at type Integer
wordShiftOp2 op l1 l2 = Nothing	

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

litEq :: Name 
      -> Bool		-- True <=> equality, False <=> inequality
      -> [CoreRule]
litEq op_name is_eq
  = [BuiltinRule { ru_name = occNameFS (nameOccName op_name) 
				`appendFS` FSLIT("->case"),
		   ru_fn = op_name, 
		   ru_nargs = 2, ru_try = rule_fn }]
  where
    rule_fn [Lit lit, expr] = do_lit_eq lit expr
    rule_fn [expr, Lit lit] = do_lit_eq lit expr
    rule_fn other	    = Nothing
    
    do_lit_eq lit expr
      = Just (Case expr (mkWildId (literalType lit)) boolTy
		    [(DEFAULT,    [], val_if_neq),
		     (LitAlt lit, [], val_if_eq)])
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

wordResult :: Integer -> Maybe CoreExpr
wordResult result
  = Just (mkWordVal (toInteger (fromInteger result :: Word)))
\end{code}


%************************************************************************
%*									*
\subsection{Vaguely generic functions
%*									*
%************************************************************************

\begin{code}
mkBasicRule :: Name -> Int -> ([CoreExpr] -> Maybe CoreExpr) -> [CoreRule]
-- Gives the Rule the same name as the primop itself
mkBasicRule op_name n_args rule_fn
  = [BuiltinRule { ru_name = occNameFS (nameOccName op_name),
		   ru_fn = op_name, 
		   ru_nargs = n_args, ru_try = rule_fn }]

oneLit :: Name -> (Literal -> Maybe CoreExpr)
       -> [CoreRule]
oneLit op_name test
  = mkBasicRule op_name 1 rule_fn
  where
    rule_fn [Lit l1] = test (convFloating l1)
    rule_fn _        = Nothing

twoLits :: Name -> (Literal -> Literal -> Maybe CoreExpr)
	-> [CoreRule]
twoLits op_name test 
  = mkBasicRule op_name 2 rule_fn
  where
    rule_fn [Lit l1, Lit l2] = test (convFloating l1) (convFloating l2)
    rule_fn _                = Nothing

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
  | tag_to_enum `hasKey` tagToEnumKey
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
  = [ BuiltinRule FSLIT("AppendLitString") unpackCStringFoldrName 4 match_append_lit,
      BuiltinRule FSLIT("EqString") eqStringName 2 match_eq_string,
      BuiltinRule FSLIT("Inline") inlineIdName 1 match_inline
    ]


---------------------------------------------------
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

---------------------------------------------------
-- The rule is this:
-- 	eqString (unpackCString# (Lit s1)) (unpackCString# (Lit s2) = s1==s2

match_eq_string [Var unpk1 `App` Lit (MachStr s1),
		 Var unpk2 `App` Lit (MachStr s2)]
  | unpk1 `hasKey` unpackCStringIdKey,
    unpk2 `hasKey` unpackCStringIdKey
  = Just (if s1 == s2 then trueVal else falseVal)

match_eq_string other = Nothing


---------------------------------------------------
-- The rule is this:
--	inline (f a b c) = <f's unfolding> a b c
-- (if f has an unfolding)
match_inline (e:_)
  | (Var f, args1) <- collectArgs e,
    Just unf <- maybeUnfoldingTemplate (idUnfolding f)
  = Just (mkApps unf args1)

match_inline other = Nothing
\end{code}		
