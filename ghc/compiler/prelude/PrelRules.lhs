%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[ConFold]{Constant Folder}

ToDo:
   check boundaries before folding, e.g. we can fold the Float addition
   (i1 + i2) only if it results	in a valid Float.

\begin{code}
module PrelRules ( primOpRule, builtinRules ) where

#include "HsVersions.h"

import CoreSyn
import Rules		( ProtoCoreRule(..) )
import Id		( idUnfolding, mkWildId, isDataConId_maybe )
import Literal		( Literal(..), isLitLitLit, mkMachInt, mkMachWord
			, inIntRange, inWordRange, literalType
			, word2IntLit, int2WordLit, char2IntLit, int2CharLit
			, float2IntLit, int2FloatLit, double2IntLit, int2DoubleLit
			, addr2IntLit, int2AddrLit, float2DoubleLit, double2FloatLit
			)
import PrimOp		( PrimOp(..), primOpOcc )
import TysWiredIn	( trueDataConId, falseDataConId )
import TyCon		( tyConDataCons, isEnumerationTyCon, isNewTyCon )
import DataCon		( DataCon, dataConTag, dataConRepArity, dataConTyCon, dataConId, fIRST_TAG )
import CoreUnfold	( maybeUnfoldingTemplate )
import CoreUtils	( exprIsValue, cheapEqExpr, exprIsConApp_maybe )
import Type		( splitTyConApp_maybe )
import OccName		( occNameUserString)
import ThinAir		( unpackCStringFoldrId )
import Maybes		( maybeToBool )
import Char		( ord, chr )
import Bits		( Bits(..) )
import PrelAddr		( intToWord, wordToInt )
import Word		( Word64 )
import Outputable
\end{code}



\begin{code}
primOpRule :: PrimOp -> CoreRule
primOpRule op 
  = BuiltinRule (primop_rule op)
  where
    op_name = _PK_ (occNameUserString (primOpOcc op))
    op_name_case = op_name _APPEND_ SLIT("case")

    -- ToDo:	something for integer-shift ops?
    --		NotOp

    primop_rule SeqOp	    = seqRule
    primop_rule TagToEnumOp = tagToEnumRule
    primop_rule DataToTagOp = dataToTagRule

	-- Int operations
    primop_rule IntAddOp    = twoLits (intOp2  (+)  op_name)
    primop_rule IntSubOp    = twoLits (intOp2  (-)  op_name)
    primop_rule IntMulOp    = twoLits (intOp2  (*)  op_name)
    primop_rule IntQuotOp   = twoLits (intOp2Z quot op_name)
    primop_rule IntRemOp    = twoLits (intOp2Z rem  op_name)
    primop_rule IntNegOp    = oneLit  (negOp        op_name)

	-- Word operations
    primop_rule WordQuotOp  = twoLits (wordOp2Z   quot  op_name)
    primop_rule WordRemOp   = twoLits (wordOp2Z   rem   op_name)
    primop_rule AndOp       = twoLits (wordBitOp2 (.&.) op_name)
    primop_rule OrOp        = twoLits (wordBitOp2 (.|.) op_name)
    primop_rule XorOp       = twoLits (wordBitOp2 xor   op_name)

	-- coercions
    primop_rule Word2IntOp 	= oneLit (litCoerce word2IntLit     op_name)
    primop_rule Int2WordOp 	= oneLit (litCoerce int2WordLit     op_name)
    primop_rule OrdOp   	= oneLit (litCoerce char2IntLit     op_name)
    primop_rule ChrOp    	= oneLit (litCoerce int2CharLit	    op_name)
    primop_rule Float2IntOp	= oneLit (litCoerce float2IntLit    op_name)
    primop_rule Int2FloatOp	= oneLit (litCoerce int2FloatLit    op_name)
    primop_rule Double2IntOp	= oneLit (litCoerce double2IntLit   op_name)
    primop_rule Int2DoubleOp	= oneLit (litCoerce int2DoubleLit   op_name)
    primop_rule Addr2IntOp 	= oneLit (litCoerce addr2IntLit     op_name)
    primop_rule Int2AddrOp 	= oneLit (litCoerce int2AddrLit     op_name)
	-- SUP: Not sure what the standard says about precision in the following 2 cases
    primop_rule Float2DoubleOp 	= oneLit (litCoerce float2DoubleLit op_name)
    primop_rule Double2FloatOp 	= oneLit (litCoerce double2FloatLit op_name)

	-- Float
    primop_rule FloatAddOp   = twoLits (floatOp2 (+) op_name)
    primop_rule FloatSubOp   = twoLits (floatOp2 (-) op_name)
    primop_rule FloatMulOp   = twoLits (floatOp2 (*) op_name)
    primop_rule FloatDivOp   = twoLits (floatOp2Z (/) op_name)
    primop_rule FloatNegOp   = oneLit  (negOp op_name)

	-- Double
    primop_rule DoubleAddOp   = twoLits (doubleOp2 (+) op_name)
    primop_rule DoubleSubOp   = twoLits (doubleOp2 (-) op_name)
    primop_rule DoubleMulOp   = twoLits (doubleOp2 (*) op_name)
    primop_rule DoubleDivOp   = twoLits (doubleOp2Z (/) op_name)
    primop_rule DoubleNegOp   = oneLit  (negOp op_name)

	-- Relational operators
    primop_rule IntEqOp  = relop (==) `or_rule` litEq True  op_name_case
    primop_rule IntNeOp  = relop (/=) `or_rule` litEq False op_name_case
    primop_rule CharEqOp = relop (==) `or_rule` litEq True  op_name_case
    primop_rule CharNeOp = relop (/=) `or_rule` litEq False op_name_case

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

    primop_rule other		= \args -> Nothing


    relop cmp = twoLits (cmpOp (\ord -> ord `cmp` EQ) op_name)
	-- Cunning.  cmpOp compares the values to give an Ordering.
	-- It applies its argument to that ordering value to turn
	-- the ordering into a boolean value.  (`cmp` EQ) is just the job.
\end{code}

%************************************************************************
%*									*
\subsection{Doing the business}
%*									*
%************************************************************************

	IMPORTANT NOTE

In all these operations we might find a LitLit as an operand; that's
why we have the catch-all Nothing case.

\begin{code}
--------------------------
litCoerce :: (Literal -> Literal) -> RuleName -> Literal -> Maybe (RuleName, CoreExpr)
litCoerce fn name lit | isLitLitLit lit = Nothing
                      | otherwise       = Just (name, Lit (fn lit))

--------------------------
cmpOp :: (Ordering -> Bool) -> FAST_STRING -> Literal -> Literal -> Maybe (RuleName, CoreExpr)
cmpOp cmp name l1 l2
  = go l1 l2
  where
    done res | cmp res = Just (name, trueVal)
	     | otherwise    = Just (name, falseVal)

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

negOp name (MachFloat f)  = Just (name, mkFloatVal (-f))
negOp name (MachDouble d) = Just (name, mkDoubleVal (-d))
negOp name l@(MachInt i)  = intResult name (ppr l) (-i)
negOp name l		  = Nothing

--------------------------
intOp2 op name l1@(MachInt i1) l2@(MachInt i2)
  = intResult name (ppr l1 <+> ppr l2) (i1 `op` i2)
intOp2 op name l1 l2 = Nothing		-- Could find LitLit

intOp2Z op name (MachInt i1) (MachInt i2)
  | i2 /= 0 = Just (name, mkIntVal (i1 `op` i2))
intOp2Z op name l1 l2 = Nothing		-- LitLit or zero dividend

--------------------------
-- Integer is not an instance of Bits, so we operate on Word64
wordBitOp2 op name l1@(MachWord w1) l2@(MachWord w2)
  = wordResult name (ppr l1 <+> ppr l2)
                    ((fromIntegral::Word64->Integer) (fromIntegral w1 `op` fromIntegral w2))
wordBitOp2 op name l1 l2 = Nothing		-- Could find LitLit

wordOp2Z op name (MachWord w1) (MachWord w2)
  | w2 /= 0 = Just (name, mkWordVal (w1 `op` w2))
wordOp2Z op name l1 l2 = Nothing	-- LitLit or zero dividend

--------------------------
floatOp2  op name (MachFloat f1) (MachFloat f2)
  = Just (name, mkFloatVal (f1 `op` f2))
floatOp2  op name l1 l2 = Nothing

floatOp2Z op name (MachFloat f1) (MachFloat f2)
  | f1 /= 0   = Just (name, mkFloatVal (f1 `op` f2))
floatOp2Z op name l1 l2 = Nothing

--------------------------
doubleOp2  op name (MachDouble f1) (MachDouble f2)
  = Just (name, mkDoubleVal (f1 `op` f2))
doubleOp2 op name l1 l2 = Nothing

doubleOp2Z op name (MachDouble f1) (MachDouble f2)
  | f1 /= 0   = Just (name, mkDoubleVal (f1 `op` f2))
doubleOp2Z op name l1 l2 = Nothing


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
        -> RuleName
	-> RuleFun
litEq is_eq name [Lit lit, expr] = do_lit_eq is_eq name lit expr
litEq is_eq name [expr, Lit lit] = do_lit_eq is_eq name lit expr
litEq is_eq name other		 = Nothing

do_lit_eq is_eq name lit expr
  = Just (name, Case expr (mkWildId (literalType lit))
		     [(LitAlt lit, [], val_if_eq),
		      (DEFAULT,    [], val_if_neq)])
  where
    val_if_eq  | is_eq     = trueVal
	       | otherwise = falseVal
    val_if_neq | is_eq     = falseVal
	       | otherwise = trueVal

-- TODO: Merge intResult/wordResult
intResult name pp_args result
  | not (inIntRange result)
	-- Better tell the user that we've overflowed...
	-- ..not that it stops us from actually folding!
  
  = pprTrace "Warning:" (text "Integer overflow in:" <+> ppr name <+> pp_args)
    Just (name, mkIntVal (squashInt result))

  | otherwise
  = Just (name, mkIntVal result)

wordResult name pp_args result
  | not (inWordRange result)
	-- Better tell the user that we've overflowed...
	-- ..not that it stops us from actually folding!
  
  = pprTrace "Warning:" (text "Word overflow in:" <+> ppr name <+> pp_args)
    Just (name, mkWordVal (squashInt result))

  | otherwise
  = Just (name, mkWordVal result)

squashInt :: Integer -> Integer	-- Squash into Int range
squashInt i = toInteger ((fromInteger i)::Int)
\end{code}


%************************************************************************
%*									*
\subsection{Vaguely generic functions
%*									*
%************************************************************************

\begin{code}
type RuleFun = [CoreExpr] -> Maybe (RuleName, CoreExpr)

or_rule :: RuleFun -> RuleFun -> RuleFun
or_rule r1 r2 args = case r1 args of
		   Just stuff -> Just stuff
		   Nothing    -> r2 args

twoLits :: (Literal -> Literal -> Maybe (RuleName, CoreExpr)) -> RuleFun
twoLits rule [Lit l1, Lit l2] = rule l1 l2
twoLits rule other	      = Nothing

oneLit :: (Literal -> Maybe (RuleName, CoreExpr)) -> RuleFun
oneLit rule [Lit l1] = rule l1
oneLit rule other    = Nothing


trueVal       = Var trueDataConId
falseVal      = Var falseDataConId
mkIntVal    i = Lit (mkMachInt  i)
mkWordVal   w = Lit (mkMachWord w)
mkCharVal   c = Lit (MachChar   c)
mkFloatVal  f = Lit (MachFloat  f)
mkDoubleVal d = Lit (MachDouble d)
\end{code}

						
%************************************************************************
%*									*
\subsection{Special rules for seq, tagToEnum, dataToTag}
%*									*
%************************************************************************

In the parallel world, we use _seq_ to control the order in which
certain expressions will be evaluated.  Operationally, the expression
``_seq_ a b'' evaluates a and then evaluates b.  We have an inlining
for _seq_ which translates _seq_ to:

   _seq_ = /\ a b -> \ x::a y::b -> case seq# x of { 0# -> parError#; _ -> y }

Now, we know that the seq# primitive will never return 0#, but we
don't let the simplifier know that.  We also use a special error
value, parError#, which is *not* a bottoming Id, so as far as the
simplifier is concerned, we have to evaluate seq# a before we know
whether or not y will be evaluated.

If we didn't have the extra case, then after inlining the compiler might
see:
	f p q = case seq# p of { _ -> p+q }

If it sees that, it can see that f is strict in q, and hence it might
evaluate q before p!  The "0# ->" case prevents this happening.
By having the parError# branch we make sure that anything in the
other branch stays there!

This is fine, but we'd like to get rid of the extraneous code.  Hence,
we *do* let the simplifier know that seq# is strict in its argument.
As a result, we hope that `a' will be evaluated before seq# is called.
At this point, we have a very special and magical simpification which
says that ``seq# a'' can be immediately simplified to `1#' if we
know that `a' is already evaluated.

NB: If we ever do case-floating, we have an extra worry:

    case a of
      a' -> let b' = case seq# a of { True -> b; False -> parError# }
	    in case b' of ...

    =>

    case a of
      a' -> let b' = case True of { True -> b; False -> parError# }
	    in case b' of ...

    =>

    case a of
      a' -> let b' = b
	    in case b' of ...

    =>

    case a of
      a' -> case b of ...

The second case must never be floated outside of the first!

\begin{code}
seqRule [Type ty, arg] | exprIsValue arg = Just (SLIT("Seq"), mkIntVal 1)
seqRule other				 = Nothing
\end{code}


\begin{code}
tagToEnumRule [Type ty, Lit (MachInt i)]
  = ASSERT( isEnumerationTyCon tycon ) 
    Just (SLIT("TagToEnum"), Var (dataConId dc))
  where 
    tag = fromInteger i
    constrs = tyConDataCons tycon
    (dc:_) = [ dc | dc <- constrs, tag == dataConTag dc - fIRST_TAG ]
    (Just (tycon,_)) = splitTyConApp_maybe ty

tagToEnumRule other = Nothing
\end{code}

For dataToTag#, we can reduce if either 
	
	(a) the argument is a constructor
	(b) the argument is a variable whose unfolding is a known constructor

\begin{code}
dataToTagRule [_, val_arg]
  = case exprIsConApp_maybe val_arg of
	Just (dc,_) -> ASSERT( not (isNewTyCon (dataConTyCon dc)) )
	     	       Just (SLIT("DataToTag"), 
		   	     mkIntVal (toInteger (dataConTag dc - fIRST_TAG)))

	other	    -> Nothing

dataToTagRule other = Nothing
\end{code}

%************************************************************************
%*									*
\subsection{Built in rules}
%*									*
%************************************************************************

\begin{code}
builtinRules :: [ProtoCoreRule]
-- Rules for non-primops that can't be expressed using a RULE pragma
builtinRules
  = [ ProtoCoreRule False unpackCStringFoldrId 
		    (BuiltinRule match_append_lit_str)
    ]


-- unpack "foo" c (unpack "baz" c n)  =  unpack "foobaz" c n

match_append_lit_str [Type ty1,
		      Lit (MachStr s1),
		      c1,
		      Var unpk `App` Type ty2 
			       `App` Lit (MachStr s2)
			       `App` c2
			       `App` n
		     ]
  | unpk == unpackCStringFoldrId && 
    c1 `cheapEqExpr` c2
  = ASSERT( ty1 == ty2 )
    Just (SLIT("AppendLitString"),
	  Var unpk `App` Type ty1
		   `App` Lit (MachStr (s1 _APPEND_ s2))
		   `App` c1
		   `App` n)

match_append_lit_str other = Nothing
\end{code}		
