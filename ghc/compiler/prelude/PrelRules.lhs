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
import Id		( getIdUnfolding )
import Const		( mkMachInt, mkMachWord, Literal(..), Con(..) )
import PrimOp		( PrimOp(..), primOpOcc )
import TysWiredIn	( trueDataCon, falseDataCon )
import TyCon		( tyConDataCons, isEnumerationTyCon, isNewTyCon )
import DataCon		( dataConTag, dataConTyCon, fIRST_TAG )
import CoreUnfold	( maybeUnfoldingTemplate )
import CoreUtils	( exprIsValue, cheapEqExpr )
import Type		( splitTyConApp_maybe )
import OccName		( occNameUserString)
import ThinAir		( unpackCStringFoldrId )
import Maybes		( maybeToBool )
import Char		( ord, chr )
import Outputable

#if __GLASGOW_HASKELL__ >= 404
import GlaExts		( fromInt )
#endif
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
    --		Int2WordOp	-- SIGH: these two cause trouble in unfoldery
    --		Int2AddrOp	-- as we can't distinguish unsigned literals in interfaces (ToDo?)

    primop_rule SeqOp	    = seqRule
    primop_rule TagToEnumOp = tagToEnumRule
    primop_rule DataToTagOp = dataToTagRule

	-- Addr operations
    primop_rule Addr2IntOp	= oneLit (addr2IntOp op_name)
 
	-- Char operations
    primop_rule OrdOp   	= oneLit (chrOp op_name)
 
	-- Int/Word operations
    primop_rule IntAddOp    = twoLits (intOp2 (+) op_name)
    primop_rule IntSubOp    = twoLits (intOp2 (-) op_name)
    primop_rule IntMulOp    = twoLits (intOp2 (*) op_name)
    primop_rule IntQuotOp   = twoLits (intOp2Z quot op_name)
    primop_rule IntRemOp    = twoLits (intOp2Z rem  op_name)
    primop_rule IntNegOp    = oneLit  (negOp op_name)

    primop_rule ChrOp    	= oneLit (intCoerce (mkCharVal . chr) op_name)
    primop_rule Int2FloatOp	= oneLit (intCoerce mkFloatVal	      op_name)
    primop_rule Int2DoubleOp	= oneLit (intCoerce mkDoubleVal       op_name)
    primop_rule Word2IntOp 	= oneLit (intCoerce mkIntVal	      op_name)
    primop_rule Int2WordOp 	= oneLit (intCoerce mkWordVal	      op_name)

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

	-- Relational operators
    primop_rule IntEqOp  = relop (==) op_name `or_rule` litVar True  op_name_case
    primop_rule IntNeOp  = relop (/=) op_name `or_rule` litVar False op_name_case
    primop_rule CharEqOp = relop (==) op_name `or_rule` litVar True  op_name_case
    primop_rule CharNeOp = relop (/=) op_name `or_rule` litVar False op_name_case

    primop_rule IntGtOp		= relop (>)  op_name
    primop_rule IntGeOp		= relop (>=) op_name
    primop_rule IntLeOp		= relop (<=) op_name
    primop_rule IntLtOp		= relop (<)  op_name

    primop_rule CharGtOp	= relop (>)  op_name
    primop_rule CharGeOp	= relop (>=) op_name
    primop_rule CharLeOp	= relop (<=) op_name
    primop_rule CharLtOp	= relop (<)  op_name

    primop_rule FloatGtOp	= relop (>)  op_name
    primop_rule FloatGeOp	= relop (>=) op_name
    primop_rule FloatLeOp	= relop (<=) op_name
    primop_rule FloatLtOp	= relop (<)  op_name
    primop_rule FloatEqOp	= relop (==) op_name
    primop_rule FloatNeOp	= relop (/=) op_name

    primop_rule DoubleGtOp	= relop (>)  op_name
    primop_rule DoubleGeOp	= relop (>=) op_name
    primop_rule DoubleLeOp	= relop (<=) op_name
    primop_rule DoubleLtOp	= relop (<)  op_name
    primop_rule DoubleEqOp	= relop (==) op_name
    primop_rule DoubleNeOp	= relop (/=) op_name

    primop_rule WordGtOp	= relop (>)  op_name
    primop_rule WordGeOp	= relop (>=) op_name
    primop_rule WordLeOp	= relop (<=) op_name
    primop_rule WordLtOp	= relop (<)  op_name
    primop_rule WordEqOp	= relop (==) op_name
    primop_rule WordNeOp	= relop (/=) op_name

    primop_rule other		= \args -> Nothing
\end{code}

%************************************************************************
%*									*
\subsection{Doing the business}
%*									*
%************************************************************************

\begin{code}
--------------------------
intCoerce :: Num a => (a -> CoreExpr) -> RuleName -> Literal -> Maybe (RuleName, CoreExpr)
intCoerce fn name (MachInt i _) = Just (name, fn (fromInteger i))

--------------------------
relop cmp name = twoLits (\l1 l2 -> Just (name, if l1 `cmp` l2 then trueVal else falseVal))

--------------------------
negOp name (MachFloat f)  = Just (name, mkFloatVal (-f))
negOp name (MachDouble d) = Just (name, mkDoubleVal (-d))
negOp name (MachInt i _)  = Just (name, mkIntVal (-i))

chrOp name (MachChar c) = Just (name, mkIntVal (fromInt (ord c)))

addr2IntOp name (MachAddr i) = Just (name, mkIntVal i)

--------------------------
intOp2 op name l1@(MachInt i1 s1) l2@(MachInt i2 s2)
  | (result > fromInt maxInt) || (result < fromInt minInt) 
	-- Better tell the user that we've overflowed...
	-- ..not that it stops us from actually folding!
  = pprTrace "Warning:" (text "Integer overflow in expression: " <> 
	                 ppr name <+> ppr l1 <+> ppr l2) $
    Just (name, mkIntVal result)

  | otherwise
  = ASSERT( s1 && s2 )		-- Both should be signed
    Just (name, mkIntVal result)
  where
    result = i1 `op` i2

intOp2Z op name (MachInt i1 s1) (MachInt i2 s2)
  | i2 == 0   = Nothing	-- Don't do it if the dividend < 0
  | otherwise = Just (name, mkIntVal (i1 `op` i2))


--------------------------
floatOp2  op name (MachFloat f1) (MachFloat f2)
  = Just (name, mkFloatVal (f1 `op` f2))

floatOp2Z op name (MachFloat f1) (MachFloat f2)
  | f1 /= 0   = Just (name, mkFloatVal (f1 `op` f2))
  | otherwise = Nothing


--------------------------
doubleOp2  op name (MachDouble f1) (MachDouble f2)
  = Just (name, mkDoubleVal (f1 `op` f2))

doubleOp2Z op name (MachDouble f1) (MachDouble f2)
  | f1 /= 0   = Just (name, mkDoubleVal (f1 `op` f2))
  | otherwise = Nothing


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

litVar :: Bool		-- True <=> equality, False <=> inequality
        -> RuleName
	-> RuleFun
litVar is_eq name [Con (Literal lit) _, Var var] = do_lit_var is_eq name lit var
litVar is_eq name [Var var, Con (Literal lit) _] = do_lit_var is_eq name lit var
litVar is_eq name other			 	 = Nothing

do_lit_var is_eq name lit var 
  = Just (name, Case (Var var) var [(Literal lit, [], val_if_eq),
			            (DEFAULT,     [], val_if_neq)])
  where
    val_if_eq  | is_eq     = trueVal
	       | otherwise = falseVal
    val_if_neq | is_eq     = falseVal
	       | otherwise = trueVal
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
twoLits rule [Con (Literal l1) _, Con (Literal l2) _] = rule l1 l2
twoLits rule other				      = Nothing

oneLit :: (Literal -> Maybe (RuleName, CoreExpr)) -> RuleFun
oneLit rule [Con (Literal l1) _] = rule l1
oneLit rule other		 = Nothing


trueVal       = Con (DataCon trueDataCon)  []
falseVal      = Con (DataCon falseDataCon) []
mkIntVal i    = Con (Literal (mkMachInt  i)) []
mkCharVal c   = Con (Literal (MachChar   c)) []
mkWordVal w   = Con (Literal (mkMachWord w)) []
mkFloatVal f  = Con (Literal (MachFloat  f)) []
mkDoubleVal d = Con (Literal (MachDouble d)) []
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
tagToEnumRule [Type ty, Con (Literal (MachInt i _)) _]
  = ASSERT( isEnumerationTyCon tycon ) 
    Just (SLIT("TagToEnum"), Con (DataCon dc) [])
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
  = case val_arg of
	Con (DataCon dc) _ -> yes dc
	Var x		   -> case maybeUnfoldingTemplate (getIdUnfolding x) of
				Just (Con (DataCon dc) _) -> yes dc
				other			  -> Nothing
	other		   -> Nothing
  where
    yes dc = ASSERT( not (isNewTyCon (dataConTyCon dc)) )
	     Just (SLIT("DataToTag"), 
		   mkIntVal (toInteger (dataConTag dc - fIRST_TAG)))

dataToTagRule other = Nothing
\end{code}

%************************************************************************
%*									*
\subsection{Built in rules}
%*									*
%************************************************************************

\begin{code}
builtinRules :: [ProtoCoreRule]
builtinRules
  = [ ProtoCoreRule False unpackCStringFoldrId 
		    (BuiltinRule match_append_lit_str)
    ]


-- unpack "foo" c (unpack "baz" c n)  =  unpack "foobaz" c n

match_append_lit_str [Type ty1,
		      Con (Literal (MachStr s1)) [],
		      c1,
		      Var unpk `App` Type ty2 
			       `App` Con (Literal (MachStr s2)) []
			       `App` c2
			       `App` n
		     ]
  | unpk == unpackCStringFoldrId && 
    c1 `cheapEqExpr` c2
  = ASSERT( ty1 == ty2 )
    Just (SLIT("AppendLitString"),
	  Var unpk `App` Type ty1
		   `App` Con (Literal (MachStr (s1 _APPEND_ s2))) []
		   `App` c1
		   `App` n)

match_append_lit_str other = Nothing
\end{code}		
