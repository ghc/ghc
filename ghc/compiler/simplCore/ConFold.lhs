%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[ConFold]{Constant Folder}

ToDo:
   check boundaries before folding, e.g. we can fold the Float addition
   (i1 + i2) only if it results	in a valid Float.

\begin{code}
module ConFold	( tryPrimOp ) where

#include "HsVersions.h"

import CoreSyn
import Id		( getIdUnfolding )
import Const		( mkMachInt, mkMachWord, Literal(..), Con(..) )
import PrimOp		( PrimOp(..) )
import SimplMonad
import TysWiredIn	( trueDataCon, falseDataCon )
import TyCon		( tyConDataCons, isEnumerationTyCon, isNewTyCon )
import DataCon		( dataConTag, dataConTyCon, fIRST_TAG )
import Const		( conOkForAlt )
import CoreUnfold	( maybeUnfoldingTemplate )
import CoreUtils	( exprIsValue )
import Type		( splitTyConApp_maybe )

import Maybes		( maybeToBool )
import Char		( ord, chr )
import Outputable

#if __GLASGOW_HASKELL__ >= 404
import GlaExts		( fromInt )
#endif
\end{code}

\begin{code}
tryPrimOp :: PrimOp -> [CoreArg]  -- op arg1 ... argn
				  --   Args are already simplified
	  -> Maybe CoreExpr	  -- Nothing => no transformation
				  -- Just e  => transforms to e
\end{code}

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
tryPrimOp SeqOp [Type ty, arg]
  | exprIsValue arg
  = Just (Con (Literal (mkMachInt 1)) [])
\end{code}

\begin{code}
tryPrimOp TagToEnumOp [Type ty, Con (Literal (MachInt i _)) _]
  | isEnumerationTyCon tycon = Just (Con (DataCon dc) [])
  | otherwise = panic "tryPrimOp: tagToEnum# on non-enumeration type"
    where tag = fromInteger i
	  constrs = tyConDataCons tycon
	  (dc:_) = [ dc | dc <- constrs, tag == dataConTag dc - fIRST_TAG ]
	  (Just (tycon,_)) = splitTyConApp_maybe ty
\end{code}

For dataToTag#, we can reduce if either 
	
	(a) the argument is a constructor
	(b) the argument is a variable whose unfolding is a known constructor

\begin{code}
tryPrimOp DataToTagOp [Type ty, Con (DataCon dc) _]
  = Just (Con (Literal (mkMachInt (toInteger (dataConTag dc - fIRST_TAG)))) [])
tryPrimOp DataToTagOp [Type ty, Var x]
  | maybeToBool maybe_constr
  = ASSERT( not (isNewTyCon (dataConTyCon dc)) )
    Just (Con (Literal (mkMachInt (toInteger (dataConTag dc - fIRST_TAG)))) [])
  where
    maybe_constr = case maybeUnfoldingTemplate (getIdUnfolding x) of
			Just (Con (DataCon dc) _) -> Just dc
			other			  -> Nothing
    Just dc = maybe_constr
\end{code}

\begin{code}
tryPrimOp op args
  = case args of
     [Con (Literal (MachChar char_lit))      _] -> oneCharLit   op char_lit
     [Con (Literal (MachInt int_lit signed)) _] -> (if signed then oneIntLit else oneWordLit)
							  op int_lit
     [Con (Literal (MachFloat float_lit))   _]  -> oneFloatLit  op float_lit
     [Con (Literal (MachDouble double_lit)) _]  -> oneDoubleLit op double_lit
     [Con (Literal other_lit)               _]  -> oneLit       op other_lit

     [Con (Literal (MachChar char_lit1)) _,
      Con (Literal (MachChar char_lit2)) _]     -> twoCharLits op char_lit1 char_lit2

     [Con (Literal (MachInt int_lit1 True)) _,  -- both *signed* literals
      Con (Literal (MachInt int_lit2 True)) _]  -> twoIntLits op int_lit1 int_lit2

     [Con (Literal (MachInt int_lit1 False)) _, -- both *unsigned* literals
      Con (Literal (MachInt int_lit2 False)) _] -> twoWordLits op int_lit1 int_lit2

     [Con (Literal (MachInt int_lit1 False)) _, -- unsigned+signed (shift ops)
      Con (Literal (MachInt int_lit2 True))  _] -> oneWordOneIntLit op int_lit1 int_lit2

     [Con (Literal (MachFloat float_lit1)) _,
      Con (Literal (MachFloat float_lit2)) _]   -> twoFloatLits op float_lit1 float_lit2

     [Con (Literal (MachDouble double_lit1)) _,
      Con (Literal (MachDouble double_lit2)) _] -> twoDoubleLits op double_lit1 double_lit2

     [Con (Literal lit) _, Var var]	        -> litVar op lit var
     [Var var, Con (Literal lit) _]	        -> litVar op lit var

     other			       		-> give_up
  where
    give_up = Nothing

    return_char c   = Just (Con (Literal (MachChar   c)) [])
    return_int i    = Just (Con (Literal (mkMachInt  i)) [])
    return_word i   = Just (Con (Literal (mkMachWord i)) [])
    return_float f  = Just (Con (Literal (MachFloat  f)) [])
    return_double d = Just (Con (Literal (MachDouble d)) [])
    return_lit lit  = Just (Con (Literal lit) [])

    return_bool True  = Just trueVal
    return_bool False = Just falseVal

    return_prim_case var lit val_if_eq val_if_neq
      = Just (Case (Var var) var [(Literal lit, [], val_if_eq),
			          (DEFAULT,     [], val_if_neq)])

	---------   Ints --------------
    oneIntLit IntNegOp     i = return_int (-i)
    oneIntLit ChrOp        i = return_char (chr (fromInteger i))
-- SIGH: these two cause trouble in unfoldery
-- as we can't distinguish unsigned literals in interfaces (ToDo?)
--  oneIntLit Int2WordOp   i = ASSERT( i>=0 ) return_word i
--  oneIntLit Int2AddrOp   i = ASSERT( i>=0 ) return_lit (MachAddr i)
    oneIntLit Int2FloatOp  i = return_float (fromInteger i)
    oneIntLit Int2DoubleOp i = return_double (fromInteger i)
    oneIntLit _            _ = {-trace "oneIntLit: giving up"-} give_up

    oneWordLit Word2IntOp   w = {-lazy:ASSERT( w<= maxInt)-} return_int w
--  oneWordLit NotOp	    w = ??? ToDo: sort-of a pain
    oneWordLit _            _ = {-trace "oneIntLit: giving up"-} give_up

    twoIntLits IntAddOp	 i1 i2		 = checkRange (i1+i2)
    twoIntLits IntSubOp	 i1 i2		 = checkRange (i1-i2)
    twoIntLits IntMulOp	 i1 i2		 = checkRange (i1*i2)
    twoIntLits IntQuotOp i1 i2 | i2 /= 0 = return_int (i1 `quot` i2)
    twoIntLits IntRemOp  i1 i2 | i2 /= 0 = return_int (i1 `rem` i2)
    twoIntLits IntGtOp	 i1 i2		 = return_bool (i1 >  i2)
    twoIntLits IntGeOp	 i1 i2		 = return_bool (i1 >= i2)
    twoIntLits IntEqOp	 i1 i2		 = return_bool (i1 == i2)
    twoIntLits IntNeOp	 i1 i2		 = return_bool (i1 /= i2)
    twoIntLits IntLtOp	 i1 i2		 = return_bool (i1 <  i2)
    twoIntLits IntLeOp	 i1 i2		 = return_bool (i1 <= i2)
    -- ToDo: something for integer-shift ops?
    twoIntLits _	 _  _	         = give_up

    twoWordLits WordGtOp w1 w2 = return_bool (w1 >  w2)
    twoWordLits WordGeOp w1 w2 = return_bool (w1 >= w2)
    twoWordLits WordEqOp w1 w2 = return_bool (w1 == w2)
    twoWordLits WordNeOp w1 w2 = return_bool (w1 /= w2)
    twoWordLits WordLtOp w1 w2 = return_bool (w1 <  w2)
    twoWordLits WordLeOp w1 w2 = return_bool (w1 <= w2)
    -- ToDo: something for AndOp, OrOp?
    twoWordLits _	 _  _  = give_up

    -- ToDo: something for shifts
    oneWordOneIntLit _ _  _    = give_up

	---------   Floats --------------
    oneFloatLit FloatNegOp  f 	= return_float (-f)
    -- hard to do float ops in Rationals ?? (WDP 94/10) ToDo
    oneFloatLit _	    _   = give_up

    twoFloatLits FloatGtOp    f1 f2	      = return_bool (f1 >  f2)
    twoFloatLits FloatGeOp    f1 f2	      = return_bool (f1 >= f2)
    twoFloatLits FloatEqOp    f1 f2	      = return_bool (f1 == f2)
    twoFloatLits FloatNeOp    f1 f2	      = return_bool (f1 /= f2)
    twoFloatLits FloatLtOp    f1 f2	      = return_bool (f1 <  f2)
    twoFloatLits FloatLeOp    f1 f2	      = return_bool (f1 <= f2)
    twoFloatLits FloatAddOp   f1 f2	      = return_float (f1 + f2)
    twoFloatLits FloatSubOp   f1 f2	      = return_float (f1 - f2)
    twoFloatLits FloatMulOp   f1 f2	      = return_float (f1 * f2)
    twoFloatLits FloatDivOp   f1 f2 | f2 /= 0 = return_float (f1 / f2)
    twoFloatLits _	      _  _	      = give_up

	---------   Doubles --------------
    oneDoubleLit DoubleNegOp  d = return_double (-d)
    oneDoubleLit _	      _ = give_up

    twoDoubleLits DoubleGtOp    d1 d2		= return_bool (d1 >  d2)
    twoDoubleLits DoubleGeOp    d1 d2		= return_bool (d1 >= d2)
    twoDoubleLits DoubleEqOp    d1 d2		= return_bool (d1 == d2)
    twoDoubleLits DoubleNeOp    d1 d2		= return_bool (d1 /= d2)
    twoDoubleLits DoubleLtOp    d1 d2		= return_bool (d1 <  d2)
    twoDoubleLits DoubleLeOp    d1 d2		= return_bool (d1 <= d2)
    twoDoubleLits DoubleAddOp   d1 d2		= return_double (d1 + d2)
    twoDoubleLits DoubleSubOp   d1 d2		= return_double (d1 - d2)
    twoDoubleLits DoubleMulOp   d1 d2		= return_double (d1 * d2)
    twoDoubleLits DoubleDivOp   d1 d2 | d2 /= 0 = return_double (d1 / d2)
    twoDoubleLits _             _  _ 	        = give_up

	---------   Characters --------------
    oneCharLit OrdOp c = return_int (fromInt (ord c))
    oneCharLit _     _ = give_up

    twoCharLits CharGtOp c1 c2 = return_bool (c1 >  c2)
    twoCharLits CharGeOp c1 c2 = return_bool (c1 >= c2)
    twoCharLits CharEqOp c1 c2 = return_bool (c1 == c2)
    twoCharLits CharNeOp c1 c2 = return_bool (c1 /= c2)
    twoCharLits CharLtOp c1 c2 = return_bool (c1 <  c2)
    twoCharLits CharLeOp c1 c2 = return_bool (c1 <= c2)
    twoCharLits _        _  _  = give_up

	---------   Miscellaneous --------------
    oneLit Addr2IntOp (MachAddr i) = return_int (fromInteger i)
    oneLit op         lit          = give_up

	---------   Equality and inequality for Int/Char --------------
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

    litVar IntEqOp  lit var = return_prim_case var lit trueVal  falseVal
    litVar IntNeOp  lit var = return_prim_case var lit falseVal trueVal
    litVar CharEqOp lit var = return_prim_case var lit trueVal  falseVal
    litVar CharNeOp lit var = return_prim_case var lit falseVal trueVal
    litVar other_op lit var = give_up


    checkRange :: Integer -> Maybe CoreExpr
    checkRange val
     | (val > fromInt maxInt) || (val < fromInt minInt)  = 
	-- Better tell the user that we've overflowed...
       pprTrace "Warning:" (text "Integer overflow in expression: " <> 
	                   ppr ((mkPrimApp op args)::CoreExpr)) $
	-- ..not that it stops us from actually folding!
	-- ToDo: a SrcLoc would be nice.
       return_int val
     | otherwise = return_int val

trueVal  = Con (DataCon trueDataCon)  []
falseVal = Con (DataCon falseDataCon) []
\end{code}
