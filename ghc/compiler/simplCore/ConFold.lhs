%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[ConFold]{Constant Folder}

ToDo:
   check boundaries before folding, e.g. we can fold the Float addition
   (i1 + i2) only if it results	in a valid Float.

\begin{code}
#include "HsVersions.h"

module ConFold	( completePrim ) where

IMP_Ubiq(){-uitous-}

import CoreSyn
import CoreUnfold	( Unfolding(..), SimpleUnfolding )
import Id		( idType )
import Literal		( mkMachInt, mkMachWord, Literal(..) )
import MagicUFs		( MagicUnfoldingFun )
import PrimOp		( PrimOp(..) )
import SimplEnv
import SimplMonad
import TysWiredIn	( trueDataCon, falseDataCon )

#ifdef REALLY_HASKELL_1_3
ord = fromEnum :: Char -> Int
chr = toEnum   :: Int -> Char
#endif
\end{code}

\begin{code}
completePrim :: SimplEnv
	     -> PrimOp -> [OutArg]
	     -> SmplM OutExpr
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
completePrim env SeqOp [TyArg ty, LitArg lit]
  = returnSmpl (Lit (mkMachInt 1))

completePrim env op@SeqOp args@[TyArg ty, VarArg var]
  | isEvaluated (lookupRhsInfo env var) = returnSmpl (Lit (mkMachInt 1))  -- var is eval'd
  | otherwise				= returnSmpl (Prim op args)	  -- var not eval'd
\end{code}

\begin{code}
completePrim env op args
  = case args of
     [LitArg (MachChar char_lit)]      -> oneCharLit   op char_lit
     [LitArg (MachInt int_lit signed)] -> (if signed then oneIntLit else oneWordLit)
							  op int_lit
     [LitArg (MachFloat float_lit)]    -> oneFloatLit  op float_lit
     [LitArg (MachDouble double_lit)]  -> oneDoubleLit op double_lit
     [LitArg other_lit]		       -> oneLit       op other_lit

     [LitArg (MachChar char_lit1),
      LitArg (MachChar char_lit2)]     -> twoCharLits op char_lit1 char_lit2

     [LitArg (MachInt int_lit1 True),  -- both *signed* literals
      LitArg (MachInt int_lit2 True)]  -> twoIntLits op int_lit1 int_lit2

     [LitArg (MachInt int_lit1 False), -- both *unsigned* literals
      LitArg (MachInt int_lit2 False)] -> twoWordLits op int_lit1 int_lit2

     [LitArg (MachInt int_lit1 False), -- unsigned+signed (shift ops)
      LitArg (MachInt int_lit2 True)]  -> oneWordOneIntLit op int_lit1 int_lit2

     [LitArg (MachFloat float_lit1),
      LitArg (MachFloat float_lit2)]   -> twoFloatLits op float_lit1 float_lit2

     [LitArg (MachDouble double_lit1),
      LitArg (MachDouble double_lit2)] -> twoDoubleLits op double_lit1 double_lit2

     [LitArg lit, VarArg var]	       -> litVar op lit var
     [VarArg var, LitArg lit]	       -> litVar op lit var

     other			       -> give_up
  where
    give_up = returnSmpl (Prim op args)

    return_char c   = returnSmpl (Lit (MachChar   c))
    return_int i    = returnSmpl (Lit (mkMachInt  i))
    return_word i   = returnSmpl (Lit (mkMachWord i))
    return_float f  = returnSmpl (Lit (MachFloat  f))
    return_double d = returnSmpl (Lit (MachDouble d))
    return_lit lit  = returnSmpl (Lit lit)

    return_bool True  = returnSmpl trueVal
    return_bool False = returnSmpl falseVal

    return_prim_case var lit val_if_eq val_if_neq
      = newId (idType var)	`thenSmpl` \ unused_binder ->
	let
	    result
	      =	Case (Var var)
		  (PrimAlts [(lit,val_if_eq)]
		  (BindDefault unused_binder val_if_neq))
	in
	returnSmpl result

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

    twoIntLits IntAddOp	 i1 i2		 = return_int (i1+i2)
    twoIntLits IntSubOp	 i1 i2		 = return_int (i1-i2)
    twoIntLits IntMulOp	 i1 i2		 = return_int (i1*i2)
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
#if __GLASGOW_HASKELL__ <= 22
    oneFloatLit FloatExpOp  f	= return_float (exp f)
    oneFloatLit FloatLogOp  f	= return_float (log f)
    oneFloatLit FloatSqrtOp f	= return_float (sqrt f)
    oneFloatLit FloatSinOp  f	= return_float (sin f)
    oneFloatLit FloatCosOp  f	= return_float (cos f)
    oneFloatLit FloatTanOp  f	= return_float (tan f)
    oneFloatLit FloatAsinOp f	= return_float (asin f)
    oneFloatLit FloatAcosOp f	= return_float (acos f)
    oneFloatLit FloatAtanOp f	= return_float (atan f)
    oneFloatLit FloatSinhOp f	= return_float (sinh f)
    oneFloatLit FloatCoshOp f	= return_float (cosh f)
    oneFloatLit FloatTanhOp f	= return_float (tanh f)
#else
    -- hard to do all that in Rationals ?? (WDP 94/10) ToDo
#endif
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
    oneLit Addr2IntOp (MachAddr i) = return_int i
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


trueVal  = Con trueDataCon  []
falseVal = Con falseDataCon []
\end{code}
