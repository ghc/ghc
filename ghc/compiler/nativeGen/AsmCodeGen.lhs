%
% (c) The AQUA Project, Glasgow University, 1993-1995
%

\begin{code}
#include "HsVersions.h"
#include "../../includes/platform.h"
#include "../../includes/GhcConstants.h"

module AsmCodeGen (
	writeRealAsm,
	dumpRealAsm,

	-- And, I guess we need these...
	AbstractC, GlobalSwitch, SwitchResult,
	UniqSupply, UniqSM(..)
    ) where

import AbsCSyn	    ( AbstractC )
import AbsCStixGen  ( genCodeAbstractC )
import PrelInfo	    ( PrimRep, PrimOp(..)
		      IF_ATTACK_PRAGMAS(COMMA tagOf_PrimOp)
			  IF_ATTACK_PRAGMAS(COMMA pprPrimOp)
		    )
import CmdLineOpts  ( GlobalSwitch(..), stringSwitchSet, switchIsOn, SwitchResult(..) )
import MachDesc
import Maybes	    ( Maybe(..) )
import Outputable
#if alpha_TARGET_ARCH
import AlphaDesc    ( mkAlpha )
#endif
#if i386_TARGET_ARCH
import I386Desc	    ( mkI386 )
#endif
#if sparc_TARGET_ARCH
import SparcDesc    ( mkSparc )
#endif
import Stix
import UniqSupply
import Unpretty
import Util
\end{code}

This is a generic assembly language generator for the Glasgow Haskell
Compiler.  It has been a long time in germinating, basically due to
time constraints and the large spectrum of design possibilities.
Presently it generates code for:
\begin{itemize}
\item Sparc
\end{itemize}
In the pipeline (sic) are plans and/or code for 680x0, 386/486.

The code generator presumes the presence of a working C port.  This is
because any code that cannot be compiled (e.g. @casm@s) is re-directed
via this route. It also help incremental development.  Because this
code generator is specially written for the Abstract C produced by the
Glasgow Haskell Compiler, several optimisation opportunities are open
to us that are not open to @gcc@.  In particular, we know that the A
and B stacks and the Heap are all mutually exclusive wrt. aliasing,
and that expressions have no side effects (all state transformations
are top level objects).

There are two main components to the code generator.
\begin{itemize}
\item Abstract C is considered in statements,
	with a Twig-like system handling each statement in turn.
\item A scheduler turns the tree of assembly language orderings
      into a sequence suitable for input to an assembler.
\end{itemize}
The @codeGenerate@ function returns the final assembly language output
(as a String).	We can return a string, because there is only one way
of printing the output suitable for assembler consumption. It also
allows limited abstraction of different machines from the Main module.

The first part is the actual assembly language generation.  First we
split up the Abstract C into individual functions, then consider
chunks in isolation, giving back an @OrdList@ of assembly language
instructions.  The generic algorithm is heavily inspired by Twig
(ref), but also draws concepts from (ref).  The basic idea is to
(dynamically) walk the Abstract C syntax tree, annotating it with
possible code matches.	For example, on the Sparc, a possible match
(with its translation) could be
@
   :=
   / \
  i   r2	=> ST r2,[r1]
  |
  r1
@
where @r1,r2@ are registers, and @i@ is an indirection.	 The Twig
bit twiddling algorithm for tree matching has been abandoned. It is
replaced with a more direct scheme.  This is because, after careful
consideration it is felt that the overhead of handling many bit
patterns would be heavier that simply looking at the syntax of the
tree at the node being considered, and dynamically choosing and
pruning rules.

The ultimate result of the first part is a Set of ordering lists of
ordering lists of assembly language instructions (yes, really!), where
each element in the set is basic chunk.	 Now several (generic)
simplifications and transformations can be performed.  This includes
ones that turn the the ordering of orderings into just a single
ordering list. (The equivalent of applying @concat@ to a list of
lists.) A lot of the re-ordering and optimisation is actually done
(generically) here!  The final part, the scheduler, can now be used on
this structure.	 The code sequence is optimised (obviously) to avoid
stalling the pipeline.	This part {\em has} to be heavily machine
dependent.

[The above seems to describe mostly dreamware.  -- JSM]

The flag that needs to be added is -fasm-<platform> where platform is one of
the choices below.

\begin{code}
writeRealAsm :: (GlobalSwitch -> SwitchResult) -> _FILE -> AbstractC -> UniqSupply -> PrimIO ()

writeRealAsm flags file absC uniq_supply
  = uppAppendFile file 80 (runNCG (code flags absC) uniq_supply)

dumpRealAsm :: (GlobalSwitch -> SwitchResult) -> AbstractC -> UniqSupply -> String

dumpRealAsm flags absC uniq_supply = uppShow 80 (runNCG (code flags absC) uniq_supply)

runNCG m uniq_supply = m uniq_supply

code flags absC =
    genCodeAbstractC target absC		    `thenUs` \ treelists ->
    let
	stix = map (map (genericOpt target)) treelists
    in
    codeGen {-target-} sty stix
  where
    sty = PprForAsm (switchIsOn flags) (underscore {-target-}) (fmtAsmLbl {-target-})

    (target, codeGen, underscore, fmtAsmLbl)
      = case stringSwitchSet flags AsmTarget of
#if ! OMIT_NATIVE_CODEGEN
# if alpha_TARGET_ARCH
    	Just _ {-???"alpha-dec-osf1"-} -> mkAlpha flags
# endif
# if i386_TARGET_ARCH
    	Just _ {-???"i386_unknown_linuxaout"-} -> mkI386 True flags
# endif
# if sparc_sun_sunos4_TARGET
    	Just _ {-???"sparc-sun-sunos4"-} -> mkSparc True flags
# endif
# if sparc_sun_solaris2_TARGET
    	Just _ {-???"sparc-sun-solaris2"-} -> mkSparc False flags
# endif
#endif
	_ -> error
	     ("ERROR:Trying to generate assembly language for an unsupported architecture\n"++
	      "(or one for which this build is not configured).")

\end{code}

%************************************************************************
%*									*
\subsection[NCOpt]{The Generic Optimiser}
%*									*
%************************************************************************

This is called between translating Abstract C to its Tree
and actually using the Native Code Generator to generate
the annotations.  It's a chance to do some strength reductions.

** Remember these all have to be machine independent ***

Note that constant-folding should have already happened, but we might have
introduced some new opportunities for constant-folding wrt address manipulations.

\begin{code}

genericOpt
    :: Target
    -> StixTree
    -> StixTree

\end{code}

For most nodes, just optimize the children.

\begin{code}
-- hacking with Uncle Will:
#define target_STRICT target@(Target _ _ _ _ _ _ _ _)

genericOpt target_STRICT (StInd pk addr) =
    StInd pk (genericOpt target addr)

genericOpt target (StAssign pk dst src) =
    StAssign pk (genericOpt target dst) (genericOpt target src)

genericOpt target (StJump addr) =
    StJump (genericOpt target addr)

genericOpt target (StCondJump addr test) =
    StCondJump addr (genericOpt target test)

genericOpt target (StCall fn pk args) =
    StCall fn pk (map (genericOpt target) args)

\end{code}

Fold indices together when the types match.

\begin{code}

genericOpt target (StIndex pk (StIndex pk' base off) off')
  | pk == pk' =
    StIndex pk (genericOpt target base)
    	       (genericOpt target (StPrim IntAddOp [off, off']))

genericOpt target (StIndex pk base off) =
    StIndex pk (genericOpt target base)
    	       (genericOpt target off)

\end{code}

For primOps, we first optimize the children, and then we try our hand
at some constant-folding.

\begin{code}

genericOpt target (StPrim op args) =
    primOpt op (map (genericOpt target) args)

\end{code}

Replace register leaves with appropriate StixTrees for the given target.
(Oh, so this is why we've been hauling the target around!)

\begin{code}

genericOpt target leaf@(StReg (StixMagicId id)) =
    case stgReg target id of
    	Always tree -> genericOpt target tree
    	Save _     -> leaf

genericOpt target other = other

\end{code}

Now, try to constant-fold the primOps.  The arguments have
already been optimized and folded.

\begin{code}

primOpt
    :: PrimOp	    	-- The operation from an StPrim
    -> [StixTree]   	-- The optimized arguments
    -> StixTree

primOpt op arg@[StInt x] =
    case op of
    	IntNegOp -> StInt (-x)
    	IntAbsOp -> StInt (abs x)
    	_ -> StPrim op arg

primOpt op args@[StInt x, StInt y] =
    case op of
    	CharGtOp -> StInt (if x > y then 1 else 0)
    	CharGeOp -> StInt (if x >= y then 1 else 0)
    	CharEqOp -> StInt (if x == y then 1 else 0)
    	CharNeOp -> StInt (if x /= y then 1 else 0)
    	CharLtOp -> StInt (if x < y then 1 else 0)
    	CharLeOp -> StInt (if x <= y then 1 else 0)
    	IntAddOp -> StInt (x + y)
    	IntSubOp -> StInt (x - y)
    	IntMulOp -> StInt (x * y)
    	IntQuotOp -> StInt (x `quot` y)
    	IntRemOp -> StInt (x `rem` y)
    	IntGtOp -> StInt (if x > y then 1 else 0)
    	IntGeOp -> StInt (if x >= y then 1 else 0)
    	IntEqOp -> StInt (if x == y then 1 else 0)
    	IntNeOp -> StInt (if x /= y then 1 else 0)
    	IntLtOp -> StInt (if x < y then 1 else 0)
    	IntLeOp -> StInt (if x <= y then 1 else 0)
    	_ -> StPrim op args

\end{code}

When possible, shift the constants to the right-hand side, so that we
can match for strength reductions.  Note that the code generator will
also assume that constants have been shifted to the right when possible.

\begin{code}
primOpt op [x@(StInt _), y] | commutableOp op = primOpt op [y, x]
\end{code}

We can often do something with constants of 0 and 1 ...

\begin{code}
primOpt op args@[x, y@(StInt 0)] =
    case op of
    	IntAddOp -> x
    	IntSubOp -> x
    	IntMulOp -> y
    	AndOp  -> y
    	OrOp   -> x
    	SllOp  -> x
    	SraOp  -> x
    	SrlOp  -> x
    	ISllOp -> x
    	ISraOp -> x
    	ISrlOp -> x
    	_ -> StPrim op args

primOpt op args@[x, y@(StInt 1)] =
    case op of
    	IntMulOp -> x
    	IntQuotOp -> x
    	IntRemOp -> StInt 0
    	_ -> StPrim op args
\end{code}

Now look for multiplication/division by powers of 2 (integers).

\begin{code}
primOpt op args@[x, y@(StInt n)] =
    case op of
    	IntMulOp -> case exact_log2 n of
	    Nothing -> StPrim op args
    	    Just p -> StPrim SllOp [x, StInt p]
    	IntQuotOp -> case exact_log2 n of
	    Nothing -> StPrim op args
    	    Just p -> StPrim SraOp [x, StInt p]
    	_ -> StPrim op args
\end{code}

Anything else is just too hard.

\begin{code}
primOpt op args = StPrim op args
\end{code}

The commutable ops are those for which we will try to move constants
to the right hand side for strength reduction.

\begin{code}
commutableOp :: PrimOp -> Bool

commutableOp CharEqOp = True
commutableOp CharNeOp = True
commutableOp IntAddOp = True
commutableOp IntMulOp = True
commutableOp AndOp = True
commutableOp OrOp = True
commutableOp IntEqOp = True
commutableOp IntNeOp = True
commutableOp IntegerAddOp = True
commutableOp IntegerMulOp = True
commutableOp FloatAddOp = True
commutableOp FloatMulOp = True
commutableOp FloatEqOp = True
commutableOp FloatNeOp = True
commutableOp DoubleAddOp = True
commutableOp DoubleMulOp = True
commutableOp DoubleEqOp = True
commutableOp DoubleNeOp = True
commutableOp _ = False
\end{code}

This algorithm for determining the $\log_2$ of exact powers of 2 comes
from gcc.  It requires bit manipulation primitives, so we have a ghc
version and an hbc version.  Other Haskell compilers are on their own.

\begin{code}
w2i x = word2Int# x
i2w x = int2Word# x
i2w_s x = (x::Int#)

exact_log2 :: Integer -> Maybe Integer
exact_log2 x
    | x <= 0 || x >= 2147483648 = Nothing
    | otherwise = case fromInteger x of
	I# x# -> if (w2i ((i2w x#) `and#` (i2w (0# -# x#))) /=# x#) then Nothing
    	         else Just (toInteger (I# (pow2 x#)))

    	    where pow2 x# | x# ==# 1# = 0#
    	    	    	  | otherwise = 1# +# pow2 (w2i (i2w x# `shiftr` i2w_s 1#))

		  shiftr x y = shiftRA# x y
\end{code}
