%
% (c) The AQUA Project, Glasgow University, 1993-1995
%

Machine- and flag- specific bits that the abstract code generator has
to know about.

No doubt there will be more...

\begin{code}
#include "HsVersions.h"

module MachDesc (
	Target(..){-(..) for target_STRICT only-}, mkTarget, RegLoc(..),

    	saveLoc,

	fixedHeaderSize, varHeaderSize, stgReg,
	sizeof, volatileSaves, volatileRestores, hpRel,
	amodeToStix, amodeToStix', charLikeClosureSize,
	intLikeClosureSize, mutHS, dataHS, primToStix, macroCode,
	heapCheck

	-- and, for self-sufficiency...
    ) where

import AbsCSyn
import CmdLineOpts  ( GlobalSwitch(..), stringSwitchSet, switchIsOn, SwitchResult(..) )
import Outputable
import OrdList	    ( OrdList )
import SMRep	    ( SMRep )
import Stix
import UniqSupply
import Unique
import Unpretty	    ( PprStyle, CSeq )
import Util

data RegLoc = Save StixTree | Always StixTree
\end{code}

Think of this as a big runtime class dictionary:
\begin{code}
data Target = Target
    Int     	    	    	    	-- fixedHeaderSize
    (SMRep -> Int)     	    	    	-- varHeaderSize
    (MagicId -> RegLoc)     	    	-- stgReg
    (PrimRep -> Int)     	    	-- sizeof
    (HeapOffset -> Int)	    	    	-- hpRel
    (CAddrMode -> StixTree)     	-- amodeToStix
    (CAddrMode -> StixTree)     	-- amodeToStix'
    (
    ([MagicId] -> [StixTree]),     	-- volatileSaves
    ([MagicId] -> [StixTree]),     	-- volatileRestores
    Int, 	    	    	    	-- charLikeClosureSize
    Int, 	    	    	    	-- intLikeClosureSize
    StixTree,    	    	    	-- mutHS
    StixTree,    	    	    	-- dataHS
    ([CAddrMode] -> PrimOp -> [CAddrMode] -> UniqSM StixTreeList),
    	    	    	    	    	-- primToStix
    (CStmtMacro -> [CAddrMode] -> UniqSM StixTreeList),
    	    	    	    	    	-- macroCode
    (StixTree -> StixTree -> StixTree -> UniqSM StixTreeList)
    	    	    	    	    	-- heapCheck
    )

mkTarget = Target

fixedHeaderSize (Target fhs vhs reg size hprel am am' ~(vsave, vrest, csz, isz, mhs, dhs, ps, mc, hc) ) = fhs
varHeaderSize (Target fhs vhs reg size hprel am am' ~(vsave, vrest, csz, isz, mhs, dhs, ps, mc, hc) ) x = vhs x
stgReg (Target fhs vhs reg size hprel am am' ~(vsave, vrest, csz, isz, mhs, dhs, ps, mc, hc) ) x = reg x
sizeof (Target fhs vhs reg size hprel am am' ~(vsave, vrest, csz, isz, mhs, dhs, ps, mc, hc) ) x = size x
-- used only for wrapper-hungry PrimOps:
hpRel (Target fhs vhs reg size hprel am am' ~(vsave, vrest, csz, isz, mhs, dhs, ps, mc, hc) ) x = hprel x
amodeToStix (Target fhs vhs reg size hprel am am' ~(vsave, vrest, csz, isz, mhs, dhs, ps, mc, hc) ) x = am x
amodeToStix' (Target fhs vhs reg size hprel am am' ~(vsave, vrest, csz, isz, mhs, dhs, ps, mc, hc) ) x = am' x

volatileSaves (Target fhs vhs reg size hprel am am' (vsave, vrest, csz, isz, mhs, dhs, ps, mc, hc) ) x = vsave x
-- used only for wrapper-hungry PrimOps:
volatileRestores (Target fhs vhs reg size hprel am am' (vsave, vrest, csz, isz, mhs, dhs, ps, mc, hc) ) x = vrest x
charLikeClosureSize (Target fhs vhs reg size hprel am am' (vsave, vrest, csz, isz, mhs, dhs, ps, mc, hc) ) = csz
intLikeClosureSize (Target fhs vhs reg size hprel am am' (vsave, vrest, csz, isz, mhs, dhs, ps, mc, hc) ) = isz
mutHS (Target fhs vhs reg size hprel am am' (vsave, vrest, csz, isz, mhs, dhs, ps, mc, hc) ) = mhs
dataHS (Target fhs vhs reg size hprel am am' (vsave, vrest, csz, isz, mhs, dhs, ps, mc, hc) ) = dhs
primToStix (Target fhs vhs reg size hprel am am' (vsave, vrest, csz, isz, mhs, dhs, ps, mc, hc) ) x y z = ps x y z
macroCode (Target fhs vhs reg size hprel am am' (vsave, vrest, csz, isz, mhs, dhs, ps, mc, hc) ) x y = mc x y
heapCheck (Target fhs vhs reg size hprel am am' (vsave, vrest, csz, isz, mhs, dhs, ps, mc, hc) ) x y z = hc x y z
\end{code}

Trees for register save locations
\begin{code}
saveLoc :: Target -> MagicId -> StixTree

saveLoc target reg = case stgReg target reg of {Always loc -> loc; Save loc -> loc}
\end{code}

