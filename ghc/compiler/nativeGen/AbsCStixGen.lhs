%
% (c) The AQUA Project, Glasgow University, 1993-1998
%

\begin{code}
module AbsCStixGen ( genCodeAbstractC ) where

#include "HsVersions.h"

import Ratio	( Rational )

import AbsCSyn
import Stix
import MachMisc

import AbsCUtils	( getAmodeRep, mixedTypeLocn,
			  nonemptyAbsC, mkAbsCStmts
			)
import PprAbsC          ( dumpRealC )
import SMRep		( retItblSize )
import CLabel           ( CLabel, mkReturnInfoLabel, mkReturnPtLabel,
                          mkClosureTblLabel, mkClosureLabel,
			  labelDynamic, mkSplitMarkerLabel )
import ClosureInfo
import Literal		( Literal(..), word2IntLit )
import StgSyn		( StgOp(..) )
import MachOp		( MachOp(..), resultRepOfMachOp )
import PrimRep	    	( isFloatingRep, is64BitRep, 
			  PrimRep(..), getPrimRepSizeInBytes )
import StixMacro	( macroCode, checkCode )
import StixPrim		( foreignCallCode, amodeToStix, amodeToStix' )
import Outputable       ( pprPanic, ppr )
import UniqSupply	( returnUs, thenUs, mapUs, getUniqueUs, UniqSM )
import Util		( naturalMergeSortLe )
import Panic		( panic )
import TyCon		( tyConDataCons )
import Name             ( NamedThing(..) )
import CmdLineOpts	( opt_EnsureSplittableC )
import Outputable	( assertPanic )

-- DEBUGGING ONLY
--import TRACE		( trace )
--import Outputable	( showSDoc )
--import MachOp		( pprMachOp )

#include "nativeGen/NCG.h"
\end{code}

For each independent chunk of AbstractC code, we generate a list of
@StixTree@s, where each tree corresponds to a single Stix instruction.
We leave the chunks separated so that register allocation can be
performed locally within the chunk.

\begin{code}
genCodeAbstractC :: AbstractC -> UniqSM [StixStmt]

genCodeAbstractC absC
  = gentopcode absC
 where
 a2stix      = amodeToStix
 a2stix'     = amodeToStix'
 volsaves    = volatileSaves
 volrestores = volatileRestores
 -- real code follows... ---------
\end{code}

Here we handle top-level things, like @CCodeBlock@s and
@CClosureInfoTable@s.

\begin{code}
 {-
 genCodeTopAbsC
    :: AbstractC
    -> UniqSM [StixTree]
 -}

 gentopcode (CCodeBlock lbl absC)
  = gencode absC				`thenUs` \ code ->
    returnUs (StSegment TextSegment : StFunBegin lbl : code [StFunEnd lbl])

 gentopcode stmt@(CStaticClosure lbl closure_info _ _)
  = genCodeStaticClosure stmt			`thenUs` \ code ->
    returnUs ( StSegment DataSegment 
             : StLabel lbl : code []
             )

 gentopcode stmt@(CRetVector lbl amodes srt liveness)
  = returnUs ( StSegment TextSegment
	     : StData PtrRep table
	     : StLabel lbl
	     : []
	     )
  where
    table = map amodeToStix (mkVecInfoTable amodes srt liveness)

 gentopcode stmt@(CRetDirect uniq absC srt liveness)
  = gencode absC				       `thenUs` \ code ->
    returnUs ( StSegment TextSegment
	     : StData PtrRep table
	     : StLabel info_lbl
	     : StLabel ret_lbl
	     : code [])
  where 
    info_lbl = mkReturnInfoLabel uniq
    ret_lbl  = mkReturnPtLabel uniq
    table    = map amodeToStix (mkRetInfoTable ret_lbl srt liveness)

 gentopcode stmt@(CClosureInfoAndCode cl_info entry)
  = gencode entry			`thenUs` \ slow_code ->
    returnUs ( StSegment TextSegment
	     : StData PtrRep table
	     : StLabel info_lbl
	     : StFunBegin entry_lbl
	     : slow_code [StFunEnd entry_lbl])
  where
    entry_lbl = entryLabelFromCI cl_info
    info_lbl = infoTableLabelFromCI cl_info
    table    = map amodeToStix (mkInfoTable cl_info)

 gentopcode stmt@(CSRT lbl closures)
  = returnUs [ StSegment TextSegment 
	     , StLabel lbl 
	     , StData DataPtrRep (map mk_StCLbl_for_SRT closures)
	     ]
    where
       mk_StCLbl_for_SRT :: CLabel -> StixExpr
       mk_StCLbl_for_SRT label
          | labelDynamic label
          = StIndex Int8Rep (StCLbl label) (StInt 1)
          | otherwise
          = StCLbl label

 gentopcode stmt@(CBitmap l@(Liveness lbl size mask))
  = returnUs 
	[ StSegment TextSegment 
	, StLabel lbl 
	, StData WordRep (map StInt (toInteger size : map toInteger mask))
	]

 gentopcode stmt@(CSRTDesc lbl srt_lbl off len bitmap)
  = returnUs 
	[ StSegment TextSegment 
	, StLabel lbl 
	, StData WordRep (
		StIndex PtrRep (StCLbl srt_lbl) (StInt (toInteger off)) :
		map StInt (toInteger len : map toInteger bitmap)
	    )
	]

 gentopcode stmt@(CClosureTbl tycon)
  = returnUs [ StSegment TextSegment
             , StLabel (mkClosureTblLabel tycon)
             , StData DataPtrRep (map (StCLbl . mkClosureLabel . getName) 
                                      (tyConDataCons tycon) )
             ]

 gentopcode stmt@(CModuleInitBlock plain_lbl lbl absC)
  = gencode absC			`thenUs` \ code ->
    getUniqLabelNCG 	    	    	`thenUs` \ tmp_lbl ->
    getUniqLabelNCG 	    	    	`thenUs` \ flag_lbl ->
    returnUs ( StSegment DataSegment
	     : StLabel flag_lbl
	     : StData IntRep [StInt 0]
	     : StSegment TextSegment
	     : StLabel plain_lbl
	     : StJump NoDestInfo (StCLbl lbl)
	     : StLabel lbl
	     : StCondJump tmp_lbl (StMachOp MO_Nat_Ne
				     [StInd IntRep (StCLbl flag_lbl),
				      StInt 0])
	     : StAssignMem IntRep (StCLbl flag_lbl) (StInt 1)
	     : code 
	     [ StLabel tmp_lbl
	     , StAssignReg PtrRep stgSp
                           (StIndex PtrRep (StReg stgSp) (StInt (-1)))
	     , StJump NoDestInfo (StInd WordRep (StReg stgSp))
	     ])

 gentopcode absC
  = gencode absC				`thenUs` \ code ->
    returnUs (StSegment TextSegment : code [])
\end{code}

\begin{code}
 {-
 genCodeStaticClosure
    :: AbstractC
    -> UniqSM StixTreeList
 -}
 genCodeStaticClosure (CStaticClosure lbl cl_info cost_centre amodes)
  = returnUs (\xs -> table ++ xs)
  where
    table = StData PtrRep [StCLbl (infoTableLabelFromCI cl_info)] : 
	    foldr do_one_amode [] amodes

    do_one_amode amode rest
	| rep == VoidRep = rest
	| otherwise      = StData (promote_to_word rep) [a2stix amode] : rest
	where 
	  rep = getAmodeRep amode

    -- We need to promote any item smaller than a word to a word
    promote_to_word pk 
       | getPrimRepSizeInBytes pk >= getPrimRepSizeInBytes IntRep  = pk
       | otherwise                                                 = IntRep
\end{code}

Now the individual AbstractC statements.

\begin{code}
 {-
 gencode
    :: AbstractC
    -> UniqSM StixTreeList
 -}
\end{code}

@AbsCNop@s just disappear.

\begin{code}

 gencode AbsCNop = returnUs id

\end{code}

Split markers just insert a __stg_split_marker, which is caught by the
split-mangler later on and used to split the assembly into chunks.

\begin{code}

 gencode CSplitMarker
   | opt_EnsureSplittableC = returnUs (\xs -> StLabel mkSplitMarkerLabel : xs)
   | otherwise             = returnUs id

\end{code}

AbstractC instruction sequences are handled individually, and the
resulting StixTreeLists are joined together.

\begin{code}

 gencode (AbsCStmts c1 c2)
  = gencode c1				`thenUs` \ b1 ->
    gencode c2				`thenUs` \ b2 ->
    returnUs (b1 . b2)

 gencode (CSequential stuff)
  = foo stuff
    where
       foo [] = returnUs id
       foo (s:ss) = gencode s 	`thenUs` \ stix ->
                    foo ss 	`thenUs` \ stixes ->
                    returnUs (stix . stixes)

\end{code}

Initialising closure headers in the heap...a fairly complex ordeal if
done properly.	For now, we just set the info pointer, but we should
really take a peek at the flags to determine whether or not there are
other things to be done (setting cost centres, age headers, global
addresses, etc.)

\begin{code}

 gencode (CInitHdr cl_info reg_rel _ _)
  = let
	lhs = a2stix reg_rel
    	lbl = infoTableLabelFromCI cl_info
    in
	returnUs (\xs -> StAssignMem PtrRep lhs (StCLbl lbl) : xs)

\end{code}

Heap/Stack Checks.

\begin{code}

 gencode (CCheck macro args assts)
  = gencode assts `thenUs` \assts_stix ->
    checkCode macro args assts_stix

\end{code}

Assignment, the curse of von Neumann, is the center of the code we
produce.  In most cases, the type of the assignment is determined
by the type of the destination.  However, when the destination can
have mixed types, the type of the assignment is ``StgWord'' (we use
PtrRep for lack of anything better).  Think:  do we also want a cast
of the source?  Be careful about floats/doubles.

\begin{code}

 gencode (CAssign lhs rhs)
  | lhs_rep == VoidRep 
  = returnUs id
  | otherwise
  = let -- This is a Hack.  Should be cleaned up.
        -- JRS, 10 Dec 01
        pk' | ncg_target_is_32bit && is64BitRep lhs_rep
            = lhs_rep
            | otherwise
            = if   mixedTypeLocn lhs && not (isFloatingRep lhs_rep) 
              then IntRep 
              else lhs_rep
    	lhs' = a2stix lhs
    	rhs' = a2stix' rhs
    in
	returnUs (\xs -> mkStAssign pk' lhs' rhs' : xs)
    where 
       lhs_rep = getAmodeRep lhs

\end{code}

Unconditional jumps, including the special ``enter closure'' operation.
Note that the new entry convention requires that we load the InfoPtr (R2)
with the address of the info table before jumping to the entry code for Node.

For a vectored return, we must subtract the size of the info table to
get at the return vector.  This depends on the size of the info table,
which varies depending on whether we're profiling etc.

\begin{code}

 gencode (CJump dest)
  = returnUs (\xs -> StJump NoDestInfo (a2stix dest) : xs)

 gencode (CFallThrough (CLbl lbl _))
  = returnUs (\xs -> StFallThrough lbl : xs)

 gencode (CReturn dest DirectReturn)
  = returnUs (\xs -> StJump NoDestInfo (a2stix dest) : xs)

 gencode (CReturn table (StaticVectoredReturn n))
  = returnUs (\xs -> StJump NoDestInfo dest : xs)
  where
    dest = StInd PtrRep (StIndex PtrRep (a2stix table)
    	    	      	    	  (StInt (toInteger (-n-retItblSize-1))))

 gencode (CReturn table (DynamicVectoredReturn am))
  = returnUs (\xs -> StJump NoDestInfo dest : xs)
  where
    dest = StInd PtrRep (StIndex PtrRep (a2stix table) dyn_off)
    dyn_off = StMachOp MO_Nat_Sub [StMachOp MO_NatS_Neg [a2stix am], 
			           StInt (toInteger (retItblSize+1))]

\end{code}

Now the PrimOps, some of which may need caller-saves register wrappers.

\begin{code}
 gencode (COpStmt results (StgFCallOp fcall _) args vols)
  = ASSERT( null vols )
    foreignCallCode (nonVoid results) fcall (nonVoid args)

 gencode (COpStmt results (StgPrimOp op) args vols)
  = panic "AbsCStixGen.gencode: un-translated PrimOp"

 gencode (CMachOpStmt res mop args vols)
  = returnUs (\xs -> mkStAssign (resultRepOfMachOp mop) (a2stix res) 
                                (StMachOp mop (map a2stix args))
                     : xs
             )
\end{code}

Now the dreaded conditional jump.

Now the if statement.  Almost *all* flow of control are of this form.
@
	if (am==lit) { absC } else { absCdef }
@
	=>
@
	IF am = lit GOTO l1:
	absC
	jump l2:
   l1:
	absCdef
   l2:
@

\begin{code}

 gencode (CSwitch discrim alts deflt)
  = case alts of
      [] -> gencode deflt

      [(tag,alt_code)] -> case maybe_empty_deflt of
				Nothing -> gencode alt_code
				Just dc -> mkIfThenElse discrim tag alt_code dc

      [(tag1@(MachInt i1), alt_code1),
       (tag2@(MachInt i2), alt_code2)]
	| deflt_is_empty && i1 == 0 && i2 == 1
	-> mkIfThenElse discrim tag1 alt_code1 alt_code2
	| deflt_is_empty && i1 == 1 && i2 == 0
	-> mkIfThenElse discrim tag2 alt_code2 alt_code1

	-- If the @discrim@ is simple, then this unfolding is safe.
      other | simple_discrim -> mkSimpleSwitches discrim alts deflt

	-- Otherwise, we need to do a bit of work.
      other ->  getUniqueUs		      	  `thenUs` \ u ->
		gencode (AbsCStmts
		(CAssign (CTemp u pk) discrim)
		(CSwitch (CTemp u pk) alts deflt))

  where
    maybe_empty_deflt = nonemptyAbsC deflt
    deflt_is_empty = case maybe_empty_deflt of
			Nothing -> True
			Just _  -> False

    pk = getAmodeRep discrim

    simple_discrim = case discrim of
			CReg _    -> True
			CTemp _ _ -> True
			other	  -> False
\end{code}



Finally, all of the disgusting AbstractC macros.

\begin{code}

 gencode (CMacroStmt macro args) = macroCode macro (map amodeToStix args)

 gencode (CCallProfCtrMacro macro _)
  = returnUs (\xs -> StComment macro : xs)

 gencode (CCallProfCCMacro macro _)
  = returnUs (\xs -> StComment macro : xs)

 gencode CCallTypedef{} = returnUs id

 gencode other
  = pprPanic "AbsCStixGen.gencode" (dumpRealC other)

 nonVoid = filter ((/= VoidRep) . getAmodeRep)
\end{code}

Here, we generate a jump table if there are more than four (integer)
alternatives and the jump table occupancy is greater than 50%.
Otherwise, we generate a binary comparison tree.  (Perhaps this could
be tuned.)

\begin{code}

 intTag :: Literal -> Integer
 intTag (MachChar c)  = toInteger c
 intTag (MachInt i)   = i
 intTag (MachWord w)  = intTag (word2IntLit (MachWord w))
 intTag _             = panic "intTag"

 fltTag :: Literal -> Rational

 fltTag (MachFloat f)  = f
 fltTag (MachDouble d) = d
 fltTag x              = pprPanic "fltTag" (ppr x)

 {-
 mkSimpleSwitches
    :: CAddrMode -> [(Literal,AbstractC)] -> AbstractC
    -> UniqSM StixTreeList
 -}
 mkSimpleSwitches am alts absC
  = getUniqLabelNCG 	    	    	    	    	`thenUs` \ udlbl ->
    getUniqLabelNCG 	    	    	    	    	`thenUs` \ ujlbl ->
    let am' = a2stix am
    	joinedAlts = map (\ (tag,code) -> (tag, mkJoin code ujlbl)) alts
    	sortedAlts = naturalMergeSortLe leAlt joinedAlts
    	    	     -- naturalMergeSortLe, because we often get sorted alts to begin with

    	lowTag = intTag (fst (head sortedAlts))
    	highTag = intTag (fst (last sortedAlts))

    	-- lowest and highest possible values the discriminant could take
    	lowest = if floating then targetMinDouble else targetMinInt
    	highest = if floating then targetMaxDouble else targetMaxInt
    in
    	(
    	if  not floating && choices > 4 
            && highTag - lowTag < toInteger (2 * choices)
        then
    	    mkJumpTable am' sortedAlts lowTag highTag udlbl
    	else
    	    mkBinaryTree am' floating sortedAlts choices lowest highest udlbl
    	)
    	    	    	    	    	    	`thenUs` \ alt_code ->
	gencode absC				`thenUs` \ dflt_code ->

    	returnUs (\xs -> alt_code (StLabel udlbl : dflt_code (StLabel ujlbl : xs)))

    where
    	floating = isFloatingRep (getAmodeRep am)
    	choices = length alts

    	(x@(MachChar _),_)  `leAlt` (y,_) = intTag x <= intTag y
    	(x@(MachInt _), _)  `leAlt` (y,_) = intTag x <= intTag y
    	(x@(MachWord _), _) `leAlt` (y,_) = intTag x <= intTag y
    	(x,_)               `leAlt` (y,_) = fltTag x <= fltTag y

\end{code}

We use jump tables when doing an integer switch on a relatively dense
list of alternatives.  We expect to be given a list of alternatives,
sorted by tag, and a range of values for which we are to generate a
table.  Of course, the tags of the alternatives should lie within the
indicated range.  The alternatives need not cover the range; a default
target is provided for the missing alternatives.

If a join is necessary after the switch, the alternatives should
already finish with a jump to the join point.

\begin{code}
 {-
 mkJumpTable
    :: StixTree  	    	-- discriminant
    -> [(Literal, AbstractC)] 	-- alternatives
    -> Integer 	    	    	-- low tag
    -> Integer 	    	    	-- high tag
    -> CLabel	    	    	-- default label
    -> UniqSM StixTreeList
 -}

 mkJumpTable am alts lowTag highTag dflt
  = getUniqLabelNCG 	    	    	    	    	`thenUs` \ utlbl ->
    mapUs genLabel alts 	  	    	    	`thenUs` \ branches ->
    let	cjmpLo = StCondJump dflt (StMachOp MO_NatS_Lt [am, StInt (toInteger lowTag)])
    	cjmpHi = StCondJump dflt (StMachOp MO_NatS_Gt [am, StInt (toInteger highTag)])

    	offset = StMachOp MO_Nat_Sub [am, StInt lowTag]
        dsts   = DestInfo (dflt : map fst branches)

    	jump = StJump dsts (StInd PtrRep (StIndex PtrRep (StCLbl utlbl) offset))
    	tlbl = StLabel utlbl
    	table = StData PtrRep (mkTable branches [lowTag..highTag] [])
    in
    	mapUs mkBranch branches       	    	    	`thenUs` \ alts ->

	returnUs (\xs -> cjmpLo : cjmpHi : jump :
			 StSegment DataSegment : tlbl : table :
			 StSegment TextSegment : foldr1 (.) alts xs)

    where
    	genLabel x = getUniqLabelNCG `thenUs` \ lbl -> returnUs (lbl, x)

    	mkBranch (lbl,(_,alt)) =
	    gencode alt  	    		`thenUs` \ alt_code ->
    	    returnUs (\xs -> StLabel lbl : alt_code xs)

    	mkTable _  []     tbl = reverse tbl
    	mkTable [] (x:xs) tbl = mkTable [] xs (StCLbl dflt : tbl)
    	mkTable alts@((lbl,(tag,_)):rest) (x:xs) tbl
    	  | intTag tag == x = mkTable rest xs (StCLbl lbl : tbl)
    	  | otherwise = mkTable alts xs (StCLbl dflt : tbl)

\end{code}

We generate binary comparison trees when a jump table is inappropriate.
We expect to be given a list of alternatives, sorted by tag, and for
convenience, the length of the alternative list.  We recursively break
the list in half and do a comparison on the first tag of the second half
of the list.  (Odd lists are broken so that the second half of the list
is longer.)  We can handle either integer or floating kind alternatives,
so long as they are not mixed.  (We assume that the type of the discriminant
determines the type of the alternatives.)

As with the jump table approach, if a join is necessary after the switch, the
alternatives should already finish with a jump to the join point.

\begin{code}
 {-
 mkBinaryTree
    :: StixTree  	    	-- discriminant
    -> Bool 	    	    	-- floating point?
    -> [(Literal, AbstractC)] 	-- alternatives
    -> Int  	    	    	-- number of choices
    -> Literal     	    	-- low tag
    -> Literal     	    	-- high tag
    -> CLabel	    	    	-- default code label
    -> UniqSM StixTreeList
 -}

 mkBinaryTree am floating [(tag,alt)] _ lowTag highTag udlbl
  | rangeOfOne = gencode alt
  | otherwise
  = let	tag' = a2stix (CLit tag)
    	cmpOp = if floating then MO_Dbl_Ne else MO_Nat_Ne
    	test = StMachOp cmpOp [am, tag']
    	cjmp = StCondJump udlbl test
    in
    	gencode alt 	    	    	    	`thenUs` \ alt_code ->
	returnUs (\xs -> cjmp : alt_code xs)

    where
    	rangeOfOne = not floating && intTag lowTag + 1 >= intTag highTag
    	-- When there is only one possible tag left in range, we skip the comparison

 mkBinaryTree am floating alts choices lowTag highTag udlbl
  = getUniqLabelNCG					`thenUs` \ uhlbl ->
    let tag' = a2stix (CLit splitTag)
    	cmpOp = if floating then MO_Dbl_Ge else MO_NatS_Ge
    	test = StMachOp cmpOp [am, tag']
    	cjmp = StCondJump uhlbl test
    in
    	mkBinaryTree am floating alts_lo half lowTag splitTag udlbl
    	    	    	    	    	    	  	`thenUs` \ lo_code ->
    	mkBinaryTree am floating alts_hi (choices - half) splitTag highTag udlbl
    	    	    	    	    		    	`thenUs` \ hi_code ->

	returnUs (\xs -> cjmp : lo_code (StLabel uhlbl : hi_code xs))

    where
    	half = choices `div` 2
    	(alts_lo, alts_hi) = splitAt half alts
    	splitTag = fst (head alts_hi)

\end{code}

\begin{code}
 {-
 mkIfThenElse
    :: CAddrMode    	    -- discriminant
    -> Literal     	    -- tag
    -> AbstractC    	    -- if-part
    -> AbstractC    	    -- else-part
    -> UniqSM StixTreeList
 -}

 mkIfThenElse discrim tag alt deflt
  = getUniqLabelNCG					`thenUs` \ ujlbl ->
    getUniqLabelNCG					`thenUs` \ utlbl ->
    let discrim' = a2stix discrim
    	tag' = a2stix (CLit tag)
    	cmpOp = if (isFloatingRep (getAmodeRep discrim)) then MO_Dbl_Ne else MO_Nat_Ne
    	test = StMachOp cmpOp [discrim', tag']
    	cjmp = StCondJump utlbl test
    	dest = StLabel utlbl
    	join = StLabel ujlbl
    in
	gencode (mkJoin alt ujlbl)		`thenUs` \ alt_code ->
	gencode deflt				`thenUs` \ dflt_code ->
	returnUs (\xs -> cjmp : alt_code (dest : dflt_code (join : xs)))


mkJoin :: AbstractC -> CLabel -> AbstractC
mkJoin code lbl
  | mightFallThrough code = mkAbsCStmts code (CJump (CLbl lbl PtrRep))
  | otherwise = code
\end{code}

%---------------------------------------------------------------------------

This answers the question: Can the code fall through to the next
line(s) of code?  This errs towards saying True if it can't choose,
because it is used for eliminating needless jumps.  In other words, if
you might possibly {\em not} jump, then say yes to falling through.

\begin{code}
mightFallThrough :: AbstractC -> Bool

mightFallThrough absC = ft absC True
 where
  ft AbsCNop	   if_empty = if_empty

  ft (CJump _)       if_empty = False
  ft (CReturn _ _)   if_empty = False
  ft (CSwitch _ alts deflt) if_empty
	= ft deflt if_empty ||
	  or [ft alt if_empty | (_,alt) <- alts]

  ft (AbsCStmts c1 c2) if_empty = ft c2 (ft c1 if_empty)
  ft _ if_empty = if_empty

{- Old algorithm, which called nonemptyAbsC for every subexpression! =========
fallThroughAbsC (AbsCStmts c1 c2)
  = case nonemptyAbsC c2 of
	Nothing -> fallThroughAbsC c1
	Just x -> fallThroughAbsC x
fallThroughAbsC (CJump _)	 = False
fallThroughAbsC (CReturn _ _)	 = False
fallThroughAbsC (CSwitch _ choices deflt)
  = (not (isEmptyAbsC deflt) && fallThroughAbsC deflt)
    || or (map (fallThroughAbsC . snd) choices)
fallThroughAbsC other		 = True

isEmptyAbsC :: AbstractC -> Bool
isEmptyAbsC = not . maybeToBool . nonemptyAbsC
================= End of old, quadratic, algorithm -}
\end{code}
