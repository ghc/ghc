%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[PrelVals]{Prelude values the compiler ``knows about''}

\begin{code}
#include "HsVersions.h"

module PrelVals where

import PrelFuns		-- help functions, types and things
import BasicLit		( mkMachInt, BasicLit(..), PrimKind )
import TysPrim
import TysWiredIn
#ifdef DPH
import TyPod		( mkPodNTy ,mkPodTy )
import TyProcs		( mkProcessorTy )
#endif {- Data Parallel Haskell -}

#ifndef DPH
import AbsUniType
import Id		( mkTemplateLocals, mkTupleCon, getIdUniType,
			  mkSpecId
			)
#else
import AbsUniType	( mkSigmaTy, mkDictTy, mkTyVarTy , SigmaType(..),
			  applyTyCon, splitType, specialiseTy
			)
import Id		( mkTemplateLocals, mkTupleCon, getIdUniType,
			  mkSpecId, mkProcessorCon
			)
#endif {- Data Parallel Haskell -}
import IdInfo

import Maybes		( Maybe(..) )
import PlainCore	-- to make unfolding templates
import Unique		-- *Key things
import Util
\end{code}

%************************************************************************
%*									*
\subsection[PrelVals-error-related]{@error@ and friends; @trace@}
%*									*
%************************************************************************

GHC randomly injects these into the code.

@patError#@ is just a version of @error@ for pattern-matching
failures.  It knows various ``codes'' which expand to longer
strings---this saves space!

@absent#@ is a thing we put in for ``absent'' arguments.  They jolly
well shouldn't be yanked on, but if one is, then you will get a
friendly message from @absent#@ (rather a totally random crash).

@parError#@ is a special version of @error@ which the compiler does
not know to be a bottoming Id.  It is used in the @_par_@ and @_seq_@
templates, but we don't ever expect to generate code for it.

\begin{code}
pc_bottoming_Id key mod name ty
 = pcMiscPrelId key mod name ty bottoming_info
 where
    bottoming_info = noIdInfo `addInfo` mkBottomStrictnessInfo
	-- these "bottom" out, no matter what their arguments

eRROR_ID
  = pc_bottoming_Id errorIdKey pRELUDE_BUILTIN SLIT("error") errorTy

pAT_ERROR_ID
  = pc_bottoming_Id patErrorIdKey pRELUDE_BUILTIN SLIT("patError#") errorTy

aBSENT_ERROR_ID
  = pc_bottoming_Id absentErrorIdKey pRELUDE_BUILTIN SLIT("absent#")
	(mkSigmaTy [alpha_tv] [] alpha)

pAR_ERROR_ID
  = pcMiscPrelId parErrorIdKey pRELUDE_BUILTIN SLIT("parError#")
    (mkSigmaTy [alpha_tv] [] alpha) noIdInfo

errorTy  :: UniType
errorTy  = mkSigmaTy [alpha_tv] [] (UniFun (mkListTy charTy) alpha)
\end{code}

We want \tr{_trace} (NB: name not in user namespace) to be wired in
because we don't want the strictness analyser to get ahold of it,
decide that the second argument is strict, evaluate that first (!!),
and make a jolly old mess.  Having \tr{_trace} wired in also helps when
attempting to re-export it---because it's in \tr{PreludeBuiltin}, it
won't get an \tr{import} declaration in the interface file, so the
importing-subsequently module needs to know it's magic.
\begin{code}
tRACE_ID
  = pcMiscPrelId traceIdKey pRELUDE_BUILTIN SLIT("_trace") traceTy
	(noIdInfo `addInfo` pcGenerateSpecs traceIdKey tRACE_ID noIdInfo traceTy)
  where
    traceTy = mkSigmaTy [alpha_tv] [] (UniFun (mkListTy charTy) (UniFun alpha alpha))
\end{code}

%************************************************************************
%*									*
\subsection[PrelVals-Integer-support]{To support @Integer@ and @String@ literals}
%*									*
%************************************************************************

\begin{code}
{- OLD:
int2IntegerId
  = pcMiscPrelId int2IntegerIdKey pRELUDE_BUILTIN SLIT("_int2Integer")
	(UniFun intTy integerTy)
	noIdInfo
-}

--------------------------------------------------------------------

unpackCStringId
  = pcMiscPrelId unpackCStringIdKey pRELUDE_PS SLIT("unpackPS#")
		 (addrPrimTy{-a char *-} `UniFun` stringTy) noIdInfo

unpackCString2Id -- for cases when a string has a NUL in it
  = pcMiscPrelId unpackCString2IdKey pRELUDE_PS SLIT("unpackPS2#")
		 (addrPrimTy{-a char *-}
	`UniFun` (intPrimTy -- length
	`UniFun` stringTy)) noIdInfo

--------------------------------------------------------------------
unpackCStringAppendId
  = pcMiscPrelId unpackCStringAppendIdKey pRELUDE_BUILTIN SLIT("unpackCStringAppend#")
				(addrPrimTy{-a "char *" pointer-} 
		`UniFun`	(stringTy
		`UniFun`	stringTy)) noIdInfo
  
--------------------------------------------------------------------

packStringForCId
  = pcMiscPrelId packCStringIdKey{-ToDo:rename-} pRELUDE_PS SLIT("_packStringForC")
	(UniFun stringTy byteArrayPrimTy) noIdInfo
\end{code}

OK, this is Will's idea: we should have magic values for Integers 0,
+1, +2, and -1 (go ahead, fire me):
\begin{code}
integerZeroId
  = pcMiscPrelId integerZeroIdKey     pRELUDE_CORE SLIT("__integer0")  integerTy noIdInfo
integerPlusOneId
  = pcMiscPrelId integerPlusOneIdKey  pRELUDE_CORE SLIT("__integer1")  integerTy noIdInfo
integerPlusTwoId
  = pcMiscPrelId integerPlusTwoIdKey  pRELUDE_CORE SLIT("__integer2")  integerTy noIdInfo
integerMinusOneId
  = pcMiscPrelId integerMinusOneIdKey pRELUDE_CORE SLIT("__integerm1") integerTy noIdInfo
\end{code}

%************************************************************************
%*									*
\subsection[PrelVals-parallel]{@seq@ and @par@: for parallel operation (only)}
%*									*
%************************************************************************

In the definitions that follow, we use the @TyVar@-based
alpha/beta/gamma types---not the usual @TyVarTemplate@ ones.

This is so the @TyVars@ in the @CoTyLams@ (@alpha_tyvar@, etc) match
up with those in the types of the {\em lambda-bound} template-locals
we create (using types @alpha_ty@, etc.).

\begin{code}
--------------------------------------------------------------------
-- seqId :: "_seq_", used w/ GRIP, etc., is really quite similar to
-- dangerousEval
{-
   OLDER:
   _seq_ = /\ a b -> \ x y -> case x of { _ -> y }

   OLD:
   _seq_ = /\ a b -> \ x y -> case seq# x y of { _Lift y' -> y' }

   NEW (95/05):
   _seq_ = /\ a b -> \ x::a y::b -> case seq# x of { 0# -> parError#; _ -> y; }

-}

seqId = pcMiscPrelId seqIdKey pRELUDE_BUILTIN SLIT("_seq_")
		  (mkSigmaTy [alpha_tv, beta_tv] []
		    (alpha `UniFun` (beta `UniFun` beta)))
		  (noIdInfo `addInfo_UF` (mkUnfolding EssentialUnfolding seq_template))
  where
    [x, y, z]
      = mkTemplateLocals [
    	{-x-} alpha_ty,
    	{-y-} beta_ty,
        {-z-} intPrimTy
    	]

    seq_template
      = CoTyLam alpha_tyvar
    	  (CoTyLam beta_tyvar
	     (mkCoLam [x, y] (
		CoCase (CoPrim SeqOp [alpha_ty] [CoVarAtom x]) (
		  CoPrimAlts
		    [(mkMachInt 0, CoTyApp (CoVar pAR_ERROR_ID) beta_ty)]
		    (CoBindDefault z (CoVar y))))))

--------------------------------------------------------------------
-- parId :: "_par_", also used w/ GRIP, etc.
{-
    OLDER:

    par = /\ a b -> \ x y -> case (par# (case x of { _ -> () })) of { _ -> y }

    OLD:

    _par_ = /\ a b -> \ x y -> case par# x y of { _Lift y' -> y' }

    NEW (95/05):

    _par_ = /\ a b -> \ x::a y::b -> case par# x of { 0# -> parError#; _ -> y; }

-}
parId = pcMiscPrelId parIdKey pRELUDE_BUILTIN SLIT("_par_")
		  (mkSigmaTy [alpha_tv, beta_tv] []
		    (alpha `UniFun` (beta `UniFun` beta)))
		  (noIdInfo `addInfo_UF` (mkUnfolding EssentialUnfolding par_template))
  where
    [x, y, z]
      = mkTemplateLocals [
    	{-x-} alpha_ty,
    	{-y-} beta_ty,
        {-z-} intPrimTy
    	]

    par_template
      = CoTyLam alpha_tyvar
    	  (CoTyLam beta_tyvar
	     (mkCoLam [x, y] (
		CoCase (CoPrim ParOp [alpha_ty] [CoVarAtom x]) (
		  CoPrimAlts
		    [(mkMachInt 0, CoTyApp (CoVar pAR_ERROR_ID) beta_ty)]
		    (CoBindDefault z (CoVar y))))))

-- forkId :: "_fork_", for *required* concurrent threads
{-
   _fork_ = /\ a b -> \ x::a y::b -> case fork# x of { 0# -> parError#; _ -> y; }
-}
forkId = pcMiscPrelId forkIdKey pRELUDE_BUILTIN SLIT("_fork_")
		  (mkSigmaTy [alpha_tv, beta_tv] []
		    (alpha `UniFun` (beta `UniFun` beta)))
		  (noIdInfo `addInfo_UF` (mkUnfolding EssentialUnfolding fork_template))
  where
    [x, y, z]
      = mkTemplateLocals [
    	{-x-} alpha_ty,
    	{-y-} beta_ty,
        {-z-} intPrimTy
    	]

    fork_template
      = CoTyLam alpha_tyvar
    	  (CoTyLam beta_tyvar
	     (mkCoLam [x, y] (
		CoCase (CoPrim ForkOp [alpha_ty] [CoVarAtom x]) (
		  CoPrimAlts
		    [(mkMachInt 0, CoTyApp (CoVar pAR_ERROR_ID) beta_ty)]
		    (CoBindDefault z (CoVar y))))))

\end{code}

\begin{code}
#ifdef GRAN

parLocalId = pcMiscPrelId parLocalIdKey pRELUDE_BUILTIN SLIT("_parLocal_")
		  (mkSigmaTy [alpha_tv, beta_tv] []
		    (intPrimTy `UniFun` (alpha `UniFun` (beta `UniFun` beta))))
		  (noIdInfo `addInfo_UF` (mkUnfolding EssentialUnfolding parLocal_template))
  where
    [w, x, y, z]
      = mkTemplateLocals [
	{-w-} intPrimTy,
    	{-x-} alpha_ty,
    	{-y-} beta_ty,
	{-z-} beta_ty
    	]

    parLocal_template
      = CoTyLam alpha_tyvar
    	  (CoTyLam beta_tyvar
	     (mkCoLam [w, x, y] (
		CoCase (CoPrim ParLocalOp [alpha_ty, beta_ty] [CoVarAtom x, CoVarAtom w, CoVarAtom y]) (
		  CoAlgAlts
		    [(liftDataCon, [z], CoVar z)]
		    (CoNoDefault)))))

parGlobalId = pcMiscPrelId parGlobalIdKey pRELUDE_BUILTIN SLIT("_parGlobal_")
		  (mkSigmaTy [alpha_tv, beta_tv] []
		    (intPrimTy `UniFun` (alpha `UniFun` (beta `UniFun` beta))))
		  (noIdInfo `addInfo_UF` (mkUnfolding EssentialUnfolding parGlobal_template))
  where
    [w, x, y, z]
      = mkTemplateLocals [
    	{-w-} intPrimTy,
    	{-x-} alpha_ty,
    	{-y-} beta_ty,
	{-z-} beta_ty
    	]

    parGlobal_template
      = CoTyLam alpha_tyvar
    	  (CoTyLam beta_tyvar
	     (mkCoLam [w, x, y] (
		CoCase (CoPrim ParGlobalOp [alpha_ty, beta_ty] [CoVarAtom x, CoVarAtom w, CoVarAtom y]) (
		  CoAlgAlts
		    [(liftDataCon, [z], CoVar z)]
		    (CoNoDefault)))))

#endif {-GRAN-}
\end{code}

\begin{code}
#ifdef DPH
vectorMapId = pcChooseToKnowId vectorMapU pRELUDE "vectorMap"
              (mkSigmaTy [alpha_tv, beta_tv , gamma_tv]
			 [(pidClass,alpha)]
	      ((beta `UniFun` gamma) 			 `UniFun`
	         ((mkPodTy (mkProcessorTy [alpha] beta)) `UniFun`
		    (mkPodTy (mkProcessorTy [alpha] gamma)))))
	      (panic "vectorMap:unfolding")--ToDo:DPH: (mkUnfoldTemplate vector_map_template)
	      [(2,"","")]
 where
{-
vectorMap fn vec = << (|x;fn y|) | (|x;y|) <<- vec >>

Simplified :
vectorMap :: for all a.83, b.82, c.86. <Pid a.83>
	  -> (b.82 -> c.86)
	  -> <<a.83;b.82>>
	  -> <<a.83;c.86>>
vectorMap =
    /\ t83 t82 o86 -> \ dict.127 ->
        let
          vecMap.128 =
              \ fn.129 vec.130 ->
                  << let si.133 = fn.129 ds.132 in
                     let
                       si.134 =
                           (fromDomain t82)
                               dict.127 ((toDomain t82) dict.127 ds.131)
                     in  MkProcessor1! Integer o86 si.134 si.133 |
                      (| ds.131 ; ds.132 |) <<- vec.130 >>
        in  vecMap.128

 NOTE : no need to bother with overloading in class Pid; because the result
	PID (si.133) is wrapped in fromDomain.toDomain == id . Therefore we
	use the simplification below.

Simplified:
vectorMap ::
    for all d.83, e.82, f.86.
        <Pid e.82> -> (d.83 -> f.86) -> <<e.82;d.83>> -> <<e.82;f.86>>
vectorMap =
    /\ t83 t82 o86 -> \ dict.127 fn.129 vec.130 ->
    << MkProcessor1! Integer o86 ds.131 (fn.129 ds.132) |
                      (| ds.131 ; ds.132 |) <<- vec.130 >>
-}

    vector_map_template
      = let
	   [dict,fn,vec,ds131,ds132]
	     = mkTemplateLocals
		    [mkDictTy pidClass alpha_ty,
		     beta_ty `UniFun` gamma_ty,
		     mkPodTy (mkProcessorTy [alpha_ty] beta_ty),
		     integerTy,
		     beta_ty]
	in
	  CoTyLam alpha_tyvar
	    (CoTyLam beta_tyvar
              (CoTyLam gamma_tyvar
	        (mkCoLam [dict,fn,vec]
	          (CoZfExpr
		    (CoCon (mkProcessorCon 1)
			   [integerTy,mkTyVarTy gamma_tyvar]
			   [CoVar ds131,
			    (CoApp (CoVar fn) (CoVar ds132))])
		    (CoDrawnGen [ds131] ds132 (CoVar vec)) ))))

#endif {- Data Parallel Haskell -}
\end{code}

\begin{code}
#ifdef DPH
-- A function used during podization that produces an index POD for a given
-- POD as argument.

primIfromPodNSelectorId :: Int -> Int -> Id
primIfromPodNSelectorId i n
   = pcMiscPrelId
	podSelectorIdKey
	pRELUDE_BUILTIN
        ("prim"++ show i ++ "fromPod" ++ show n ++ "Selector")
        (UniFun
	   (mkPodNTy n alpha)
	   (mkPodNTy n alpha))
	noIdInfo
#endif {- Data Parallel Haskell -}
\end{code}

%************************************************************************
%*									*
\subsection[PrelVals-deriving]{Values known about mainly for doing derived instance decls}
%*									*
%************************************************************************

map		:: (a -> b) -> [a] -> [b]
	-- this is up in the here-because-of-unfolding list

--??showChar	:: Char -> ShowS
showSpace	:: ShowS	-- non-std: == "showChar ' '"
showString	:: String -> ShowS
showParen	:: Bool -> ShowS -> ShowS

(++)		:: [a] -> [a] -> [a]
readParen	:: Bool -> ReadS a -> ReadS a
lex		:: ReadS String

\begin{code}
{- OLD:
readS_ty :: UniType -> UniType
readS_ty ty
  = UniFun stringTy (mkListTy (mkTupleTy 2 [ty, stringTy]))

showS_ty :: UniType
showS_ty = UniFun stringTy stringTy
-}
\end{code}

\begin{code}
{- OLD:
showSpaceId = pcMiscPrelId showSpaceIdKey pRELUDE_TEXT SLIT("_showSpace")
				showS_ty
				noIdInfo

showParenId = pcMiscPrelId showParenIdKey pRELUDE_TEXT SLIT("showParen")
				(boolTy `UniFun` (showS_ty `UniFun` showS_ty))
				noIdInfo

readParenId = pcMiscPrelId readParenIdKey pRELUDE_TEXT SLIT("readParen")
				(mkSigmaTy [alpha_tv] [] (
				 boolTy `UniFun` (
				 (readS_ty alpha) `UniFun` (readS_ty alpha))))
				noIdInfo

lexId = pcMiscPrelId lexIdKey pRELUDE_TEXT SLIT("lex")
				(readS_ty (mkListTy charTy))
				noIdInfo
-}
\end{code}

%************************************************************************
%*									*
\subsection[PrelVals-void]{@void#@: Magic value of type @Void#@}
%*									*
%************************************************************************

I don't think this is available to the user; it's used in the
simplifier (WDP 94/06).
\begin{code}
voidPrimId
  = pcMiscPrelId voidPrimIdKey pRELUDE_BUILTIN SLIT("void#")
	voidPrimTy noIdInfo
\end{code}

%************************************************************************
%*									*
\subsection[PrelVals-runST]{@_runST@: Magic start-state-transformer function}
%*									*
%************************************************************************

@_runST@ has a non-Haskell-able type:
\begin{verbatim}
-- _runST :: forall a. (forall s. _ST s a) -> a
-- which is to say ::
--	     forall a. (forall s. (_State s -> (a, _State s))) -> a

_runST a m = case m _RealWorld (S# _RealWorld realWorld#) of
               (r :: a, wild :: _State _RealWorld) -> r
\end{verbatim}
We unfold always, just for simplicity:
\begin{code}
runSTId
  = pcMiscPrelId runSTIdKey pRELUDE_BUILTIN SLIT("_runST") run_ST_ty id_info
  where
    s_tv = beta_tv
    s	 = beta

    st_ty a = mkSigmaTy [s_tv] [] (mkStateTransformerTy s a)

    run_ST_ty
      = mkSigmaTy [alpha_tv] [] (st_ty alpha `UniFun` alpha)
	    -- NB: rank-2 polymorphism! (forall inside the st_ty...)

    id_info
      = noIdInfo
	`addInfo` mkArityInfo 1
        `addInfo` mkStrictnessInfo [WwStrict] Nothing
	-- ABSOLUTELY NO UNFOLDING, e.g.: (mkUnfolding EssentialUnfolding run_ST_template)
	-- see example below
{- OUT:
    [m, t, r, wild]
      = mkTemplateLocals [
	{-m-} st_ty alpha_ty,
	{-t-} realWorldStateTy,
	{-r-} alpha_ty,
	{-_-} realWorldStateTy
	]

    run_ST_template
      = CoTyLam alpha_tyvar
	 (mkCoLam [m] (
	    CoLet (CoNonRec t (CoCon stateDataCon [realWorldTy] [CoVarAtom realWorldPrimId])) (
	      CoCase (CoApp (mkCoTyApp (CoVar m) realWorldTy) (CoVarAtom t)) (
		CoAlgAlts
		  [(mkTupleCon 2, [r, wild], CoVar r)]
		  CoNoDefault))))
-}
\end{code}

SLPJ 95/04: Why @_runST@ must not have an unfolding; consider:
\begin{verbatim}
f x =
  _runST ( \ s -> let
		    (a, s')  = newArray# 100 [] s
		    (_, s'') = fill_in_array_or_something a x s'
		  in
		  freezeArray# a s'' )
\end{verbatim}
If we inline @_runST@, we'll get:
\begin{verbatim}
f x = let
	(a, s')  = newArray# 100 [] realWorld#{-NB-}
	(_, s'') = fill_in_array_or_something a x s'
      in
      freezeArray# a s''
\end{verbatim}
And now the @newArray#@ binding can be floated to become a CAF, which
is totally and utterly wrong:
\begin{verbatim}
f = let
    (a, s')  = newArray# 100 [] realWorld#{-NB-} -- YIKES!!!
    in
    \ x ->
	let (_, s'') = fill_in_array_or_something a x s' in
	freezeArray# a s''
\end{verbatim}
All calls to @f@ will share a {\em single} array!  End SLPJ 95/04.

@realWorld#@ used to be a magic literal, \tr{void#}.  If things get
nasty as-is, change it back to a literal (@BasicLit@).
\begin{code}
realWorldPrimId
  = pcMiscPrelId realWorldPrimIdKey pRELUDE_BUILTIN SLIT("realWorld#")
	realWorldStatePrimTy
	noIdInfo
\end{code}

%************************************************************************
%*									*
\subsection[PrelVals-foldr-build]{Values known about for ``foldr/build''}
%*									*
%************************************************************************

\begin{code}
buildId
  = pcMiscPrelId buildIdKey pRELUDE_CORE SLIT("_build") buildTy
	((((noIdInfo 
		`addInfo_UF` mkMagicUnfolding SLIT("build"))
		`addInfo` mkStrictnessInfo [WwStrict] Nothing)
		`addInfo` mkArgUsageInfo [ArgUsage 2])
	        `addInfo` pcGenerateSpecs buildIdKey buildId noIdInfo{-ToDo-} buildTy)
	-- cheating, but since _build never actually exists ...
  where
    -- The type of this strange object is:
    --  \/ a . (\/ b . (a -> b -> b) -> b -> b) -> [a]

    buildTy = mkSigmaTy [alpha_tv] [] (buildUniTy `UniFun` (mkListTy alpha))
	where
	    buildUniTy = mkSigmaTy [beta_tv] []
		    ((alpha `UniFun` (beta `UniFun` beta))
			    `UniFun` (beta `UniFun` beta))
\end{code}

@mkBuild@ is sugar for building a build!

@mkbuild ty tv c n e@ $Rightarrow$ @build ty (/\ tv -> \ c n -> e)@
@ty@ is the type of the list.
@tv@ is always a new type variable.
@c,n@ are Id's for the abstract cons and nil, @g@ for let binding the argument argument.
	c :: a -> b -> b
	n :: b
	v :: (\/ b . (a -> b -> b) -> b -> b) -> [a]
--  \/ a .  (\/ b . (a -> b -> b) -> b -> b) -> [a]
@e@ is the object right inside the @build@

\begin{code}
mkBuild :: UniType
	-> TyVar
	-> Id
	-> Id
	-> Id
	-> PlainCoreExpr -- template
	-> PlainCoreExpr -- template

mkBuild ty tv c n g expr
 = CoLet (CoNonRec g (CoTyLam tv (mkCoLam [c,n] expr)))
	 (CoApp (mkCoTyApp (CoVar buildId) ty) (CoVarAtom g))
\end{code}

mkFoldr ty_a ty_b [x,y...] => foldr ty_a ty_b x y ..

\begin{code}
foldrId = pcMiscPrelId foldrIdKey pRELUDE_FB{-not "List"-} SLIT("foldr")
		 foldrTy idInfo
  where
	foldrTy =
	  mkSigmaTy [alpha_tv, beta_tv] []
		((alpha `UniFun` (beta `UniFun` beta))
		`UniFun` (beta
		`UniFun` ((mkListTy alpha)
		`UniFun` beta)))

	idInfo = (((((noIdInfo 
			`addInfo_UF` mkMagicUnfolding SLIT("foldr"))
			`addInfo` mkStrictnessInfo [WwLazy False,WwLazy False,WwStrict] Nothing)
			`addInfo` mkArityInfo 3)
			`addInfo` mkUpdateInfo [2,2,1])
	        	`addInfo` pcGenerateSpecs foldrIdKey foldrId noIdInfo{-ToDo-} foldrTy)

mkFoldr a b f z xs = foldl CoApp
			   (mkCoTyApps (CoVar foldrId) [a, b]) 
			   [CoVarAtom f,CoVarAtom z,CoVarAtom xs]

foldlId = pcMiscPrelId foldlIdKey pRELUDE_FB{-not "List"-} SLIT("foldl")
		 foldlTy idInfo
  where
	foldlTy =
	  mkSigmaTy [alpha_tv, beta_tv] []
		((alpha `UniFun` (beta `UniFun` alpha))
		`UniFun` (alpha
		`UniFun` ((mkListTy beta)
		`UniFun` alpha)))

	idInfo = (((((noIdInfo 
			`addInfo_UF` mkMagicUnfolding SLIT("foldl"))
			`addInfo` mkStrictnessInfo [WwLazy False,WwLazy False,WwStrict] Nothing)
			`addInfo` mkArityInfo 3)
			`addInfo` mkUpdateInfo [2,2,1])
	        	`addInfo` pcGenerateSpecs foldlIdKey foldlId noIdInfo{-ToDo-} foldlTy)

mkFoldl a b f z xs = foldl CoApp
			   (mkCoTyApps (CoVar foldlId) [a, b]) 
			   [CoVarAtom f,CoVarAtom z,CoVarAtom xs]

pRELUDE_FB = SLIT("PreludeFoldrBuild")
\end{code}
