%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-2000
%
\section[StgInterp]{Translates STG syntax to interpretable form, and run it}

\begin{code}

module StgInterp ( 

    ClosureEnv, ItblEnv, 
    filterRdrNameEnv,   -- :: [ModuleName] -> FiniteMap RdrName a 
			-- -> FiniteMap RdrName a

    linkIModules, 	-- :: ItblEnv -> ClosureEnv
	     		-- -> [([UnlinkedIBind], ItblEnv)]
	     		-- -> IO ([LinkedIBind], ItblEnv, ClosureEnv)

    iExprToHValue,	--  :: ItblEnv -> ClosureEnv 
			--  -> UnlinkedIExpr -> HValue

    stgBindsToInterpSyn,-- :: [StgBinding] 
	       		-- -> [TyCon] -> [Class] 
	       		-- -> IO ([UnlinkedIBind], ItblEnv)

    stgExprToInterpSyn, -- :: StgExpr
	       		-- -> IO UnlinkedIExpr

    interp		-- :: LinkedIExpr -> HValue
 ) where

{- -----------------------------------------------------------------------------

 ToDo:
   - link should be in the IO monad, so it can modify the symtabs as it
     goes along
 
   - need a way to remove the bindings for a module from the symtabs. 
     maybe the symtabs should be indexed by module first.

   - change the representation to something less verbose (?).

   - converting string literals to Addr# is horrible and introduces
     a memory leak.  See if something can be done about this.

----------------------------------------------------------------------------- -}

#include "HsVersions.h"

import Linker
import Id 		( Id, idPrimRep )
import Outputable
import Var
import PrimOp		( PrimOp(..) )
import PrimRep		( PrimRep(..) )
import Literal		( Literal(..) )
import Type		( Type, typePrimRep, deNoteType, repType, funResultTy )
import DataCon		( DataCon, dataConTag, dataConRepArgTys )
import ClosureInfo	( mkVirtHeapOffsets )
import Module		( ModuleName )
import Name		( toRdrName )
import UniqFM
import UniqSet

import {-# SOURCE #-} MCI_make_constr

import IOExts		( unsafePerformIO ) -- ToDo: remove
import PrelGHC		--( unsafeCoerce#, dataToTag#,
			--  indexPtrOffClosure#, indexWordOffClosure# )
import PrelAddr 	( Addr(..) )
import PrelFloat	( Float(..), Double(..) )
import Bits
import FastString
import GlaExts		( Int(..) )
import Module		( moduleNameFS )

import TyCon		( TyCon, isDataTyCon, tyConDataCons, tyConFamilySize )
import Class		( Class, classTyCon )
import InterpSyn
import StgSyn
import Addr
import RdrName		( RdrName, rdrNameModule, rdrNameOcc )
import FiniteMap
import Panic		( panic )
import OccName		( occNameString )
import ErrUtils		( showPass )
import CmdLineOpts	( DynFlags )

import Foreign
import CTypes
import IO

-- ---------------------------------------------------------------------------
-- Environments needed by the linker
-- ---------------------------------------------------------------------------

type ItblEnv    = FiniteMap RdrName (Ptr StgInfoTable)
type ClosureEnv = FiniteMap RdrName HValue
emptyClosureEnv = emptyFM

-- remove all entries for a given set of modules from the environment
filterRdrNameEnv :: [ModuleName] -> FiniteMap RdrName a -> FiniteMap RdrName a
filterRdrNameEnv mods env 
   = filterFM (\n _ -> rdrNameModule n `notElem` mods) env

-- ---------------------------------------------------------------------------
-- Turn an UnlinkedIExpr into a value we can run, for the interpreter
-- ---------------------------------------------------------------------------

iExprToHValue :: ItblEnv -> ClosureEnv -> UnlinkedIExpr -> IO HValue
iExprToHValue ie ce expr = return (interp (linkIExpr ie ce expr))

-- ---------------------------------------------------------------------------
-- Convert STG to an unlinked interpretable
-- ---------------------------------------------------------------------------

-- visible from outside
stgBindsToInterpSyn :: DynFlags
		    -> [StgBinding] 
	            -> [TyCon] -> [Class] 
	            -> IO ([UnlinkedIBind], ItblEnv)
stgBindsToInterpSyn dflags binds local_tycons local_classes
 = do showPass dflags "StgToInterp"
      let ibinds = concatMap (translateBind emptyUniqSet) binds
      let tycs   = local_tycons ++ map classTyCon local_classes
      itblenv <- mkITbls tycs
      return (ibinds, itblenv)

stgExprToInterpSyn :: DynFlags
		   -> StgExpr
	           -> IO UnlinkedIExpr
stgExprToInterpSyn dflags expr
 = do showPass dflags "StgToInterp"
      return (stg2expr emptyUniqSet expr)

translateBind :: UniqSet Id -> StgBinding -> [UnlinkedIBind]
translateBind ie (StgNonRec v e)  = [IBind v (rhs2expr ie e)]
translateBind ie (StgRec vs_n_es) = [IBind v (rhs2expr ie' e) | (v,e) <- vs_n_es]
  where ie' = addListToUniqSet ie (map fst vs_n_es)

isRec (StgNonRec _ _) = False
isRec (StgRec _)      = True

rhs2expr :: UniqSet Id -> StgRhs -> UnlinkedIExpr
rhs2expr ie (StgRhsClosure ccs binfo srt fvs uflag args rhs)
   = mkLambdas args
     where
        rhsExpr = stg2expr (addListToUniqSet ie args) rhs
        rhsRep  = repOfStgExpr rhs
        mkLambdas [] = rhsExpr
        mkLambdas (v:vs) = mkLam (repOfId v) rhsRep v (mkLambdas vs)
rhs2expr ie (StgRhsCon ccs dcon args)
   = conapp2expr ie dcon args

conapp2expr :: UniqSet Id -> DataCon -> [StgArg] -> UnlinkedIExpr
conapp2expr ie dcon args
   = mkConApp con_rdrname reps exprs
     where
	con_rdrname = toRdrName dcon
        exprs       = map (arg2expr ie) inHeapOrder
        reps        = map repOfArg inHeapOrder
        inHeapOrder = toHeapOrder args

        toHeapOrder :: [StgArg] -> [StgArg]
        toHeapOrder args
           = let (_, _, rearranged_w_offsets) = mkVirtHeapOffsets getArgPrimRep args
                 (rearranged, offsets) = unzip rearranged_w_offsets
             in
                 rearranged

foreign label "PrelBase_Izh_con_info" prelbase_Izh_con_info :: Addr

-- Handle most common cases specially; do the rest with a generic
-- mechanism (deferred till later :)
mkConApp :: RdrName -> [Rep] -> [UnlinkedIExpr] -> UnlinkedIExpr
mkConApp nm []               []         = ConApp    nm
mkConApp nm [RepI]           [a1]       = ConAppI   nm a1
mkConApp nm [RepP]           [a1]       = ConAppP   nm a1
mkConApp nm [RepP,RepP]      [a1,a2]    = ConAppPP  nm a1 a2
mkConApp nm [RepP,RepP,RepP] [a1,a2,a3] = ConAppPPP nm a1 a2 a3
mkConApp nm reps args
   = pprPanic "StgInterp.mkConApp: unhandled reps" (hsep (map ppr reps))

mkLam RepP RepP = LamPP
mkLam RepI RepP = LamIP
mkLam RepP RepI = LamPI
mkLam RepI RepI = LamII
mkLam repa repr = pprPanic "StgInterp.mkLam" (ppr repa <+> ppr repr)

mkApp RepP RepP = AppPP
mkApp RepI RepP = AppIP
mkApp RepP RepI = AppPI
mkApp RepI RepI = AppII
mkApp repa repr = pprPanic "StgInterp.mkApp" (ppr repa <+> ppr repr)

repOfId :: Id -> Rep
repOfId = primRep2Rep . idPrimRep

primRep2Rep primRep
   = case primRep of

	-- genuine lifted types
        PtrRep        -> RepP

	-- all these are unboxed, fit into a word, and we assume they
	-- all have the same call/return convention.
        IntRep        -> RepI
	CharRep       -> RepI
	WordRep       -> RepI
	AddrRep       -> RepI
	WeakPtrRep    -> RepI
	StablePtrRep  -> RepI

	-- these are pretty dodgy: really pointers, but
	-- we can't let the compiler build thunks with these reps.
	ForeignObjRep -> RepP
	StableNameRep -> RepP
	ThreadIdRep   -> RepP
	ArrayRep      -> RepP
	ByteArrayRep  -> RepP

	FloatRep      -> RepF
	DoubleRep     -> RepD

        other -> pprPanic "primRep2Rep" (ppr other)

repOfStgExpr :: StgExpr -> Rep
repOfStgExpr stgexpr
   = case stgexpr of
        StgLit lit 
           -> repOfLit lit
        StgCase scrut live liveR bndr srt alts
           -> case altRhss alts of
                 (a:_) -> repOfStgExpr a
                 []    -> panic "repOfStgExpr: no alts"
        StgApp var []
           -> repOfId var
        StgApp var args
           -> repOfApp ((deNoteType.repType.idType) var) (length args)

        StgPrimApp op args res_ty
           -> (primRep2Rep.typePrimRep) res_ty

        StgLet binds body -> repOfStgExpr body
        StgLetNoEscape live liveR binds body -> repOfStgExpr body

        StgConApp con args -> RepP -- by definition

        other 
           -> pprPanic "repOfStgExpr" (ppr other)
     where
        altRhss (StgAlgAlts tycon alts def)
           = [rhs | (dcon,bndrs,uses,rhs) <- alts] ++ defRhs def
        altRhss (StgPrimAlts tycon alts def)
           = [rhs | (lit,rhs) <- alts] ++ defRhs def
        defRhs StgNoDefault 
           = []
        defRhs (StgBindDefault rhs)
           = [rhs]

        -- returns the Rep of the result of applying ty to n args.
        repOfApp :: Type -> Int -> Rep
        repOfApp ty 0 = (primRep2Rep.typePrimRep) ty
        repOfApp ty n = repOfApp (funResultTy ty) (n-1)



repOfLit lit
   = case lit of
        MachInt _    -> RepI
        MachWord _   -> RepI
        MachAddr _   -> RepI
        MachChar _   -> RepI
        MachFloat _  -> RepF
        MachDouble _ -> RepD
        MachStr _    -> RepI   -- because it's a ptr outside the heap
        other -> pprPanic "repOfLit" (ppr lit)

lit2expr :: Literal -> UnlinkedIExpr
lit2expr lit
   = case lit of
        MachInt  i   -> case fromIntegral i of I# i -> LitI i
        MachWord i   -> case fromIntegral i of I# i -> LitI i
        MachAddr i   -> case fromIntegral i of I# i -> LitI i
	MachChar i   -> case fromIntegral i of I# i -> LitI i
	MachFloat f  -> case fromRational f of F# f -> LitF f
	MachDouble f -> case fromRational f of D# f -> LitD f
        MachStr s    -> 
	   case s of
     		CharStr s i -> LitI (addr2Int# s)

		FastString _ l ba -> 
		-- sigh, a string in the heap is no good to us.  We need a 
		-- static C pointer, since the type of a string literal is 
		-- Addr#.  So, copy the string into C land and introduce a 
		-- memory leak at the same time.
		  let n = I# l in
		 -- CAREFUL!  Chars are 32 bits in ghc 4.09+
		  case unsafePerformIO (do a@(Ptr addr) <- mallocBytes (n+1)
				 	   strncpy a ba (fromIntegral n)
				 	   writeCharOffAddr addr n '\0'
				 	   return addr)
		  of  A# a -> LitI (addr2Int# a)

     		_ -> error "StgInterp.lit2expr: unhandled string constant type"

        other -> pprPanic "lit2expr" (ppr lit)

stg2expr :: UniqSet Id -> StgExpr -> UnlinkedIExpr
stg2expr ie stgexpr
   = case stgexpr of
        StgApp var []
           -> mkVar ie (repOfId var) var

        StgApp var args
           -> mkAppChain ie (repOfStgExpr stgexpr) (mkVar ie (repOfId var) var) args
        StgLit lit
           -> lit2expr lit

        StgCase scrut live liveR bndr srt (StgPrimAlts ty alts def)
           |  repOfStgExpr scrut /= RepP
           -> mkCasePrim (repOfStgExpr stgexpr) 
                         bndr (stg2expr ie scrut) 
                              (map doPrimAlt alts) 
                              (def2expr def)

        StgCase scrut live liveR bndr srt (StgAlgAlts tycon alts def)
           |  repOfStgExpr scrut == RepP
           -> mkCaseAlg (repOfStgExpr stgexpr) 
                        bndr (stg2expr ie scrut) 
                             (map doAlgAlt alts) 
                             (def2expr def)

        StgPrimApp op args res_ty
           -> mkPrimOp (repOfStgExpr stgexpr)
                       op (map (arg2expr ie) args)

        StgConApp dcon args
           -> conapp2expr ie dcon args

        StgLet binds@(StgNonRec v e) body
	   -> mkNonRec (repOfStgExpr stgexpr) 
		(head (translateBind ie binds)) 
		(stg2expr (addOneToUniqSet ie v) body)

        StgLet binds@(StgRec bs) body
           -> mkRec (repOfStgExpr stgexpr) 
		(translateBind ie binds) 
		(stg2expr (addListToUniqSet ie (map fst bs)) body)

	-- treat let-no-escape just like let.
	StgLetNoEscape _ _ binds body
	   -> stg2expr ie (StgLet binds body)

        other
           -> pprPanic "stg2expr" (ppr stgexpr)
     where
        doPrimAlt (lit,rhs) 
           = AltPrim (lit2expr lit) (stg2expr ie rhs)
        doAlgAlt (dcon,vars,uses,rhs) 
           = AltAlg (dataConTag dcon - 1) 
                    (map id2VaaRep (toHeapOrder vars)) 
			(stg2expr (addListToUniqSet ie vars) rhs)

        toHeapOrder vars
           = let (_,_,rearranged_w_offsets) = mkVirtHeapOffsets idPrimRep vars
                 (rearranged,offsets)       = unzip rearranged_w_offsets
             in
                 rearranged

        def2expr StgNoDefault         = Nothing
        def2expr (StgBindDefault rhs) = Just (stg2expr ie rhs)

        mkAppChain ie result_rep so_far []
           = panic "mkAppChain"
        mkAppChain ie result_rep so_far [a]
           = mkApp (repOfArg a) result_rep so_far (arg2expr ie a)
        mkAppChain ie result_rep so_far (a:as)
           = mkAppChain ie result_rep (mkApp (repOfArg a) RepP so_far (arg2expr ie a)) as

mkCasePrim RepI = CasePrimI
mkCasePrim RepP = CasePrimP

mkCaseAlg  RepI = CaseAlgI
mkCaseAlg  RepP = CaseAlgP

-- any var that isn't in scope is turned into a Native
mkVar ie rep var
  | var `elementOfUniqSet` ie = 
	(case rep of
	   RepI -> VarI
	   RepF -> VarF
	   RepD -> VarD
	   RepP -> VarP)  var
  | otherwise = Native (toRdrName var)

mkRec RepI = RecI
mkRec RepP = RecP
mkNonRec RepI = NonRecI
mkNonRec RepP = NonRecP

mkPrimOp RepI = PrimOpI
mkPrimOp RepP = PrimOpP        

arg2expr :: UniqSet Id -> StgArg -> UnlinkedIExpr
arg2expr ie (StgVarArg v)   = mkVar ie (repOfId v) v
arg2expr ie (StgLitArg lit) = lit2expr lit
arg2expr ie (StgTypeArg ty) = pprPanic "arg2expr" (ppr ty)

repOfArg :: StgArg -> Rep
repOfArg (StgVarArg v)   = repOfId v
repOfArg (StgLitArg lit) = repOfLit lit
repOfArg (StgTypeArg ty) = pprPanic "repOfArg" (ppr ty)

id2VaaRep var = (var, repOfId var)


-- ---------------------------------------------------------------------------
-- Link interpretables into something we can run
-- ---------------------------------------------------------------------------

linkIModules :: ItblEnv    -- incoming global itbl env; returned updated
	     -> ClosureEnv -- incoming global closure env; returned updated
	     -> [([UnlinkedIBind], ItblEnv)]
	     -> IO ([LinkedIBind], ItblEnv, ClosureEnv)
linkIModules gie gce mods = do
  let (bindss, ies) = unzip mods
      binds  = concat bindss
      top_level_binders = map (toRdrName.binder) binds
      final_gie = foldr plusFM gie ies
  
  let {-rec-}
      new_gce = addListToFM gce (zip top_level_binders new_rhss)
      new_rhss = map (\b -> evalP (bindee b) emptyUFM) new_binds
    --vvvvvvvvv----------------------------------------^^^^^^^^^-- circular
      new_binds = linkIBinds final_gie new_gce binds

  return (new_binds, final_gie, new_gce)


-- We're supposed to augment the environments with the values of any
-- external functions/info tables we need as we go along, but that's a
-- lot of hassle so for now I'll look up external things as they crop
-- up and not cache them in the source symbol tables.  The interpreted
-- code will still be referenced in the source symbol tables.

linkIBinds :: ItblEnv -> ClosureEnv -> [UnlinkedIBind] -> [LinkedIBind]
linkIBinds ie ce binds = map (linkIBind ie ce) binds

linkIBind ie ce (IBind bndr expr) = IBind bndr (linkIExpr ie ce expr)

linkIExpr :: ItblEnv -> ClosureEnv -> UnlinkedIExpr -> LinkedIExpr
linkIExpr ie ce expr = case expr of

   CaseAlgP  bndr expr alts dflt -> 
	CaseAlgP bndr (linkIExpr ie ce expr) (linkAlgAlts ie ce alts)
	   		(linkDefault ie ce dflt)

   CaseAlgI  bndr expr alts dflt ->
	CaseAlgI bndr (linkIExpr ie ce expr) (linkAlgAlts ie ce alts)
	   		(linkDefault ie ce dflt)

   CasePrimP bndr expr alts dflt ->
	CasePrimP bndr (linkIExpr ie ce expr) (linkPrimAlts ie ce alts)
	   		(linkDefault ie ce dflt)

   CasePrimI bndr expr alts dflt ->
	CasePrimI bndr (linkIExpr ie ce expr) (linkPrimAlts ie ce alts)
	   		(linkDefault ie ce dflt)
   
   ConApp con -> 
	ConApp (lookupCon ie con)

   ConAppI   con arg0 -> 
	ConAppI (lookupCon ie con) (linkIExpr ie ce arg0)

   ConAppP   con arg0 ->
	ConAppP (lookupCon ie con) (linkIExpr ie ce arg0)

   ConAppPP  con arg0 arg1 -> 
	ConAppPP (lookupCon ie con) (linkIExpr ie ce arg0) (linkIExpr ie ce arg1)

   ConAppPPP con arg0 arg1 arg2 -> 
	ConAppPPP (lookupCon ie con) (linkIExpr ie ce arg0) 
			(linkIExpr ie ce arg1) (linkIExpr ie ce arg2)
   
   PrimOpI op args -> PrimOpI op (map (linkIExpr ie ce) args)
   PrimOpP op args -> PrimOpP op (map (linkIExpr ie ce) args)
   
   NonRecP bind expr  -> NonRecP (linkIBind ie ce bind) (linkIExpr ie ce expr)
   RecP    binds expr -> RecP (linkIBinds ie ce binds) (linkIExpr ie ce expr)
   
   NonRecI bind expr  -> NonRecI (linkIBind ie ce bind) (linkIExpr ie ce expr)
   RecI    binds expr -> RecI (linkIBinds ie ce binds) (linkIExpr ie ce expr)
   
   LitI i -> LitI i
   LitF i -> LitF i
   LitD i -> LitD i

   Native var -> lookupNative ce var
   
   VarP v -> lookupVar ce VarP v
   VarI v -> lookupVar ce VarI v
   
   LamPP  bndr expr -> LamPP bndr (linkIExpr ie ce expr)
   LamPI  bndr expr -> LamPI bndr (linkIExpr ie ce expr)
   LamIP  bndr expr -> LamIP bndr (linkIExpr ie ce expr)
   LamII  bndr expr -> LamII bndr (linkIExpr ie ce expr)
   
   AppPP  fun arg -> AppPP (linkIExpr ie ce fun) (linkIExpr ie ce arg)
   AppPI  fun arg -> AppPI (linkIExpr ie ce fun) (linkIExpr ie ce arg)
   AppIP  fun arg -> AppIP (linkIExpr ie ce fun) (linkIExpr ie ce arg)
   AppII  fun arg -> AppII (linkIExpr ie ce fun) (linkIExpr ie ce arg)

lookupCon ie con = 
  case lookupFM ie con of
    Just (Ptr addr) -> addr
    Nothing   -> 
	-- try looking up in the object files.
	case {-HACK!!!-}
		unsafePerformIO (lookupSymbol (rdrNameToCLabel con "con_info")) of
	    Just addr -> addr
	    Nothing   -> pprPanic "linkIExpr" (ppr con)

lookupNative ce var =
  case lookupFM ce var of
    Just e  -> Native e
    Nothing -> 
        -- try looking up in the object files.
	let lbl = (rdrNameToCLabel var "closure")
	    addr = unsafePerformIO (lookupSymbol lbl) in
	case {- trace (lbl ++ " -> " ++ show addr) $ -} addr of
	    Just (A# addr) -> Native (unsafeCoerce# addr)
	    Nothing   -> pprPanic "linkIExpr" (ppr var)

-- some VarI/VarP refer to top-level interpreted functions; we change
-- them into Natives here.
lookupVar ce f v =
  case lookupFM ce (toRdrName v) of
	Nothing -> f v
	Just e  -> Native e

-- HACK!!!  ToDo: cleaner
rdrNameToCLabel :: RdrName -> String{-suffix-} -> String
rdrNameToCLabel rn suffix = 
  _UNPK_(moduleNameFS (rdrNameModule rn)) 
  ++ '_':occNameString(rdrNameOcc rn) ++ '_':suffix

linkAlgAlts ie ce = map (linkAlgAlt ie ce)
linkAlgAlt ie ce (AltAlg tag args rhs) = AltAlg tag args (linkIExpr ie ce rhs)

linkPrimAlts ie ce = map (linkPrimAlt ie ce)
linkPrimAlt ie ce (AltPrim lit rhs)
   = AltPrim (linkIExpr ie ce lit) (linkIExpr ie ce rhs)

linkDefault ie ce Nothing = Nothing
linkDefault ie ce (Just expr) = Just (linkIExpr ie ce expr)

-- ---------------------------------------------------------------------------
-- The interpreter proper
-- ---------------------------------------------------------------------------

-- The dynamic environment contains everything boxed.
-- eval* functions which look up values in it will know the
-- representation of the thing they are looking up, so they
-- can cast/unbox it as necessary.

-- ---------------------------------------------------------------------------
-- Evaluator for things of boxed (pointer) representation
-- ---------------------------------------------------------------------------

interp :: LinkedIExpr -> HValue
interp iexpr = unsafeCoerce# (evalP iexpr emptyUFM)

evalP :: LinkedIExpr -> UniqFM boxed -> boxed

{-
evalP expr de
--   | trace ("evalP: " ++ showExprTag expr) False
   | trace ("evalP:\n" ++ showSDoc (pprIExpr expr) ++ "\n") False
   = error "evalP: ?!?!"
-}

evalP (Native p) de  = unsafeCoerce# p

-- First try the dynamic env.  If that fails, assume it's a top-level
-- binding and look in the static env.  That gives an Expr, which we
-- must convert to a boxed thingy by applying evalP to it.  Because
-- top-level bindings are always ptr-rep'd (either lambdas or boxed
-- CAFs), it's always safe to use evalP.
evalP (VarP v) de 
   = case lookupUFM de v of
        Just xx -> xx
        Nothing -> error ("evalP: lookupUFM " ++ show v)

-- Deal with application of a function returning a pointer rep
-- to arguments of any persuasion.  Note that the function itself
-- always has pointer rep.
evalP (AppIP e1 e2) de  = unsafeCoerce# (evalP e1 de) (evalI e2 de)
evalP (AppPP e1 e2) de  = unsafeCoerce# (evalP e1 de) (evalP e2 de)
evalP (AppFP e1 e2) de  = unsafeCoerce# (evalP e1 de) (evalF e2 de)
evalP (AppDP e1 e2) de  = unsafeCoerce# (evalP e1 de) (evalD e2 de)

-- Lambdas always return P-rep, but we need to do different things
-- depending on both the argument and result representations.
evalP (LamPP x b) de
   = unsafeCoerce# (\ xP -> evalP b (addToUFM de x xP))
evalP (LamPI x b) de
   = unsafeCoerce# (\ xP -> evalI b (addToUFM de x xP))
evalP (LamPF x b) de
   = unsafeCoerce# (\ xP -> evalF b (addToUFM de x xP))
evalP (LamPD x b) de
   = unsafeCoerce# (\ xP -> evalD b (addToUFM de x xP))
evalP (LamIP x b) de
   = unsafeCoerce# (\ xI -> evalP b (addToUFM de x (unsafeCoerce# (I# xI))))
evalP (LamII x b) de
   = unsafeCoerce# (\ xI -> evalI b (addToUFM de x (unsafeCoerce# (I# xI))))
evalP (LamIF x b) de
   = unsafeCoerce# (\ xI -> evalF b (addToUFM de x (unsafeCoerce# (I# xI))))
evalP (LamID x b) de
   = unsafeCoerce# (\ xI -> evalD b (addToUFM de x (unsafeCoerce# (I# xI))))
evalP (LamFP x b) de
   = unsafeCoerce# (\ xI -> evalP b (addToUFM de x (unsafeCoerce# (F# xI))))
evalP (LamFI x b) de
   = unsafeCoerce# (\ xI -> evalI b (addToUFM de x (unsafeCoerce# (F# xI))))
evalP (LamFF x b) de
   = unsafeCoerce# (\ xI -> evalF b (addToUFM de x (unsafeCoerce# (F# xI))))
evalP (LamFD x b) de
   = unsafeCoerce# (\ xI -> evalD b (addToUFM de x (unsafeCoerce# (F# xI))))
evalP (LamDP x b) de
   = unsafeCoerce# (\ xI -> evalP b (addToUFM de x (unsafeCoerce# (D# xI))))
evalP (LamDI x b) de
   = unsafeCoerce# (\ xI -> evalI b (addToUFM de x (unsafeCoerce# (D# xI))))
evalP (LamDF x b) de
   = unsafeCoerce# (\ xI -> evalF b (addToUFM de x (unsafeCoerce# (D# xI))))
evalP (LamDD x b) de
   = unsafeCoerce# (\ xI -> evalD b (addToUFM de x (unsafeCoerce# (D# xI))))


-- NonRec, Rec, CaseAlg and CasePrim are the same for all result reps, 
-- except in the sense that we go on and evaluate the body with whichever
-- evaluator was used for the expression as a whole.
evalP (NonRecP bind e) de
   = evalP e (augment_nonrec bind de)
evalP (RecP binds b) de
   = evalP b (augment_rec binds de)
evalP (CaseAlgP bndr expr alts def) de
   = case helper_caseAlg bndr expr alts def de of
        (rhs, de') -> evalP rhs de'
evalP (CasePrimP bndr expr alts def) de
   = case helper_casePrim bndr expr alts def de of
        (rhs, de') -> evalP rhs de'

{-
-- ConApp can only be handled by evalP
evalP (ConApp itbl args) se de
   = loop args
     where
        -- This appalling hack suggested (gleefully) by SDM
        -- It is not well typed (needless to say?)
        loop :: [Expr] -> boxed
        loop [] 
           = trace "loop-empty" (
             case itbl of A# addr# -> unsafeCoerce# (mci_make_constr addr#)
             )
        loop (a:as) 
           = trace "loop-not-empty" (
             case repOf a of
                RepI -> case evalI a de of i# -> loop as i#
                RepP -> let p = evalP a de in loop as p                
             )
-}

evalP (ConAppI (A# itbl) a1) de
   = case evalI a1 de of i1 -> mci_make_constrI itbl i1

evalP (ConApp (A# itbl)) de
   = mci_make_constr itbl

evalP (ConAppP (A# itbl) a1) de
   = let p1 = evalP a1 de
     in  mci_make_constrP itbl p1

evalP (ConAppPP (A# itbl) a1 a2) de
   = let p1 = evalP a1 de
         p2 = evalP a2 de
     in  mci_make_constrPP itbl p1 p2

evalP (ConAppPPP (A# itbl) a1 a2 a3) de
   = let p1 = evalP a1 de
         p2 = evalP a2 de
         p3 = evalP a3 de
     in  mci_make_constrPPP itbl p1 p2 p3



evalP other de
   = error ("evalP: unhandled case: " ++ showExprTag other)

--------------------------------------------------------
--- Evaluator for things of Int# representation
--------------------------------------------------------

-- Evaluate something which has an unboxed Int rep
evalI :: LinkedIExpr -> UniqFM boxed -> Int#

{-
evalI expr de
--   | trace ("evalI: " ++ showExprTag expr) False
   | trace ("evalI:\n" ++ showSDoc (pprIExpr expr) ++ "\n") False
   = error "evalI: ?!?!"
-}

evalI (LitI i#) de = i#

evalI (VarI v) de = 
   case lookupUFM de v of
	Just e  -> case unsafeCoerce# e of I# i -> i
	Nothing -> error ("evalI: lookupUFM " ++ show v)

-- Deal with application of a function returning an Int# rep
-- to arguments of any persuasion.  Note that the function itself
-- always has pointer rep.
evalI (AppII e1 e2) de 
   = unsafeCoerce# (evalP e1 de) (evalI e2 de)
evalI (AppPI e1 e2) de
   = unsafeCoerce# (evalP e1 de) (evalP e2 de)
evalI (AppFI e1 e2) de 
   = unsafeCoerce# (evalP e1 de) (evalF e2 de)
evalI (AppDI e1 e2) de
   = unsafeCoerce# (evalP e1 de) (evalD e2 de)

-- NonRec, Rec, CaseAlg and CasePrim are the same for all result reps, 
-- except in the sense that we go on and evaluate the body with whichever
-- evaluator was used for the expression as a whole.
evalI (NonRecI bind b) de
   = evalI b (augment_nonrec bind de)
evalI (RecI binds b) de
   = evalI b (augment_rec binds de)
evalI (CaseAlgI bndr expr alts def) de
   = case helper_caseAlg bndr expr alts def de of
        (rhs, de') -> evalI rhs de'
evalI (CasePrimI bndr expr alts def) de
   = case helper_casePrim bndr expr alts def de of
        (rhs, de') -> evalI rhs de'

-- evalI can't be applied to a lambda term, by defn, since those
-- are ptr-rep'd.

evalI (PrimOpI IntAddOp [e1,e2]) de  = evalI e1 de +# evalI e2 de
evalI (PrimOpI IntSubOp [e1,e2]) de  = evalI e1 de -# evalI e2 de

--evalI (NonRec (IBind v e) b) de
--   = evalI b (augment de v (eval e de))

evalI other de
   = error ("evalI: unhandled case: " ++ showExprTag other)

--------------------------------------------------------
--- Evaluator for things of Float# representation
--------------------------------------------------------

-- Evaluate something which has an unboxed Int rep
evalF :: LinkedIExpr -> UniqFM boxed -> Float#

{-
evalF expr de
--   | trace ("evalF: " ++ showExprTag expr) False
   | trace ("evalF:\n" ++ showSDoc (pprIExpr expr) ++ "\n") False
   = error "evalF: ?!?!"
-}

evalF (LitF f#) de = f#

evalF (VarF v) de = 
   case lookupUFM de v of
	Just e  -> case unsafeCoerce# e of F# i -> i
	Nothing -> error ("evalF: lookupUFM " ++ show v)

-- Deal with application of a function returning an Int# rep
-- to arguments of any persuasion.  Note that the function itself
-- always has pointer rep.
evalF (AppIF e1 e2) de 
   = unsafeCoerce# (evalP e1 de) (evalI e2 de)
evalF (AppPF e1 e2) de
   = unsafeCoerce# (evalP e1 de) (evalP e2 de)
evalF (AppFF e1 e2) de 
   = unsafeCoerce# (evalP e1 de) (evalF e2 de)
evalF (AppDF e1 e2) de
   = unsafeCoerce# (evalP e1 de) (evalD e2 de)

-- NonRec, Rec, CaseAlg and CasePrim are the same for all result reps, 
-- except in the sense that we go on and evaluate the body with whichever
-- evaluator was used for the expression as a whole.
evalF (NonRecF bind b) de
   = evalF b (augment_nonrec bind de)
evalF (RecF binds b) de
   = evalF b (augment_rec binds de)
evalF (CaseAlgF bndr expr alts def) de
   = case helper_caseAlg bndr expr alts def de of
        (rhs, de') -> evalF rhs de'
evalF (CasePrimF bndr expr alts def) de
   = case helper_casePrim bndr expr alts def de of
        (rhs, de') -> evalF rhs de'

-- evalF can't be applied to a lambda term, by defn, since those
-- are ptr-rep'd.

evalF (PrimOpF op _) de 
  = error ("evalF: unhandled primop: " ++ showSDoc (ppr op))

evalF other de
  = error ("evalF: unhandled case: " ++ showExprTag other)

--------------------------------------------------------
--- Evaluator for things of Double# representation
--------------------------------------------------------

-- Evaluate something which has an unboxed Int rep
evalD :: LinkedIExpr -> UniqFM boxed -> Double#

{-
evalD expr de
--   | trace ("evalD: " ++ showExprTag expr) False
   | trace ("evalD:\n" ++ showSDoc (pprIExpr expr) ++ "\n") False
   = error "evalD: ?!?!"
-}

evalD (LitD d#) de = d#

evalD (VarD v) de = 
   case lookupUFM de v of
	Just e  -> case unsafeCoerce# e of D# i -> i
	Nothing -> error ("evalD: lookupUFM " ++ show v)

-- Deal with application of a function returning an Int# rep
-- to arguments of any persuasion.  Note that the function itself
-- always has pointer rep.
evalD (AppID e1 e2) de 
   = unsafeCoerce# (evalP e1 de) (evalI e2 de)
evalD (AppPD e1 e2) de
   = unsafeCoerce# (evalP e1 de) (evalP e2 de)
evalD (AppFD e1 e2) de 
   = unsafeCoerce# (evalP e1 de) (evalF e2 de)
evalD (AppDD e1 e2) de
   = unsafeCoerce# (evalP e1 de) (evalD e2 de)

-- NonRec, Rec, CaseAlg and CasePrim are the same for all result reps, 
-- except in the sense that we go on and evaluate the body with whichever
-- evaluator was used for the expression as a whole.
evalD (NonRecD bind b) de
   = evalD b (augment_nonrec bind de)
evalD (RecD binds b) de
   = evalD b (augment_rec binds de)
evalD (CaseAlgD bndr expr alts def) de
   = case helper_caseAlg bndr expr alts def de of
        (rhs, de') -> evalD rhs de'
evalD (CasePrimD bndr expr alts def) de
   = case helper_casePrim bndr expr alts def de of
        (rhs, de') -> evalD rhs de'

-- evalD can't be applied to a lambda term, by defn, since those
-- are ptr-rep'd.

evalD (PrimOpD op _) de
  = error ("evalD: unhandled primop: " ++ showSDoc (ppr op))

evalD other de 
  = error ("evalD: unhandled case: " ++ showExprTag other)

--------------------------------------------------------
--- Helper bits and pieces
--------------------------------------------------------

-- Find the Rep of any Expr
repOf :: LinkedIExpr -> Rep

repOf (LamPP _ _)      = RepP 
repOf (LamPI _ _)      = RepP 
repOf (LamPF _ _)      = RepP 
repOf (LamPD _ _)      = RepP 
repOf (LamIP _ _)      = RepP 
repOf (LamII _ _)      = RepP 
repOf (LamIF _ _)      = RepP 
repOf (LamID _ _)      = RepP 
repOf (LamFP _ _)      = RepP 
repOf (LamFI _ _)      = RepP 
repOf (LamFF _ _)      = RepP 
repOf (LamFD _ _)      = RepP 
repOf (LamDP _ _)      = RepP 
repOf (LamDI _ _)      = RepP 
repOf (LamDF _ _)      = RepP 
repOf (LamDD _ _)      = RepP 

repOf (AppPP _ _)      = RepP
repOf (AppPI _ _)      = RepI
repOf (AppPF _ _)      = RepF
repOf (AppPD _ _)      = RepD
repOf (AppIP _ _)      = RepP
repOf (AppII _ _)      = RepI
repOf (AppIF _ _)      = RepF
repOf (AppID _ _)      = RepD
repOf (AppFP _ _)      = RepP
repOf (AppFI _ _)      = RepI
repOf (AppFF _ _)      = RepF
repOf (AppFD _ _)      = RepD
repOf (AppDP _ _)      = RepP
repOf (AppDI _ _)      = RepI
repOf (AppDF _ _)      = RepF
repOf (AppDD _ _)      = RepD

repOf (NonRecP _ _)    = RepP
repOf (NonRecI _ _)    = RepI
repOf (NonRecF _ _)    = RepF
repOf (NonRecD _ _)    = RepD

repOf (RecP _ _)       = RepP
repOf (RecI _ _)       = RepI
repOf (RecF _ _)       = RepF
repOf (RecD _ _)       = RepD

repOf (LitI _)         = RepI
repOf (LitF _)         = RepF
repOf (LitD _)         = RepD

repOf (Native _)       = RepP

repOf (VarP _)         = RepP
repOf (VarI _)         = RepI
repOf (VarF _)         = RepF
repOf (VarD _)         = RepD

repOf (PrimOpP _ _)    = RepP
repOf (PrimOpI _ _)    = RepI
repOf (PrimOpF _ _)    = RepF
repOf (PrimOpD _ _)    = RepD

repOf (ConApp _)       = RepP
repOf (ConAppI _ _)    = RepP
repOf (ConAppP _ _)    = RepP
repOf (ConAppPP _ _ _) = RepP
repOf (ConAppPPP _ _ _ _) = RepP

repOf (CaseAlgP _ _ _ _) = RepP
repOf (CaseAlgI _ _ _ _) = RepI
repOf (CaseAlgF _ _ _ _) = RepF
repOf (CaseAlgD _ _ _ _) = RepD

repOf (CasePrimP _ _ _ _) = RepP
repOf (CasePrimI _ _ _ _) = RepI
repOf (CasePrimF _ _ _ _) = RepF
repOf (CasePrimD _ _ _ _) = RepD

repOf other         
   = error ("repOf: unhandled case: " ++ showExprTag other)

-- how big (in words) is one of these
repSizeW :: Rep -> Int
repSizeW RepI = 1
repSizeW RepP = 1


-- Evaluate an expression, using the appropriate evaluator,
-- then box up the result.  Note that it's only safe to use this 
-- to create values to put in the environment.  You can't use it 
-- to create a value which might get passed to native code since that
-- code will have no idea that unboxed things have been boxed.
eval :: LinkedIExpr -> UniqFM boxed -> boxed
eval expr de
   = case repOf expr of
        RepI -> unsafeCoerce# (I# (evalI expr de))
        RepP -> evalP expr de
        RepF -> unsafeCoerce# (F# (evalF expr de))
        RepD -> unsafeCoerce# (D# (evalD expr de))

-- Evaluate the scrutinee of a case, select an alternative,
-- augment the environment appropriately, and return the alt
-- and the augmented environment.
helper_caseAlg :: Id -> LinkedIExpr -> [LinkedAltAlg] -> Maybe LinkedIExpr 
                  -> UniqFM boxed
                  -> (LinkedIExpr, UniqFM boxed)
helper_caseAlg bndr expr alts def de
   = let exprEv = evalP expr de
     in  
     exprEv `seq` -- vitally important; otherwise exprEv is never eval'd
     case select_altAlg (tagOf exprEv) alts def of
        (vars,rhs) -> (rhs, augment_from_constr (addToUFM de bndr exprEv) 
                                                exprEv (vars,1))

helper_casePrim :: Var -> LinkedIExpr -> [LinkedAltPrim] -> Maybe LinkedIExpr 
                   -> UniqFM boxed
                   -> (LinkedIExpr, UniqFM boxed)
helper_casePrim bndr expr alts def de
   = case repOf expr of
        -- Umm, can expr have any other rep?  Yes ...
        -- CharRep, DoubleRep, FloatRep.  What about string reps?
        RepI -> case evalI expr de of 
                   i# -> (select_altPrim alts def (LitI i#), 
                          addToUFM de bndr (unsafeCoerce# (I# i#)))


augment_from_constr :: UniqFM boxed -> a -> ([(Id,Rep)],Int) -> UniqFM boxed
augment_from_constr de con ([],offset) 
   = de
augment_from_constr de con ((v,rep):vs,offset)
   = let v_binding
            = case rep of
                 RepP -> indexPtrOffClosure con offset
                 RepI -> unsafeCoerce# (I# (indexIntOffClosure con offset))
     in
         augment_from_constr (addToUFM de v v_binding) con 
                             (vs,offset + repSizeW rep)

-- Augment the environment for a non-recursive let.
augment_nonrec :: LinkedIBind -> UniqFM boxed -> UniqFM boxed
augment_nonrec (IBind v e) de  = addToUFM de v (eval e de)

-- Augment the environment for a recursive let.
augment_rec :: [LinkedIBind] -> UniqFM boxed -> UniqFM boxed
augment_rec binds de
   = let vars   = map binder binds
         rhss   = map bindee binds
         rhs_vs = map (\rhs -> eval rhs de') rhss
         de'    = addListToUFM de (zip vars rhs_vs)
     in
         de'

-- a must be a constructor?
tagOf :: a -> Int
tagOf x = I# (dataToTag# x)

select_altAlg :: Int -> [LinkedAltAlg] -> Maybe LinkedIExpr -> ([(Id,Rep)],LinkedIExpr)
select_altAlg tag [] Nothing = error "select_altAlg: no match and no default?!"
select_altAlg tag [] (Just def) = ([],def)
select_altAlg tag ((AltAlg tagNo vars rhs):alts) def
   = if   tag == tagNo 
     then (vars,rhs) 
     else select_altAlg tag alts def

-- literal may only be a literal, not an arbitrary expression
select_altPrim :: [LinkedAltPrim] -> Maybe LinkedIExpr -> LinkedIExpr -> LinkedIExpr
select_altPrim [] Nothing    literal = error "select_altPrim: no match and no default?!"
select_altPrim [] (Just def) literal = def
select_altPrim ((AltPrim lit rhs):alts) def literal
   = if eqLits lit literal
     then rhs
     else select_altPrim alts def literal

eqLits (LitI i1#) (LitI i2#) = i1# ==# i2#


-- a is a constructor
indexPtrOffClosure :: a -> Int -> b
indexPtrOffClosure con (I# offset)
   = case indexPtrOffClosure# con offset of (# x #) -> x

indexIntOffClosure :: a -> Int -> Int#
indexIntOffClosure con (I# offset)
   = case wordToInt (W# (indexWordOffClosure# con offset)) of I# i# -> i#


------------------------------------------------------------------------
--- Manufacturing of info tables for DataCons defined in this module ---
------------------------------------------------------------------------

#if __GLASGOW_HASKELL__ <= 408
type ItblPtr = Addr
#else
type ItblPtr = Ptr StgInfoTable
#endif

-- Make info tables for the data decls in this module
mkITbls :: [TyCon] -> IO ItblEnv
mkITbls [] = return emptyFM
mkITbls (tc:tcs) = do itbls  <- mkITbl tc
                      itbls2 <- mkITbls tcs
                      return (itbls `plusFM` itbls2)

mkITbl :: TyCon -> IO ItblEnv
mkITbl tc
--   | trace ("TYCON: " ++ showSDoc (ppr tc)) False
--   = error "?!?!"
   | not (isDataTyCon tc) 
   = return emptyFM
   | n == length dcs  -- paranoia; this is an assertion.
   = make_constr_itbls dcs
     where
        dcs = tyConDataCons tc
        n   = tyConFamilySize tc

cONSTR :: Int
cONSTR = 1  -- as defined in ghc/includes/ClosureTypes.h

-- Assumes constructors are numbered from zero, not one
make_constr_itbls :: [DataCon] -> IO ItblEnv
make_constr_itbls cons
   | length cons <= 8
   = do is <- mapM mk_vecret_itbl (zip cons [0..])
	return (listToFM is)
   | otherwise
   = do is <- mapM mk_dirret_itbl (zip cons [0..])
	return (listToFM is)
     where
        mk_vecret_itbl (dcon, conNo)
           = mk_itbl dcon conNo (vecret_entry conNo)
        mk_dirret_itbl (dcon, conNo)
           = mk_itbl dcon conNo mci_constr_entry

        mk_itbl :: DataCon -> Int -> Addr -> IO (RdrName,ItblPtr)
        mk_itbl dcon conNo entry_addr
           = let (tot_wds, ptr_wds, _) 
                    = mkVirtHeapOffsets typePrimRep (dataConRepArgTys dcon)
                 ptrs = ptr_wds
                 nptrs  = tot_wds - ptr_wds
                 itbl  = StgInfoTable {
                           ptrs = fromIntegral ptrs, nptrs = fromIntegral nptrs,
                           tipe = fromIntegral cONSTR,
                           srtlen = fromIntegral conNo,
                           code0 = fromIntegral code0, code1 = fromIntegral code1,
                           code2 = fromIntegral code2, code3 = fromIntegral code3,
                           code4 = fromIntegral code4, code5 = fromIntegral code5,
                           code6 = fromIntegral code6, code7 = fromIntegral code7 
                        }
                 -- Make a piece of code to jump to "entry_label".
                 -- This is the only arch-dependent bit.
                 -- On x86, if entry_label has an address 0xWWXXYYZZ,
                 -- emit   movl $0xWWXXYYZZ,%eax  ;  jmp *%eax
                 -- which is
                 -- B8 ZZ YY XX WW FF E0
                 (code0,code1,code2,code3,code4,code5,code6,code7)
                    = (0xB8, byte 0 entry_addr_w, byte 1 entry_addr_w, 
                             byte 2 entry_addr_w, byte 3 entry_addr_w, 
                       0xFF, 0xE0, 
                       0x90 {-nop-})

                 entry_addr_w :: Word32
                 entry_addr_w = fromIntegral (addrToInt entry_addr)
             in
                 do addr <- malloc
                    putStrLn ("SIZE of itbl is " ++ show (sizeOf itbl))
                    putStrLn ("# ptrs  of itbl is " ++ show ptrs)
                    putStrLn ("# nptrs of itbl is " ++ show nptrs)
                    poke addr itbl
                    return (toRdrName dcon, addr `plusPtr` 8)


byte :: Int -> Word32 -> Word32
byte 0 w = w .&. 0xFF
byte 1 w = (w `shiftR` 8) .&. 0xFF
byte 2 w = (w `shiftR` 16) .&. 0xFF
byte 3 w = (w `shiftR` 24) .&. 0xFF


vecret_entry 0 = mci_constr1_entry
vecret_entry 1 = mci_constr2_entry
vecret_entry 2 = mci_constr3_entry
vecret_entry 3 = mci_constr4_entry
vecret_entry 4 = mci_constr5_entry
vecret_entry 5 = mci_constr6_entry
vecret_entry 6 = mci_constr7_entry
vecret_entry 7 = mci_constr8_entry

-- entry point for direct returns for created constr itbls
foreign label "stg_mci_constr_entry" mci_constr_entry :: Addr
-- and the 8 vectored ones
foreign label "stg_mci_constr1_entry" mci_constr1_entry :: Addr
foreign label "stg_mci_constr2_entry" mci_constr2_entry :: Addr
foreign label "stg_mci_constr3_entry" mci_constr3_entry :: Addr
foreign label "stg_mci_constr4_entry" mci_constr4_entry :: Addr
foreign label "stg_mci_constr5_entry" mci_constr5_entry :: Addr
foreign label "stg_mci_constr6_entry" mci_constr6_entry :: Addr
foreign label "stg_mci_constr7_entry" mci_constr7_entry :: Addr
foreign label "stg_mci_constr8_entry" mci_constr8_entry :: Addr



data Constructor = Constructor Int{-ptrs-} Int{-nptrs-}


-- Ultra-minimalist version specially for constructors
data StgInfoTable = StgInfoTable {
   ptrs :: Word16,
   nptrs :: Word16,
   srtlen :: Word16,
   tipe :: Word16,
   code0, code1, code2, code3, code4, code5, code6, code7 :: Word8
}


instance Storable StgInfoTable where

   sizeOf itbl 
      = (sum . map (\f -> f itbl))
        [fieldSz ptrs, fieldSz nptrs, fieldSz srtlen, fieldSz tipe,
         fieldSz code0, fieldSz code1, fieldSz code2, fieldSz code3, 
         fieldSz code4, fieldSz code5, fieldSz code6, fieldSz code7]

   alignment itbl 
      = (sum . map (\f -> f itbl))
        [fieldAl ptrs, fieldAl nptrs, fieldAl srtlen, fieldAl tipe,
         fieldAl code0, fieldAl code1, fieldAl code2, fieldAl code3, 
         fieldAl code4, fieldAl code5, fieldAl code6, fieldAl code7]

   poke a0 itbl
      = do a1 <- store (ptrs   itbl) (castPtr a0)
           a2 <- store (nptrs  itbl) a1
           a3 <- store (tipe   itbl) a2
           a4 <- store (srtlen itbl) a3
           a5 <- store (code0  itbl) a4
           a6 <- store (code1  itbl) a5
           a7 <- store (code2  itbl) a6
           a8 <- store (code3  itbl) a7
           a9 <- store (code4  itbl) a8
           aA <- store (code5  itbl) a9
           aB <- store (code6  itbl) aA
           aC <- store (code7  itbl) aB
           return ()

   peek a0
      = do (a1,ptrs)   <- load (castPtr a0)
           (a2,nptrs)  <- load a1
           (a3,tipe)   <- load a2
           (a4,srtlen) <- load a3
           (a5,code0)  <- load a4
           (a6,code1)  <- load a5
           (a7,code2)  <- load a6
           (a8,code3)  <- load a7
           (a9,code4)  <- load a8
           (aA,code5)  <- load a9
           (aB,code6)  <- load aA
           (aC,code7)  <- load aB
           return StgInfoTable { ptrs = ptrs, nptrs = nptrs, 
                                 srtlen = srtlen, tipe = tipe,
                                 code0 = code0, code1 = code1, code2 = code2,
                                 code3 = code3, code4 = code4, code5 = code5,
                                 code6 = code6, code7 = code7 }

fieldSz :: (Storable a, Storable b) => (a -> b) -> a -> Int
fieldSz sel x = sizeOf (sel x)

fieldAl :: (Storable a, Storable b) => (a -> b) -> a -> Int
fieldAl sel x = alignment (sel x)

store :: Storable a => a -> Ptr a -> IO (Ptr b)
store x addr = do poke addr x
                  return (castPtr (addr `plusPtr` sizeOf x))

load :: Storable a => Ptr a -> IO (Ptr b, a)
load addr = do x <- peek addr
               return (castPtr (addr `plusPtr` sizeOf x), x)

-----------------------------------------------------------------------------q

foreign import "strncpy" strncpy :: Ptr a -> ByteArray# -> CInt -> IO ()
\end{code}

