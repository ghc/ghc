%
% (c) The AQUA Project, Glasgow University, 1996-1998
%
\section[RdrHsSyn]{Specialisations of the @HsSyn@ syntax for the reader}

(Well, really, for specialisations involving @RdrName@s, even if
they are used somewhat later on in the compiler...)

\begin{code}
module RdrHsSyn (
	RdrBinding(..),

	main_RDR_Unqual,

	extractHsTyRdrTyVars, 
	extractHsRhoRdrTyVars, extractGenericPatTyVars,
 
	mkHsOpApp, mkClassDecl, 
	mkHsNegApp, mkHsIntegral, mkHsFractional,
	mkHsDo, mkHsSplice, mkSigDecls,
        mkTyData, mkPrefixCon, mkRecCon,
	mkRecConstrOrUpdate, -- HsExp -> [HsFieldUpdate] -> P HsExp
	mkBootIface,

	cvBindGroup,
	cvBindsAndSigs,
	cvTopDecls,
	findSplice, mkGroup,

	-- Stuff to do with Foreign declarations
	, CallConv(..)
	, mkImport            -- CallConv -> Safety 
			      -- -> (FastString, RdrName, RdrNameHsType)
			      -- -> P RdrNameHsDecl
	, mkExport            -- CallConv
			      -- -> (FastString, RdrName, RdrNameHsType)
			      -- -> P RdrNameHsDecl
	, mkExtName           -- RdrName -> CLabelString
			      
	-- Bunch of functions in the parser monad for 
	-- checking and constructing values
	, checkPrecP 	      -- Int -> P Int
	, checkContext	      -- HsType -> P HsContext
	, checkPred	      -- HsType -> P HsPred
	, checkTyClHdr	      -- HsType -> (name,[tyvar])
	, checkInstType	      -- HsType -> P HsType
	, checkPattern	      -- HsExp -> P HsPat
	, checkPatterns	      -- SrcLoc -> [HsExp] -> P [HsPat]
	, checkDo	      -- [Stmt] -> P [Stmt]
	, checkMDo	      -- [Stmt] -> P [Stmt]
	, checkValDef	      -- (SrcLoc, HsExp, HsRhs, [HsDecl]) -> P HsDecl
	, checkValSig	      -- (SrcLoc, HsExp, HsRhs, [HsDecl]) -> P HsDecl
	, parseError	      -- String -> Pa
    ) where

#include "HsVersions.h"

import HsSyn		-- Lots of it
import IfaceType
import HscTypes		( ModIface(..), emptyModIface, mkIfaceVerCache )
import IfaceSyn		( IfaceDecl(..), IfaceIdInfo(..) )
import RdrName		( RdrName, isRdrTyVar, mkUnqual, rdrNameOcc, 
			  isRdrTyVar, isRdrDataCon, isUnqual, getRdrName, isQual,
			  setRdrNameSpace, rdrNameModule )
import BasicTypes	( RecFlag(..), mapIPName, maxPrecedence, initialVersion )
import Lexer		( P, failSpanMsgP )
import HscTypes		( GenAvailInfo(..) )
import TysWiredIn	( unitTyCon ) 
import ForeignCall	( CCallConv, Safety, CCallTarget(..), CExportSpec(..),
			  DNCallSpec(..), DNKind(..))
import OccName  	( OccName, srcDataName, varName, isDataOcc, isTcOcc, 
			  occNameUserString, isValOcc )
import BasicTypes	( initialVersion )
import TyCon		( DataConDetails(..) )
import Module		( ModuleName )
import SrcLoc
import CStrings		( CLabelString )
import CmdLineOpts	( opt_InPackage )
import Bag		( Bag, emptyBag, snocBag, consBag, foldrBag )
import Outputable
import FastString
import Panic

import List		( isSuffixOf, nubBy )
\end{code}

 
%************************************************************************
%*									*
\subsection{Type synonyms}
%*									*
%************************************************************************

\begin{code}
main_RDR_Unqual :: RdrName
main_RDR_Unqual = mkUnqual varName FSLIT("main")
	-- We definitely don't want an Orig RdrName, because
	-- main might, in principle, be imported into module Main
\end{code}

%************************************************************************
%*									*
\subsection{A few functions over HsSyn at RdrName}
%*                                                                    *
%************************************************************************

@extractHsTyRdrNames@ finds the free variables of a HsType
It's used when making the for-alls explicit.

\begin{code}
extractHsTyRdrTyVars :: LHsType RdrName -> [Located RdrName]
extractHsTyRdrTyVars ty = nubBy eqLocated (extract_lty ty [])

extractHsRhoRdrTyVars :: LHsContext RdrName -> LHsType RdrName -> [Located RdrName]
-- This one takes the context and tau-part of a 
-- sigma type and returns their free type variables
extractHsRhoRdrTyVars ctxt ty 
 = nubBy eqLocated $ extract_lctxt ctxt (extract_lty ty [])

extract_lctxt ctxt acc = foldr (extract_pred.unLoc) acc (unLoc ctxt)

extract_pred (HsClassP cls tys) acc	= foldr extract_lty acc tys
extract_pred (HsIParam n ty) acc	= extract_lty ty acc

extract_lty (L loc (HsTyVar tv)) acc
  | isRdrTyVar tv = L loc tv : acc
  | otherwise = acc
extract_lty ty acc = extract_ty (unLoc ty) acc

extract_ty (HsAppTy ty1 ty2)         acc = extract_lty ty1 (extract_lty ty2 acc)
extract_ty (HsListTy ty)             acc = extract_lty ty acc
extract_ty (HsPArrTy ty)             acc = extract_lty ty acc
extract_ty (HsTupleTy _ tys)         acc = foldr extract_lty acc tys
extract_ty (HsFunTy ty1 ty2)         acc = extract_lty ty1 (extract_lty ty2 acc)
extract_ty (HsPredTy p)		     acc = extract_pred (unLoc p) acc
extract_ty (HsOpTy ty1 nam ty2)      acc = extract_lty ty1 (extract_lty ty2 acc)
extract_ty (HsParTy ty)              acc = extract_lty ty acc
extract_ty (HsNumTy num)             acc = acc
extract_ty (HsKindSig ty k)	     acc = extract_lty ty acc
extract_ty (HsForAllTy exp [] cx ty) acc = extract_lctxt cx (extract_lty ty acc)
extract_ty (HsForAllTy exp tvs cx ty) 
                                acc = (filter ((`notElem` locals) . unLoc) $
				       extract_lctxt cx (extract_lty ty [])) ++ acc
				    where
				      locals = hsLTyVarNames tvs

extractGenericPatTyVars :: LHsBinds RdrName -> [Located RdrName]
-- Get the type variables out of the type patterns in a bunch of
-- possibly-generic bindings in a class declaration
extractGenericPatTyVars binds
  = nubBy eqLocated (foldrBag get [] binds)
  where
    get (L _ (FunBind _ _ ms)) acc = foldr (get_m.unLoc) acc ms
    get other	               acc = acc

    get_m (Match (L _ (TypePat ty) : _) _ _) acc = extract_lty ty acc
    get_m other			       		   acc = acc
\end{code}


%************************************************************************
%*									*
\subsection{Construction functions for Rdr stuff}
%*                                                                    *
%************************************************************************

mkClassDecl builds a RdrClassDecl, filling in the names for tycon and datacon
by deriving them from the name of the class.  We fill in the names for the
tycon and datacon corresponding to the class, by deriving them from the
name of the class itself.  This saves recording the names in the interface
file (which would be equally good).

Similarly for mkConDecl, mkClassOpSig and default-method names.

	*** See "THE NAMING STORY" in HsDecls ****
  
\begin{code}
mkClassDecl (cxt, cname, tyvars) fds sigs mbinds
  = ClassDecl { tcdCtxt = cxt, tcdLName = cname, tcdTyVars = tyvars,
		tcdFDs = fds,  
		tcdSigs = sigs,
		tcdMeths = mbinds,
		}

mkTyData new_or_data (context, tname, tyvars) data_cons maybe
  = TyData { tcdND = new_or_data, tcdCtxt = context, tcdLName = tname,
	     tcdTyVars = tyvars,  tcdCons = data_cons, 
	     tcdDerivs = maybe }
\end{code}

\begin{code}
mkHsNegApp :: LHsExpr RdrName -> HsExpr RdrName
-- RdrName If the type checker sees (negate 3#) it will barf, because negate
-- can't take an unboxed arg.  But that is exactly what it will see when
-- we write "-3#".  So we have to do the negation right now!
mkHsNegApp (L loc e) = f e
  where f (HsLit (HsIntPrim i))    = HsLit (HsIntPrim (-i))    
	f (HsLit (HsFloatPrim i))  = HsLit (HsFloatPrim (-i))  
	f (HsLit (HsDoublePrim i)) = HsLit (HsDoublePrim (-i)) 
	f expr	    		   = NegApp (L loc e) placeHolderName
\end{code}

%************************************************************************
%*									*
		Hi-boot files
%*									*
%************************************************************************

mkBootIface, and its boring helper functions, have two purposes:
a) HsSyn to IfaceSyn.  The parser parses the former, but we're reading
	an hi-boot file, and interfaces consist of the latter
b) Convert unqualifed names from the "current module" to qualified Orig
   names.  E.g.
	module This where
	 foo :: GHC.Base.Int -> GHC.Base.Int
   becomes
	 This.foo :: GHC.Base.Int -> GHC.Base.Int

It assumes that everything is well kinded, of course.

\begin{code}
mkBootIface :: ModuleName -> [HsDecl RdrName] -> ModIface
-- Make the ModIface for a hi-boot file
-- The decls are of very limited form
mkBootIface mod decls
  = (emptyModIface opt_InPackage mod) {
	mi_boot     = True,
	mi_exports  = [(mod, map mk_export decls')],
	mi_decls    = decls_w_vers,
	mi_ver_fn   = mkIfaceVerCache decls_w_vers }
  where
    decls' = map hsIfaceDecl decls
    decls_w_vers = repeat initialVersion `zip` decls'

		-- hi-boot declarations don't (currently)
		-- expose constructors or class methods
    mk_export decl | isValOcc occ = Avail occ
	           | otherwise    = AvailTC occ [occ]
		   where
		     occ = ifName decl


hsIfaceDecl :: HsDecl RdrName -> IfaceDecl
	-- Change to Iface syntax, and replace unqualified names with
	-- qualified Orig names from this module.  Reason: normal
	-- iface files have everything fully qualified, so it's convenient
	-- for hi-boot files to look the same
	--
	-- NB: no constructors or class ops to worry about
hsIfaceDecl (SigD (Sig name ty)) 
  = IfaceId { ifName = rdrNameOcc (unLoc name),
	      ifType = hsIfaceLType ty,
	      ifIdInfo = NoInfo }

hsIfaceDecl (TyClD decl@(TySynonym {}))
  = IfaceSyn { ifName = rdrNameOcc (tcdName decl), 
	       ifTyVars = hsIfaceTvs (tcdTyVars decl), 
	       ifSynRhs = hsIfaceLType (tcdSynRhs decl), 
	       ifVrcs = [] } 

hsIfaceDecl (TyClD decl@(TyData {}))
  = IfaceData { ifND = tcdND decl, 
	        ifName = rdrNameOcc (tcdName decl), 
	        ifTyVars = hsIfaceTvs (tcdTyVars decl), 
		ifCtxt = hsIfaceCtxt (unLoc (tcdCtxt decl)),
		ifCons = Unknown, ifRec = NonRecursive,
		ifVrcs = [], ifGeneric = False }
	-- I'm not sure that [] is right for ifVrcs, but
	-- since we don't use them I'm not going to fiddle

hsIfaceDecl (TyClD decl@(ClassDecl {}))
  = IfaceClass { ifName = rdrNameOcc (tcdName decl), 
	         ifTyVars = hsIfaceTvs (tcdTyVars decl), 
		 ifCtxt = hsIfaceCtxt (unLoc (tcdCtxt decl)),
		 ifFDs = hsIfaceFDs (map unLoc (tcdFDs decl)),
		 ifSigs = [], 	-- Is this right??
		 ifRec = NonRecursive, ifVrcs = [] }

hsIfaceDecl decl = pprPanic "hsIfaceDecl" (ppr decl)

hsIfaceName rdr_name	-- Qualify unqualifed occurrences
				-- with the module name
  | isUnqual rdr_name = LocalTop (rdrNameOcc rdr_name)
  | otherwise         = ExtPkg (rdrNameModule rdr_name) (rdrNameOcc rdr_name)

hsIfaceLType :: LHsType RdrName -> IfaceType
hsIfaceLType = hsIfaceType . unLoc

hsIfaceType :: HsType RdrName -> IfaceType	
hsIfaceType (HsForAllTy exp tvs cxt ty) 
  = foldr (IfaceForAllTy . hsIfaceTv) rho tvs'
  where
    rho = foldr (IfaceFunTy . IfacePredTy . hsIfaceLPred) tau (unLoc cxt)
    tau = hsIfaceLType ty
    tvs' = case exp of
	     Explicit -> map unLoc tvs
	     Implicit -> map (UserTyVar . unLoc) (extractHsRhoRdrTyVars cxt ty)

hsIfaceType ty@(HsTyVar _)     = hs_tc_app ty []
hsIfaceType ty@(HsAppTy t1 t2) = hs_tc_app ty []
hsIfaceType (HsFunTy t1 t2)    = IfaceFunTy (hsIfaceLType t1) (hsIfaceLType t2)
hsIfaceType (HsListTy t)       = IfaceTyConApp IfaceListTc [hsIfaceLType t]
hsIfaceType (HsPArrTy t)       = IfaceTyConApp IfacePArrTc [hsIfaceLType t]
hsIfaceType (HsTupleTy bx ts)  = IfaceTyConApp (IfaceTupTc bx (length ts)) (hsIfaceLTypes ts)
hsIfaceType (HsOpTy t1 tc t2)  = hs_tc_app (HsTyVar (unLoc tc)) (hsIfaceLTypes [t1, t2])
hsIfaceType (HsParTy t)	       = hsIfaceLType t
hsIfaceType (HsNumTy n)	       = panic "hsIfaceType:HsNum"
hsIfaceType (HsPredTy p)       = IfacePredTy (hsIfaceLPred p)
hsIfaceType (HsKindSig t _)    = hsIfaceLType t

-----------
hsIfaceLTypes tys = map (hsIfaceType.unLoc) tys

-----------
hsIfaceCtxt :: [LHsPred RdrName] -> [IfacePredType]
hsIfaceCtxt ctxt = map hsIfaceLPred ctxt

-----------
hsIfaceLPred :: LHsPred RdrName -> IfacePredType	
hsIfaceLPred = hsIfacePred . unLoc

hsIfacePred :: HsPred RdrName -> IfacePredType	
hsIfacePred (HsClassP cls ts) = IfaceClassP (hsIfaceName cls) (hsIfaceLTypes ts)
hsIfacePred (HsIParam ip t)   = IfaceIParam (mapIPName rdrNameOcc ip) (hsIfaceLType t)

-----------
hs_tc_app :: HsType RdrName -> [IfaceType] -> IfaceType
hs_tc_app (HsAppTy t1 t2) args = hs_tc_app (unLoc t1) (hsIfaceLType t2 : args)
hs_tc_app (HsTyVar n) args
  | isTcOcc (rdrNameOcc n) = IfaceTyConApp (IfaceTc (hsIfaceName n)) args
  | otherwise		   = foldl IfaceAppTy (IfaceTyVar (rdrNameOcc n)) args
hs_tc_app ty args 	   = foldl IfaceAppTy (hsIfaceType ty) args

-----------
hsIfaceTvs tvs = map (hsIfaceTv.unLoc) tvs

-----------
hsIfaceTv (UserTyVar n)     = (rdrNameOcc n, IfaceLiftedTypeKind)
hsIfaceTv (KindedTyVar n k) = (rdrNameOcc n, toIfaceKind k)

-----------
hsIfaceFDs :: [([RdrName], [RdrName])] -> [([OccName], [OccName])]
hsIfaceFDs fds = [ (map rdrNameOcc xs, map rdrNameOcc ys)
		 | (xs,ys) <- fds ]
\end{code}


%************************************************************************
%*									*
\subsection[rdrBinding]{Bindings straight out of the parser}
%*									*
%************************************************************************

\begin{code}
data RdrBinding
  =   -- Value bindings havn't been united with their
      -- signatures yet
    RdrBindings [RdrBinding]	-- Convenience for parsing

  | RdrValBinding     (LHsBind RdrName)

      -- The remainder all fit into the main HsDecl form
  | RdrHsDecl         (LHsDecl RdrName)
\end{code}

%************************************************************************
%*									*
\subsection[cvBinds-etc]{Converting to @HsBinds@, etc.}
%*									*
%************************************************************************

Function definitions are restructured here. Each is assumed to be recursive
initially, and non recursive definitions are discovered by the dependency
analyser.


\begin{code}
cvTopDecls :: [RdrBinding] -> [LHsDecl RdrName]
-- Incoming bindings are in reverse order; result is in ordinary order
-- (a) flatten RdrBindings
-- (b) Group together bindings for a single function
cvTopDecls decls
  = go [] decls
  where
    go :: [LHsDecl RdrName] -> [RdrBinding] -> [LHsDecl RdrName]
    go acc [] 			   = acc
    go acc (RdrBindings ds1 : ds2) = go (go acc ds1)    ds2
    go acc (RdrHsDecl d : ds)      = go (d       : acc) ds
    go acc (RdrValBinding b : ds)  = go (L l (ValD b') : acc) ds'
				   where
				     (L l b', ds') = getMonoBind b ds

cvBindGroup :: [RdrBinding] -> HsBindGroup RdrName
cvBindGroup binding
  = case (cvBindsAndSigs binding) of { (mbs, sigs) ->
    HsBindGroup mbs sigs Recursive -- just one big group for now
    }

cvBindsAndSigs :: [RdrBinding] -> (Bag (LHsBind RdrName), [LSig RdrName])
-- Input bindings are in *reverse* order, 
-- and contain just value bindings and signatures
cvBindsAndSigs  fb
  = go (emptyBag, []) fb
  where
    go acc	[] 		          = acc
    go acc 	(RdrBindings ds1 : ds2)   = go (go acc ds1) ds2
    go (bs, ss) (RdrHsDecl (L l (SigD s)) : ds) = go (bs, L l s : ss) ds
    go (bs, ss) (RdrValBinding b : ds)    = go (b' `consBag` bs, ss) ds'
					  where
					    (b',ds') = getMonoBind b ds

-----------------------------------------------------------------------------
-- Group function bindings into equation groups

getMonoBind :: LHsBind RdrName -> [RdrBinding] -> (LHsBind RdrName, [RdrBinding])
-- Suppose 	(b',ds') = getMonoBind b ds
-- 	ds is a *reversed* list of parsed bindings
--	b is a MonoBinds that has just been read off the front

-- Then b' is the result of grouping more equations from ds that
-- belong with b into a single MonoBinds, and ds' is the depleted
-- list of parsed bindings.
--
-- No AndMonoBinds or EmptyMonoBinds here; just single equations

getMonoBind (L loc (FunBind lf@(L _ f) inf mtchs)) binds
  | has_args mtchs
  = go mtchs loc binds
  where
    go mtchs1 loc1 (RdrValBinding (L loc2 (FunBind f2 inf2 mtchs2)) : binds)
	| f == unLoc f2 = go (mtchs2 ++ mtchs1) loc binds
	-- Remember binds is reversed, so glue mtchs2 on the front
	-- and use loc2 as the final location
	where loc = combineSrcSpans loc1 loc2
    go mtchs1 loc binds = (L loc (FunBind lf inf mtchs1), binds)

getMonoBind bind binds = (bind, binds)

has_args ((L _ (Match args _ _)) : _) = not (null args)
	-- Don't group together FunBinds if they have
	-- no arguments.  This is necessary now that variable bindings
	-- with no arguments are now treated as FunBinds rather
	-- than pattern bindings (tests/rename/should_fail/rnfail002).
\end{code}

\begin{code}
emptyGroup = HsGroup { hs_valds = [HsBindGroup emptyBag [] Recursive],
		       hs_tyclds = [], hs_instds = [],
		       hs_fixds = [], hs_defds = [], hs_fords = [], 
		       hs_depds = [] ,hs_ruleds = [] }

findSplice :: [LHsDecl a] -> (HsGroup a, Maybe (SpliceDecl a, [LHsDecl a]))
findSplice ds = addl emptyGroup ds

mkGroup :: [LHsDecl a] -> HsGroup a
mkGroup ds = addImpDecls emptyGroup ds

addImpDecls :: HsGroup a -> [LHsDecl a] -> HsGroup a
-- The decls are imported, and should not have a splice
addImpDecls group decls = case addl group decls of
				(group', Nothing) -> group'
				other		  -> panic "addImpDecls"

addl :: HsGroup a -> [LHsDecl a] -> (HsGroup a, Maybe (SpliceDecl a, [LHsDecl a]))
	-- This stuff reverses the declarations (again) but it doesn't matter

-- Base cases
addl gp []		   = (gp, Nothing)
addl gp (L l d : ds) = add gp l d ds


add :: HsGroup a -> SrcSpan -> HsDecl a -> [LHsDecl a]
  -> (HsGroup a, Maybe (SpliceDecl a, [LHsDecl a]))

add gp l (SpliceD e) ds = (gp, Just (e, ds))

-- Class declarations: pull out the fixity signatures to the top
add gp@(HsGroup {hs_tyclds = ts, hs_fixds = fs}) l (TyClD d) ds
	| isClassDecl d = 	
		let fsigs = [ L l f | L l (FixSig f) <- tcdSigs d ] in
		addl (gp { hs_tyclds = L l d : ts, hs_fixds  = fsigs ++ fs }) ds
	| otherwise =
		addl (gp { hs_tyclds = L l d : ts }) ds

-- Signatures: fixity sigs go a different place than all others
add gp@(HsGroup {hs_fixds = ts}) l (SigD (FixSig f)) ds
  = addl (gp {hs_fixds = L l f : ts}) ds
add gp@(HsGroup {hs_valds = ts}) l (SigD d) ds
  = addl (gp {hs_valds = add_sig (L l d) ts}) ds

-- Value declarations: use add_bind
add gp@(HsGroup {hs_valds  = ts}) l (ValD d) ds
  = addl (gp { hs_valds = add_bind (L l d) ts }) ds

-- The rest are routine
add gp@(HsGroup {hs_instds = ts})  l (InstD d) ds
  = addl (gp { hs_instds = L l d : ts }) ds
add gp@(HsGroup {hs_defds  = ts})  l (DefD d) ds
  = addl (gp { hs_defds = L l d : ts }) ds
add gp@(HsGroup {hs_fords  = ts})  l (ForD d) ds
  = addl (gp { hs_fords = L l d : ts }) ds
add gp@(HsGroup {hs_depds  = ts})  l (DeprecD d) ds
  = addl (gp { hs_depds = L l d : ts }) ds
add gp@(HsGroup {hs_ruleds  = ts}) l (RuleD d) ds
  = addl (gp { hs_ruleds = L l d : ts }) ds

add_bind b [HsBindGroup bs sigs r] = [HsBindGroup (bs `snocBag` b) sigs     r]
add_sig  s [HsBindGroup bs sigs r] = [HsBindGroup bs	           (s:sigs) r]
\end{code}

%************************************************************************
%*									*
\subsection[PrefixToHS-utils]{Utilities for conversion}
%*									*
%************************************************************************


\begin{code}
-----------------------------------------------------------------------------
-- mkPrefixCon

-- When parsing data declarations, we sometimes inadvertently parse
-- a constructor application as a type (eg. in data T a b = C a b `D` E a b)
-- This function splits up the type application, adds any pending
-- arguments, and converts the type constructor back into a data constructor.

mkPrefixCon :: LHsType RdrName -> [LBangType RdrName]
  -> P (Located RdrName, HsConDetails RdrName (LBangType RdrName))
mkPrefixCon ty tys
 = split ty tys
 where
   split (L _ (HsAppTy t u)) ts = split t (unbangedType u : ts)
   split (L l (HsTyVar tc))  ts = do data_con <- tyConToDataCon l tc
				     return (data_con, PrefixCon ts)
   split (L l _) _ 		= parseError l "parse error in data/newtype declaration"

mkRecCon :: Located RdrName -> [([Located RdrName], LBangType RdrName)]
  -> P (Located RdrName, HsConDetails RdrName (LBangType RdrName))
mkRecCon (L loc con) fields
  = do data_con <- tyConToDataCon loc con
       return (data_con, RecCon [ (l,t) | (ls,t) <- fields, l <- ls ])

tyConToDataCon :: SrcSpan -> RdrName -> P (Located RdrName)
tyConToDataCon loc tc
  | isTcOcc (rdrNameOcc tc)
  = return (L loc (setRdrNameSpace tc srcDataName))
  | otherwise
  = parseError loc (showSDoc (text "Not a constructor:" <+> quotes (ppr tc)))

----------------------------------------------------------------------------
-- Various Syntactic Checks

checkInstType :: LHsType RdrName -> P (LHsType RdrName)
checkInstType (L l t)
  = case t of
	HsForAllTy exp tvs ctxt ty -> do
		dict_ty <- checkDictTy ty
	      	return (L l (HsForAllTy exp tvs ctxt dict_ty))

        HsParTy ty -> checkInstType ty

	ty ->   do dict_ty <- checkDictTy (L l ty)
	      	   return (L l (HsForAllTy Implicit [] (noLoc []) dict_ty))

checkTyVars :: [LHsType RdrName] -> P [LHsTyVarBndr RdrName]
checkTyVars tvs 
  = mapM chk tvs
  where
	--  Check that the name space is correct!
    chk (L l (HsKindSig (L _ (HsTyVar tv)) k))
	| isRdrTyVar tv = return (L l (KindedTyVar tv k))
    chk (L l (HsTyVar tv))
        | isRdrTyVar tv = return (L l (UserTyVar tv))
    chk (L l other)
	= parseError l "Type found where type variable expected"

checkTyClHdr :: LHsContext RdrName -> LHsType RdrName
  -> P (LHsContext RdrName, Located RdrName, [LHsTyVarBndr RdrName])
-- The header of a type or class decl should look like
--	(C a, D b) => T a b
-- or	T a b
-- or	a + b
-- etc
checkTyClHdr (L l cxt) ty
  = do (tc, tvs) <- gol ty []
       mapM_ chk_pred cxt
       return (L l cxt, tc, tvs)
  where
    gol (L l ty) acc = go l ty acc

    go l (HsTyVar tc)    acc 
	| not (isRdrTyVar tc)   = checkTyVars acc		>>= \ tvs ->
				  return (L l tc, tvs)
    go l (HsOpTy t1 tc t2) acc  = checkTyVars (t1:t2:acc)	>>= \ tvs ->
				  return (tc, tvs)
    go l (HsParTy ty)    acc    = gol ty acc
    go l (HsAppTy t1 t2) acc    = gol t1 (t2:acc)
    go l other	         acc    = parseError l "Malformed LHS to type of class declaration"

	-- The predicates in a type or class decl must all
	-- be HsClassPs.  They need not all be type variables,
	-- even in Haskell 98.  E.g. class (Monad m, Monad (t m)) => MonadT t m
    chk_pred (L l (HsClassP _ args)) = return ()
    chk_pred (L l _)
       = parseError l "Malformed context in type or class declaration"

  
checkContext :: LHsType RdrName -> P (LHsContext RdrName)
checkContext (L l t)
  = check t
 where
  check (HsTupleTy _ ts) 	-- (Eq a, Ord b) shows up as a tuple type
    = do ctx <- mapM checkPred ts
	 return (L l ctx)

  check (HsParTy ty)	-- to be sure HsParTy doesn't get into the way
    = check (unLoc ty)

  check (HsTyVar t)	-- Empty context shows up as a unit type ()
    | t == getRdrName unitTyCon = return (L l [])

  check t 
    = do p <- checkPred (L l t)
         return (L l [p])


checkPred :: LHsType RdrName -> P (LHsPred RdrName)
-- Watch out.. in ...deriving( Show )... we use checkPred on 
-- the list of partially applied predicates in the deriving,
-- so there can be zero args.
checkPred (L spn (HsPredTy (L _ (HsIParam n ty))) )
  = return (L spn (HsIParam n ty))
checkPred (L spn ty)
  = check spn ty []
  where
    checkl (L l ty) args = check l ty args

    check loc (HsTyVar t)   args | not (isRdrTyVar t) 
		  	     = return (L spn (HsClassP t args))
    check loc (HsAppTy l r) args = checkl l (r:args)
    check loc (HsParTy t)   args = checkl t args
    check loc _             _    = parseError loc  "malformed class assertion"

checkDictTy :: LHsType RdrName -> P (LHsType RdrName)
checkDictTy (L spn ty) = check ty []
  where
  check (HsTyVar t) args@(_:_) | not (isRdrTyVar t) 
  	= return (L spn (HsPredTy (L spn (HsClassP t args))))
  check (HsAppTy l r) args = check (unLoc l) (r:args)
  check (HsParTy t)   args = check (unLoc t) args
  check _ _ = parseError spn "Malformed context in instance header"

---------------------------------------------------------------------------
-- Checking statements in a do-expression
-- 	We parse   do { e1 ; e2 ; }
-- 	as [ExprStmt e1, ExprStmt e2]
-- checkDo (a) checks that the last thing is an ExprStmt
--	   (b) transforms it to a ResultStmt
-- same comments apply for mdo as well

checkDo	 = checkDoMDo "a " "'do'"
checkMDo = checkDoMDo "an " "'mdo'"

checkDoMDo :: String -> String -> SrcSpan -> [LStmt RdrName] -> P [LStmt RdrName]
checkDoMDo pre nm loc []   = parseError loc ("Empty " ++ nm ++ " construct")
checkDoMDo pre nm loc ss   = do 
  check ss
  where 
	check  [L l (ExprStmt e _)] = return [L l (ResultStmt e)]
	check  [L l _] = parseError l ("The last statement in " ++ pre ++ nm ++
					 " construct must be an expression")
	check (s:ss) = do
	  ss' <-  check ss
	  return (s:ss')

-- -------------------------------------------------------------------------
-- Checking Patterns.

-- We parse patterns as expressions and check for valid patterns below,
-- converting the expression into a pattern at the same time.

checkPattern :: LHsExpr RdrName -> P (LPat RdrName)
checkPattern e = checkLPat e

checkPatterns :: [LHsExpr RdrName] -> P [LPat RdrName]
checkPatterns es = mapM checkPattern es

checkLPat :: LHsExpr RdrName -> P (LPat RdrName)
checkLPat e@(L l _) = checkPat l e []

checkPat :: SrcSpan -> LHsExpr RdrName -> [LPat RdrName] -> P (LPat RdrName)
checkPat loc (L l (HsVar c)) args
  | isRdrDataCon c = return (L loc (ConPatIn (L l c) (PrefixCon args)))
checkPat loc (L _ (HsApp f x)) args = do
  x <- checkLPat x
  checkPat loc f (x:args)
checkPat loc (L _ e) [] = do
  p <- checkAPat loc e
  return (L loc p)
checkPat loc pat _some_args
  = patFail loc

checkAPat loc e = case e of
   EWildPat	       -> return (WildPat placeHolderType)
   HsVar x | isQual x  -> parseError loc ("Qualified variable in pattern: "
					 ++ showRdrName x)
   	   | otherwise -> return (VarPat x)
   HsLit l 	       -> return (LitPat l)

   -- Overloaded numeric patterns (e.g. f 0 x = x)
   -- Negation is recorded separately, so that the literal is zero or +ve
   -- NB. Negative *primitive* literals are already handled by
   --     RdrHsSyn.mkHsNegApp
   HsOverLit pos_lit            -> return (NPatIn pos_lit Nothing)
   NegApp (L _ (HsOverLit pos_lit)) _ 
			-> return (NPatIn pos_lit (Just placeHolderName))
   
   ELazyPat e	   -> checkLPat e >>= (return . LazyPat)
   EAsPat n e	   -> checkLPat e >>= (return . AsPat n)
   ExprWithTySig e t  -> checkLPat e >>= \e ->
   			 -- Pattern signatures are parsed as sigtypes,
   			 -- but they aren't explicit forall points.  Hence
   			 -- we have to remove the implicit forall here.
   			 let t' = case t of 
   				     L _ (HsForAllTy Implicit _ (L _ []) ty) -> ty
   				     other -> other
   			 in
   			 return (SigPatIn e t')
   
   -- n+k patterns
   OpApp (L nloc (HsVar n)) (L _ (HsVar plus)) _ 
	(L _ (HsOverLit lit@(HsIntegral _ _)))
   		      | plus == plus_RDR
   		      -> return (mkNPlusKPat (L nloc n) lit)
   		      where
   			 plus_RDR = mkUnqual varName FSLIT("+")	-- Hack
   
   OpApp l op fix r   -> checkLPat l >>= \l ->
   			 checkLPat r >>= \r ->
   			 case op of
   			    L cl (HsVar c) | isDataOcc (rdrNameOcc c)
   				   -> return (ConPatIn (L cl c) (InfixCon l r))
   			    _ -> patFail loc
   
   HsPar e		   -> checkLPat e >>= (return . ParPat)
   ExplicitList _ es  -> mapM (\e -> checkLPat e) es >>= \ps ->
   			 return (ListPat ps placeHolderType)
   ExplicitPArr _ es  -> mapM (\e -> checkLPat e) es >>= \ps ->
   			 return (PArrPat ps placeHolderType)
   
   ExplicitTuple es b -> mapM (\e -> checkLPat e) es >>= \ps ->
   			 return (TuplePat ps b)
   
   RecordCon c fs     -> mapM checkPatField fs >>= \fs ->
			 return (ConPatIn c (RecCon fs))
-- Generics 
   HsType ty          -> return (TypePat ty) 
   _                  -> patFail loc

checkAPat loc _ = patFail loc

checkPatField :: (Located RdrName, LHsExpr RdrName) -> P (Located RdrName, LPat RdrName)
checkPatField (n,e) = do
  p <- checkLPat e
  return (n,p)

patFail loc = parseError loc "Parse error in pattern"


---------------------------------------------------------------------------
-- Check Equation Syntax

checkValDef 
	:: LHsExpr RdrName
	-> Maybe (LHsType RdrName)
	-> GRHSs RdrName
	-> P (HsBind RdrName)

checkValDef lhs opt_sig grhss
  | Just (f,inf,es)  <- isFunLhs lhs []
  = if isQual (unLoc f)
	then parseError (getLoc f) ("Qualified name in function definition: "  ++ 
					showRdrName (unLoc f))
	else do ps <- checkPatterns es
		return (FunBind f inf [L (getLoc f) (Match ps opt_sig grhss)])
			-- TODO: span is wrong
  | otherwise = do
	lhs <- checkPattern lhs
	return (PatBind lhs grhss)

checkValSig
	:: LHsExpr RdrName
	-> LHsType RdrName
	-> P (Sig RdrName)
checkValSig (L l (HsVar v)) ty | isUnqual v = return (Sig (L l v) ty)
checkValSig (L l other)     ty
  = parseError l "Type signature given for an expression"

mkSigDecls :: [LSig RdrName] -> RdrBinding
mkSigDecls sigs = RdrBindings [RdrHsDecl (L l (SigD sig)) | L l sig <- sigs]


-- A variable binding is parsed as a FunBind.

isFunLhs :: LHsExpr RdrName -> [LHsExpr RdrName]
  -> Maybe (Located RdrName, Bool, [LHsExpr RdrName])
isFunLhs (L loc e) = isFunLhs' loc e
 where
   isFunLhs' loc (HsVar f) es 
	| not (isRdrDataCon f)	 	= Just (L loc f, False, es)
   isFunLhs' loc (HsApp f e) es 	= isFunLhs f (e:es)
   isFunLhs' loc (HsPar e)   es@(_:_) 	= isFunLhs e es
   isFunLhs' loc (OpApp l (L loc' (HsVar op)) fix r) es
	| not (isRdrDataCon op) = Just (L loc' op, True, (l:r:es))
	| otherwise		= 
		case isFunLhs l es of
		    Just (op', True, j : k : es') ->
		      Just (op', True, 
			    j : L loc (OpApp k (L loc' (HsVar op)) fix r) : es')
		    _ -> Nothing
   isFunLhs' _ _ _ = Nothing

---------------------------------------------------------------------------
-- Miscellaneous utilities

checkPrecP :: Located Int -> P Int
checkPrecP (L l i)
 | 0 <= i && i <= maxPrecedence = return i
 | otherwise     	        = parseError l "Precedence out of range"

mkRecConstrOrUpdate 
	:: LHsExpr RdrName 
	-> SrcSpan
	-> HsRecordBinds RdrName
	-> P (HsExpr RdrName)

mkRecConstrOrUpdate (L l (HsVar c)) loc fs | isRdrDataCon c
  = return (RecordCon (L l c) fs)
mkRecConstrOrUpdate exp loc fs@(_:_)
  = return (RecordUpd exp fs)
mkRecConstrOrUpdate _ loc []
  = parseError loc "Empty record update"

-----------------------------------------------------------------------------
-- utilities for foreign declarations

-- supported calling conventions
--
data CallConv = CCall  CCallConv	-- ccall or stdcall
	      | DNCall			-- .NET

-- construct a foreign import declaration
--
mkImport :: CallConv 
	 -> Safety 
	 -> (Located FastString, Located RdrName, LHsType RdrName) 
	 -> P (HsDecl RdrName)
mkImport (CCall  cconv) safety (entity, v, ty) = do
  importSpec <- parseCImport entity cconv safety v
  return (ForD (ForeignImport v ty importSpec False))
mkImport (DNCall      ) _      (entity, v, ty) = do
  spec <- parseDImport entity
  return $ ForD (ForeignImport v ty (DNImport spec) False)

-- parse the entity string of a foreign import declaration for the `ccall' or
-- `stdcall' calling convention'
--
parseCImport :: Located FastString
	     -> CCallConv 
	     -> Safety 
	     -> Located RdrName
	     -> P ForeignImport
parseCImport (L loc entity) cconv safety v
  -- FIXME: we should allow white space around `dynamic' and `wrapper' -=chak
  | entity == FSLIT ("dynamic") = 
    return $ CImport cconv safety nilFS nilFS (CFunction DynamicTarget)
  | entity == FSLIT ("wrapper") =
    return $ CImport cconv safety nilFS nilFS CWrapper
  | otherwise		       = parse0 (unpackFS entity)
    where
      -- using the static keyword?
      parse0 (' ':                    rest) = parse0 rest
      parse0 ('s':'t':'a':'t':'i':'c':rest) = parse1 rest
      parse0                          rest  = parse1 rest
      -- check for header file name
      parse1     ""               = parse4 ""    nilFS        False nilFS
      parse1     (' ':rest)       = parse1 rest
      parse1 str@('&':_   )       = parse2 str   nilFS
      parse1 str@('[':_   )       = parse3 str   nilFS        False
      parse1 str
	| ".h" `isSuffixOf` first = parse2 rest  (mkFastString first)
        | otherwise               = parse4 str   nilFS        False nilFS
        where
	  (first, rest) = break (\c -> c == ' ' || c == '&' || c == '[') str
      -- check for address operator (indicating a label import)
      parse2     ""         header = parse4 ""   header False nilFS
      parse2     (' ':rest) header = parse2 rest header
      parse2     ('&':rest) header = parse3 rest header True
      parse2 str@('[':_   ) header = parse3 str	 header False
      parse2 str	    header = parse4 str	 header False nilFS
      -- check for library object name
      parse3 (' ':rest) header isLbl = parse3 rest header isLbl
      parse3 ('[':rest) header isLbl = 
        case break (== ']') rest of 
	  (lib, ']':rest)           -> parse4 rest header isLbl (mkFastString lib)
	  _			    -> parseError loc "Missing ']' in entity"
      parse3 str	header isLbl = parse4 str  header isLbl nilFS
      -- check for name of C function
      parse4 ""         header isLbl lib = build (mkExtName (unLoc v)) header isLbl lib
      parse4 (' ':rest) header isLbl lib = parse4 rest         	       header isLbl lib
      parse4 str	header isLbl lib
        | all (== ' ') rest              = build (mkFastString first)  header isLbl lib
	| otherwise			 = parseError loc "Malformed entity string"
        where
	  (first, rest) = break (== ' ') str
      --
      build cid header False lib = return $
        CImport cconv safety header lib (CFunction (StaticTarget cid))
      build cid header True  lib = return $
        CImport cconv safety header lib (CLabel                  cid )

--
-- Unravel a dotnet spec string.
--
parseDImport :: Located FastString -> P DNCallSpec
parseDImport (L loc entity) = parse0 comps
 where
  comps = words (unpackFS entity)

  parse0 [] = d'oh
  parse0 (x : xs) 
    | x == "static" = parse1 True xs
    | otherwise     = parse1 False (x:xs)

  parse1 _ [] = d'oh
  parse1 isStatic (x:xs)
    | x == "method" = parse2 isStatic DNMethod xs
    | x == "field"  = parse2 isStatic DNField xs
    | x == "ctor"   = parse2 isStatic DNConstructor xs
  parse1 isStatic xs = parse2 isStatic DNMethod xs

  parse2 _ _ [] = d'oh
  parse2 isStatic kind (('[':x):xs) =
     case x of
	[] -> d'oh
	vs | last vs == ']' -> parse3 isStatic kind (init vs) xs
  parse2 isStatic kind xs = parse3 isStatic kind "" xs

  parse3 isStatic kind assem [x] = 
    return (DNCallSpec isStatic kind assem x 
    			  -- these will be filled in once known.
                        (error "FFI-dotnet-args")
                        (error "FFI-dotnet-result"))
  parse3 _ _ _ _ = d'oh

  d'oh = parseError loc "Malformed entity string"
  
-- construct a foreign export declaration
--
mkExport :: CallConv
         -> (Located FastString, Located RdrName, LHsType RdrName) 
	 -> P (HsDecl RdrName)
mkExport (CCall  cconv) (L loc entity, v, ty) = return $ 
  ForD (ForeignExport v ty (CExport (CExportStatic entity' cconv)) False)
  where
    entity' | nullFastString entity = mkExtName (unLoc v)
	    | otherwise		    = entity
mkExport DNCall (L loc entity, v, ty) =
  parseError (getLoc v){-TODO: not quite right-}
	"Foreign export is not yet supported for .NET"

-- Supplying the ext_name in a foreign decl is optional; if it
-- isn't there, the Haskell name is assumed. Note that no transformation
-- of the Haskell name is then performed, so if you foreign export (++),
-- it's external name will be "++". Too bad; it's important because we don't
-- want z-encoding (e.g. names with z's in them shouldn't be doubled)
-- (This is why we use occNameUserString.)
--
mkExtName :: RdrName -> CLabelString
mkExtName rdrNm = mkFastString (occNameUserString (rdrNameOcc rdrNm))
\end{code}


-----------------------------------------------------------------------------
-- Misc utils

\begin{code}
showRdrName :: RdrName -> String
showRdrName r = showSDoc (ppr r)

parseError :: SrcSpan -> String -> P a
parseError span s = failSpanMsgP span s
\end{code}
