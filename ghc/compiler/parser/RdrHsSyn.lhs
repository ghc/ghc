%
% (c) The AQUA Project, Glasgow University, 1996-1998
%
\section[RdrHsSyn]{Specialisations of the @HsSyn@ syntax for the reader}

(Well, really, for specialisations involving @RdrName@s, even if
they are used somewhat later on in the compiler...)

\begin{code}
module RdrHsSyn (
	RdrNameArithSeqInfo,
	RdrNameBangType,
	RdrNameClassOpSig,
	RdrNameConDecl,
	RdrNameConDetails,
	RdrNameContext,
	RdrNameDefaultDecl,
	RdrNameForeignDecl,
	RdrNameCoreDecl,
	RdrNameGRHS,
	RdrNameGRHSs,
	RdrNameHsBinds,
	RdrNameHsCmd,
	RdrNameHsCmdTop,
	RdrNameHsDecl,
	RdrNameHsExpr,
	RdrNameHsModule,
	RdrNameIE,
	RdrNameImportDecl,
	RdrNameInstDecl,
	RdrNameMatch,
	RdrNameMonoBinds,
	RdrNamePat,
	RdrNameHsType,
	RdrNameHsTyVar,
	RdrNameSig,
	RdrNameStmt,
	RdrNameTyClDecl,
	RdrNameRuleDecl,
	RdrNameRuleBndr,
	RdrNameDeprecation,
	RdrNameHsRecordBinds,
	RdrNameFixitySig,

	RdrBinding(..),
	RdrMatch(..),

	main_RDR_Unqual,

	extractHsTyRdrNames,  extractHsTyRdrTyVars, 
	extractHsCtxtRdrTyVars, extractGenericPatTyVars,
 
	mkHsOpApp, mkClassDecl, 
	mkHsNegApp, mkNPlusKPat, mkHsIntegral, mkHsFractional,
	mkHsDo, mkHsSplice, mkSigDecls,
        mkTyData, mkPrefixCon, mkRecCon,
	mkRecConstrOrUpdate, -- HsExp -> [HsFieldUpdate] -> P HsExp
	mkIfaceExports,      -- :: [RdrNameTyClDecl] -> [RdrExportItem]

	cvBinds,
	cvMonoBindsAndSigs,
	cvTopDecls,
	findSplice, addImpDecls, emptyGroup, mkGroup,

	-- Stuff to do with Foreign declarations
	, CallConv(..)
	, mkImport            -- CallConv -> Safety 
			      -- -> (FastString, RdrName, RdrNameHsType)
			      -- -> SrcLoc 
			      -- -> P RdrNameHsDecl
	, mkExport            -- CallConv
			      -- -> (FastString, RdrName, RdrNameHsType)
			      -- -> SrcLoc 
			      -- -> P RdrNameHsDecl
	, mkExtName           -- RdrName -> CLabelString
			      
	-- Bunch of functions in the parser monad for 
	-- checking and constructing values
	, checkPrecP 	      -- Int -> P Int
	, checkContext	      -- HsType -> P HsContext
	, checkPred	      -- HsType -> P HsPred
	, checkTyVars	      -- [HsTyVar] -> P [HsType]
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
import RdrName		( RdrName, isRdrTyVar, mkRdrUnqual, mkUnqual, rdrNameOcc, 
			  isRdrTyVar, isRdrDataCon, isUnqual, getRdrName, isQual,
			  setRdrNameSpace )
import BasicTypes	( RecFlag(..), FixitySig(..), maxPrecedence )
import Class            ( DefMeth (..) )
import Lexer		( P, setSrcLocFor, getSrcLoc, failLocMsgP )
import HscTypes		( RdrAvailInfo, GenAvailInfo(..) )
import TysWiredIn	( unitTyCon )
import ForeignCall	( CCallConv, Safety, CCallTarget(..), CExportSpec(..),
			  DNCallSpec(..), DNKind(..))
import OccName  	( srcDataName, varName, isDataOcc, isTcOcc, occNameUserString,
			  mkDefaultMethodOcc, mkVarOcc )
import SrcLoc
import CStrings		( CLabelString )
import List		( isSuffixOf, nub )
import Outputable
import FastString
import Panic
\end{code}

 
%************************************************************************
%*									*
\subsection{Type synonyms}
%*									*
%************************************************************************

\begin{code}
type RdrNameArithSeqInfo	= ArithSeqInfo		RdrName
type RdrNameBangType		= BangType		RdrName
type RdrNameClassOpSig		= Sig			RdrName
type RdrNameConDecl		= ConDecl		RdrName
type RdrNameConDetails		= HsConDetails		RdrName RdrNameBangType
type RdrNameContext		= HsContext 		RdrName
type RdrNameHsDecl		= HsDecl		RdrName
type RdrNameDefaultDecl		= DefaultDecl		RdrName
type RdrNameForeignDecl		= ForeignDecl		RdrName
type RdrNameCoreDecl		= CoreDecl		RdrName
type RdrNameGRHS		= GRHS			RdrName
type RdrNameGRHSs		= GRHSs			RdrName
type RdrNameHsBinds		= HsBinds		RdrName
type RdrNameHsExpr		= HsExpr		RdrName
type RdrNameHsCmd		= HsCmd			RdrName
type RdrNameHsCmdTop		= HsCmdTop		RdrName
type RdrNameHsModule		= HsModule		RdrName
type RdrNameIE			= IE			RdrName
type RdrNameImportDecl 		= ImportDecl		RdrName
type RdrNameInstDecl		= InstDecl		RdrName
type RdrNameMatch		= Match			RdrName
type RdrNameMonoBinds		= MonoBinds		RdrName
type RdrNamePat			= InPat			RdrName
type RdrNameHsType		= HsType		RdrName
type RdrNameHsTyVar		= HsTyVarBndr		RdrName
type RdrNameSig			= Sig			RdrName
type RdrNameStmt		= Stmt			RdrName
type RdrNameTyClDecl		= TyClDecl		RdrName

type RdrNameRuleBndr            = RuleBndr              RdrName
type RdrNameRuleDecl            = RuleDecl              RdrName
type RdrNameDeprecation         = DeprecDecl            RdrName
type RdrNameFixitySig		= FixitySig		RdrName

type RdrNameHsRecordBinds	= HsRecordBinds		RdrName
\end{code}

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
extractHsTyRdrNames :: RdrNameHsType -> [RdrName]
extractHsTyRdrNames ty = nub (extract_ty ty [])

extractHsTyRdrTyVars :: RdrNameHsType -> [RdrName]
extractHsTyRdrTyVars ty = nub (filter isRdrTyVar (extract_ty ty []))

extractHsCtxtRdrNames :: HsContext RdrName -> [RdrName]
extractHsCtxtRdrNames ty = nub (extract_ctxt ty [])
extractHsCtxtRdrTyVars :: HsContext RdrName -> [RdrName]
extractHsCtxtRdrTyVars ty = filter isRdrTyVar (extractHsCtxtRdrNames ty)

extract_ctxt ctxt acc = foldr extract_pred acc ctxt

extract_pred (HsClassP cls tys) acc	= foldr extract_ty (cls : acc) tys
extract_pred (HsIParam n ty) acc	= extract_ty ty acc

extract_tys tys = foldr extract_ty [] tys

extract_ty (HsAppTy ty1 ty2)          acc = extract_ty ty1 (extract_ty ty2 acc)
extract_ty (HsListTy ty)              acc = extract_ty ty acc
extract_ty (HsPArrTy ty)              acc = extract_ty ty acc
extract_ty (HsTupleTy _ tys)          acc = foldr extract_ty acc tys
extract_ty (HsFunTy ty1 ty2)          acc = extract_ty ty1 (extract_ty ty2 acc)
extract_ty (HsPredTy p)		      acc = extract_pred p acc
extract_ty (HsTyVar tv)               acc = tv : acc
extract_ty (HsForAllTy Nothing cx ty) acc = extract_ctxt cx (extract_ty ty acc)
extract_ty (HsOpTy ty1 nam ty2)       acc = extract_ty ty1 (extract_ty ty2 acc)
extract_ty (HsParTy ty)               acc = extract_ty ty acc
-- Generics
extract_ty (HsNumTy num)              acc = acc
extract_ty (HsKindSig ty k)	      acc = extract_ty ty acc
extract_ty (HsForAllTy (Just tvs) ctxt ty) 
                                acc = acc ++
                                      (filter (`notElem` locals) $
				       extract_ctxt ctxt (extract_ty ty []))
				    where
				      locals = hsTyVarNames tvs

extractGenericPatTyVars :: RdrNameMonoBinds -> [RdrName]
-- Get the type variables out of the type patterns in a bunch of
-- possibly-generic bindings in a class declaration
extractGenericPatTyVars binds
  = filter isRdrTyVar (nub (get binds []))
  where
    get (AndMonoBinds b1 b2)   acc = get b1 (get b2 acc)
    get (FunMonoBind _ _ ms _) acc = foldr get_m acc ms
    get other		       acc = acc

    get_m (Match (TypePat ty : _) _ _) acc = extract_ty ty acc
    get_m other			       acc = acc
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
mkClassDecl (cxt, cname, tyvars) fds sigs mbinds loc
  = ClassDecl { tcdCtxt = cxt, tcdName = cname, tcdTyVars = tyvars,
		tcdFDs = fds,  
		tcdSigs = map cvClassOpSig sigs,  	-- Convert to class-op sigs
		tcdMeths = mbinds,
		tcdLoc = loc }

mkTyData new_or_data (context, tname, tyvars) data_cons maybe src
  = TyData { tcdND = new_or_data, tcdCtxt = context, tcdName = tname,
	     tcdTyVars = tyvars,  tcdCons = data_cons, 
	     tcdDerivs = maybe,   tcdLoc = src, tcdGeneric = Nothing }

cvClassOpSig :: RdrNameSig -> RdrNameSig
cvClassOpSig (Sig var poly_ty src_loc) 
  = ClassOpSig var (DefMeth dm_rn) poly_ty src_loc
  where
    dm_rn = mkRdrUnqual (mkDefaultMethodOcc (rdrNameOcc var))
cvClassOpSig sig 
  = sig
\end{code}

\begin{code}
mkHsNegApp :: RdrNameHsExpr -> RdrNameHsExpr
-- If the type checker sees (negate 3#) it will barf, because negate
-- can't take an unboxed arg.  But that is exactly what it will see when
-- we write "-3#".  So we have to do the negation right now!

mkHsNegApp (HsLit (HsIntPrim i))    = HsLit (HsIntPrim (-i))    
mkHsNegApp (HsLit (HsFloatPrim i))  = HsLit (HsFloatPrim (-i))  
mkHsNegApp (HsLit (HsDoublePrim i)) = HsLit (HsDoublePrim (-i)) 
mkHsNegApp expr	    		    = NegApp expr     placeHolderName
\end{code}

A useful function for building @OpApps@.  The operator is always a
variable, and we don't know the fixity yet.

\begin{code}
mkHsOpApp e1 op e2 = OpApp e1 (HsVar op) (error "mkOpApp:fixity") e2
\end{code}

These are the bits of syntax that contain rebindable names
See RnEnv.lookupSyntaxName

\begin{code}
mkHsIntegral   i      = HsIntegral   i  placeHolderName
mkHsFractional f      = HsFractional f  placeHolderName
mkNPlusKPat n k       = NPlusKPatIn n k placeHolderName
mkHsDo ctxt stmts loc = HsDo ctxt stmts [] placeHolderType loc
\end{code}

\begin{code}
mkHsSplice e loc = HsSplice unqualSplice e loc

unqualSplice = mkRdrUnqual (mkVarOcc FSLIT("splice"))
		-- A name (uniquified later) to
		-- identify the splice
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

  | RdrValBinding     RdrNameMonoBinds

      -- The remainder all fit into the main HsDecl form
  | RdrHsDecl         RdrNameHsDecl
\end{code}

\begin{code}
data RdrMatch
  = RdrMatch
	     [RdrNamePat]
	     (Maybe RdrNameHsType)
	     RdrNameGRHSs
\end{code}

%************************************************************************
%*									*
\subsection[cvBinds-etc]{Converting to @HsBinds@, @MonoBinds@, etc.}
%*									*
%************************************************************************

Function definitions are restructured here. Each is assumed to be recursive
initially, and non recursive definitions are discovered by the dependency
analyser.


\begin{code}
cvTopDecls :: [RdrBinding] -> [RdrNameHsDecl]
-- Incoming bindings are in reverse order; result is in ordinary order
-- (a) flatten RdrBindings
-- (b) Group together bindings for a single function
cvTopDecls decls
  = go [] decls
  where
    go :: [RdrNameHsDecl] -> [RdrBinding] -> [RdrNameHsDecl]
    go acc [] 			   = acc
    go acc (RdrBindings ds1 : ds2) = go (go acc ds1)    ds2
    go acc (RdrHsDecl d : ds)      = go (d       : acc) ds
    go acc (RdrValBinding b : ds)  = go (ValD b' : acc) ds'
				   where
				     (b', ds') = getMonoBind b ds

cvBinds :: [RdrBinding] -> RdrNameHsBinds
cvBinds binding
  = case (cvMonoBindsAndSigs binding) of { (mbs, sigs) ->
    MonoBind mbs sigs Recursive
    }

cvMonoBindsAndSigs :: [RdrBinding] -> (RdrNameMonoBinds, [RdrNameSig])
-- Input bindings are in *reverse* order, 
-- and contain just value bindings and signatuers

cvMonoBindsAndSigs  fb
  = go (EmptyMonoBinds, []) fb
  where
    go acc	[] 		          = acc
    go acc 	(RdrBindings ds1 : ds2)   = go (go acc ds1) ds2
    go (bs, ss) (RdrHsDecl (SigD s) : ds) = go (bs, s : ss) ds
    go (bs, ss) (RdrValBinding b : ds)    = go (b' `AndMonoBinds` bs, ss) ds'
					  where
					    (b',ds') = getMonoBind b ds

-----------------------------------------------------------------------------
-- Group function bindings into equation groups

getMonoBind :: RdrNameMonoBinds -> [RdrBinding] -> (RdrNameMonoBinds, [RdrBinding])
-- Suppose 	(b',ds') = getMonoBind b ds
-- 	ds is a *reversed* list of parsed bindings
--	b is a MonoBinds that has just been read off the front

-- Then b' is the result of grouping more equations from ds that
-- belong with b into a single MonoBinds, and ds' is the depleted
-- list of parsed bindings.
--
-- No AndMonoBinds or EmptyMonoBinds here; just single equations

getMonoBind (FunMonoBind f inf mtchs loc) binds
  | has_args mtchs
  = go mtchs loc binds
  where
    go mtchs1 loc1 (RdrValBinding (FunMonoBind f2 inf2 mtchs2 loc2) : binds)
	| f == f2 = go (mtchs2 ++ mtchs1) loc2 binds
	-- Remember binds is reversed, so glue mtchs2 on the front
	-- and use loc2 as the final location
    go mtchs1 loc1 binds = (FunMonoBind f inf mtchs1 loc1, binds)

getMonoBind bind binds = (bind, binds)

has_args ((Match args _ _) : _) = not (null args)
	-- Don't group together FunMonoBinds if they have
	-- no arguments.  This is necessary now that variable bindings
	-- with no arguments are now treated as FunMonoBinds rather
	-- than pattern bindings (tests/rename/should_fail/rnfail002).
\end{code}

\begin{code}
emptyGroup = HsGroup { hs_valds = MonoBind EmptyMonoBinds [] Recursive, 
			-- The renamer adds structure to the bindings;
			-- they start life as a single giant MonoBinds
		       hs_tyclds = [], hs_instds = [],
		       hs_fixds = [], hs_defds = [], hs_fords = [], 
		       hs_depds = [] ,hs_ruleds = [], hs_coreds = [] }

findSplice :: [HsDecl a] -> (HsGroup a, Maybe (SpliceDecl a, [HsDecl a]))
findSplice ds = add emptyGroup ds

mkGroup :: [HsDecl a] -> HsGroup a
mkGroup ds = addImpDecls emptyGroup ds

addImpDecls :: HsGroup a -> [HsDecl a] -> HsGroup a
-- The decls are imported, and should not have a splice
addImpDecls group decls = case add group decls of
				(group', Nothing) -> group'
				other		  -> panic "addImpDecls"

add :: HsGroup a -> [HsDecl a] -> (HsGroup a, Maybe (SpliceDecl a, [HsDecl a]))
	-- This stuff reverses the declarations (again) but it doesn't matter

-- Base cases
add gp []		= (gp, Nothing)
add gp (SpliceD e : ds) = (gp, Just (e, ds))

-- Class declarations: pull out the fixity signatures to the top
add gp@(HsGroup {hs_tyclds = ts, hs_fixds = fs}) (TyClD d : ds)   
	| isClassDecl d = add (gp { hs_tyclds = d : ts, 
				    hs_fixds  = [f | FixSig f <- tcdSigs d] ++ fs }) ds
	| otherwise 	= add (gp { hs_tyclds = d : ts }) ds

-- Signatures: fixity sigs go a different place than all others
add gp@(HsGroup {hs_fixds = ts}) (SigD (FixSig f) : ds) = add (gp {hs_fixds = f : ts}) ds
add gp@(HsGroup {hs_valds = ts}) (SigD d : ds)          = add (gp {hs_valds = add_sig d ts}) ds

-- Value declarations: use add_bind
add gp@(HsGroup {hs_valds  = ts}) (ValD d : ds) = add (gp { hs_valds = add_bind d ts }) ds

-- The rest are routine
add gp@(HsGroup {hs_instds = ts}) (InstD d : ds)   = add (gp { hs_instds = d : ts }) ds
add gp@(HsGroup {hs_defds  = ts}) (DefD d : ds)    = add (gp { hs_defds = d : ts }) ds
add gp@(HsGroup {hs_fords  = ts}) (ForD d : ds)    = add (gp { hs_fords = d : ts }) ds
add gp@(HsGroup {hs_depds  = ts}) (DeprecD d : ds) = add (gp { hs_depds = d : ts }) ds
add gp@(HsGroup {hs_ruleds  = ts})(RuleD d : ds)   = add (gp { hs_ruleds = d : ts }) ds
add gp@(HsGroup {hs_coreds  = ts})(CoreD d : ds)   = add (gp { hs_coreds = d : ts }) ds

add_bind b (MonoBind bs sigs r) = MonoBind (bs `AndMonoBinds` b) sigs r
add_sig  s (MonoBind bs sigs r) = MonoBind bs 		     (s:sigs) r
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

mkPrefixCon :: RdrNameHsType -> [RdrNameBangType] -> P (RdrName, RdrNameConDetails)

mkPrefixCon ty tys
 = split ty tys
 where
   split (HsAppTy t u)  ts = split t (unbangedType u : ts)
   split (HsTyVar tc)   ts = tyConToDataCon tc	>>= \ data_con ->
			     return (data_con, PrefixCon ts)
   split _		 _ = parseError "Illegal data/newtype declaration"

mkRecCon :: RdrName -> [([RdrName],RdrNameBangType)] -> P (RdrName, RdrNameConDetails)
mkRecCon con fields
  = tyConToDataCon con	>>= \ data_con ->
    return (data_con, RecCon [ (l,t) | (ls,t) <- fields, l <- ls ])

tyConToDataCon :: RdrName -> P RdrName
tyConToDataCon tc
  | isTcOcc (rdrNameOcc tc)
  = return (setRdrNameSpace tc srcDataName)
  | otherwise
  = parseError (showSDoc (text "Not a constructor:" <+> quotes (ppr tc)))

----------------------------------------------------------------------------
-- Various Syntactic Checks

checkInstType :: RdrNameHsType -> P RdrNameHsType
checkInstType t 
  = case t of
	HsForAllTy tvs ctxt ty ->
		checkDictTy ty [] >>= \ dict_ty ->
	      	return (HsForAllTy tvs ctxt dict_ty)

        HsParTy ty -> checkInstType ty

	ty ->   checkDictTy ty [] >>= \ dict_ty->
	      	return (HsForAllTy Nothing [] dict_ty)

checkTyVars :: [RdrNameHsType] -> P [RdrNameHsTyVar]
checkTyVars tvs 
  = mapM chk tvs
  where
	--  Check that the name space is correct!
    chk (HsKindSig (HsTyVar tv) k) | isRdrTyVar tv = return (IfaceTyVar tv k)
    chk (HsTyVar tv) 	           | isRdrTyVar tv = return (UserTyVar tv)
    chk other	 		   = parseError "Type found where type variable expected"

checkTyClHdr :: RdrNameHsType -> P (RdrName, [RdrNameHsTyVar])
-- The header of a type or class decl should look like
--	(C a, D b) => T a b
-- or	T a b
-- or	a + b
-- etc
checkTyClHdr ty
  = go ty []
  where
    go (HsTyVar tc)    acc 
	| not (isRdrTyVar tc) = checkTyVars acc		>>= \ tvs ->
				return (tc, tvs)
    go (HsOpTy t1 (HsTyOp tc) t2) acc  
			      = checkTyVars (t1:t2:acc)	>>= \ tvs ->
				return (tc, tvs)
    go (HsParTy ty)    acc    = go ty acc
    go (HsAppTy t1 t2) acc    = go t1 (t2:acc)
    go other	       acc    = parseError "Malformed LHS to type of class declaration"

checkContext :: RdrNameHsType -> P RdrNameContext
checkContext (HsTupleTy _ ts) 	-- (Eq a, Ord b) shows up as a tuple type
  = mapM checkPred ts

checkContext (HsParTy ty)	-- to be sure HsParTy doesn't get into the way
  = checkContext ty

checkContext (HsTyVar t)	-- Empty context shows up as a unit type ()
  | t == getRdrName unitTyCon = return []

checkContext t 
  = checkPred t >>= \p ->
    return [p]

checkPred :: RdrNameHsType -> P (HsPred RdrName)
-- Watch out.. in ...deriving( Show )... we use checkPred on 
-- the list of partially applied predicates in the deriving,
-- so there can be zero args.
checkPred (HsPredTy (HsIParam n ty)) = return (HsIParam n ty)
checkPred ty
  = go ty []
  where
    go (HsTyVar t) args   | not (isRdrTyVar t) 
		  	  = return (HsClassP t args)
    go (HsAppTy l r) args = go l (r:args)
    go (HsParTy t)   args = go t args
    go _ 	     _    = parseError "Illegal class assertion"

checkDictTy :: RdrNameHsType -> [RdrNameHsType] -> P RdrNameHsType
checkDictTy (HsTyVar t) args@(_:_) | not (isRdrTyVar t) 
  	= return (mkHsDictTy t args)
checkDictTy (HsAppTy l r) args = checkDictTy l (r:args)
checkDictTy (HsParTy t)   args = checkDictTy t args
checkDictTy _ _ = parseError "Malformed context in instance header"


---------------------------------------------------------------------------
-- Checking statements in a do-expression
-- 	We parse   do { e1 ; e2 ; }
-- 	as [ExprStmt e1, ExprStmt e2]
-- checkDo (a) checks that the last thing is an ExprStmt
--	   (b) transforms it to a ResultStmt
-- same comments apply for mdo as well

checkDo	 = checkDoMDo "a " "'do'"
checkMDo = checkDoMDo "an " "'mdo'"

checkDoMDo _   nm []		   = parseError $ "Empty " ++ nm ++ " construct"
checkDoMDo _   _  [ExprStmt e _ l] = return [ResultStmt e l]
checkDoMDo pre nm [s]		   = parseError $ "The last statement in " ++ pre ++ nm ++ " construct must be an expression"
checkDoMDo pre nm (s:ss)	   = checkDoMDo pre nm ss	>>= \ ss' ->
			   	     return (s:ss')

-- -------------------------------------------------------------------------
-- Checking Patterns.

-- We parse patterns as expressions and check for valid patterns below,
-- converting the expression into a pattern at the same time.

checkPattern :: SrcLoc -> RdrNameHsExpr -> P RdrNamePat
checkPattern loc e = setSrcLocFor loc (checkPat e [])

checkPatterns :: SrcLoc -> [RdrNameHsExpr] -> P [RdrNamePat]
checkPatterns loc es = mapM (checkPattern loc) es

checkPat :: RdrNameHsExpr -> [RdrNamePat] -> P RdrNamePat
checkPat (HsVar c) args | isRdrDataCon c = return (ConPatIn c (PrefixCon args))
checkPat (HsApp f x) args = 
	checkPat x [] >>= \x ->
	checkPat f (x:args)
checkPat e [] = case e of
	EWildPat	    -> return (WildPat placeHolderType)
	HsVar x	| isQual x  -> parseError ("Qualified variable in pattern: " ++ showRdrName x)
		| otherwise -> return (VarPat x)
	HsLit l 	   -> return (LitPat l)
	HsOverLit l	   -> return (NPatIn l Nothing)
	ELazyPat e	   -> checkPat e [] >>= (return . LazyPat)
	EAsPat n e	   -> checkPat e [] >>= (return . AsPat n)
        ExprWithTySig e t  -> checkPat e [] >>= \e ->
			      -- Pattern signatures are parsed as sigtypes,
			      -- but they aren't explicit forall points.  Hence
			      -- we have to remove the implicit forall here.
			      let t' = case t of 
					  HsForAllTy Nothing [] ty -> ty
					  other -> other
			      in
			      return (SigPatIn e t')

	-- Translate out NegApps of literals in patterns. We negate
	-- the Integer here, and add back the call to 'negate' when
	-- we typecheck the pattern.
	-- NB. Negative *primitive* literals are already handled by
	--     RdrHsSyn.mkHsNegApp
	NegApp (HsOverLit lit) neg -> return (NPatIn lit (Just neg))

	OpApp (HsVar n) (HsVar plus) _ (HsOverLit lit@(HsIntegral _ _)) 
		  	   | plus == plus_RDR
			   -> return (mkNPlusKPat n lit)
			   where
			      plus_RDR = mkUnqual varName FSLIT("+")	-- Hack

	OpApp l op fix r   -> checkPat l [] >>= \l ->
			      checkPat r [] >>= \r ->
			      case op of
			   	 HsVar c | isDataOcc (rdrNameOcc c)
					-> return (ConPatIn c (InfixCon l r))
			   	 _ -> patFail

	HsPar e		   -> checkPat e [] >>= (return . ParPat)
	ExplicitList _ es  -> mapM (\e -> checkPat e []) es >>= \ps ->
			      return (ListPat ps placeHolderType)
	ExplicitPArr _ es  -> mapM (\e -> checkPat e []) es >>= \ps ->
			      return (PArrPat ps placeHolderType)

	ExplicitTuple es b -> mapM (\e -> checkPat e []) es >>= \ps ->
			      return (TuplePat ps b)

	RecordCon c fs     -> mapM checkPatField fs >>= \fs ->
			      return (ConPatIn c (RecCon fs))
-- Generics 
	HsType ty          -> return (TypePat ty) 
	_                  -> patFail

checkPat _ _ = patFail

checkPatField :: (RdrName, RdrNameHsExpr) -> P (RdrName, RdrNamePat)
checkPatField (n,e) = checkPat e [] >>= \p ->
		      return (n,p)

patFail = parseError "Parse error in pattern"


---------------------------------------------------------------------------
-- Check Equation Syntax

checkValDef 
	:: RdrNameHsExpr
	-> Maybe RdrNameHsType
	-> RdrNameGRHSs
	-> SrcLoc
	-> P RdrBinding

checkValDef lhs opt_sig grhss loc
 = case isFunLhs lhs [] of
	   Just (f,inf,es) 
	     | isQual f
	     -> parseError ("Qualified name in function definition: "  ++ showRdrName f)
	     | otherwise
	     -> checkPatterns loc es >>= \ps ->
		return (RdrValBinding (FunMonoBind f inf [Match ps opt_sig grhss] loc))

           Nothing ->
		checkPattern loc lhs >>= \lhs ->
		return (RdrValBinding (PatMonoBind lhs grhss loc))

checkValSig
	:: RdrNameHsExpr
	-> RdrNameHsType
	-> SrcLoc
	-> P RdrBinding
checkValSig (HsVar v) ty loc | isUnqual v = return (RdrHsDecl (SigD (Sig v ty loc)))
checkValSig other     ty loc = parseError "Type signature given for an expression"

mkSigDecls :: [Sig RdrName] -> RdrBinding
mkSigDecls sigs = RdrBindings [RdrHsDecl (SigD sig) | sig <- sigs]


-- A variable binding is parsed as an RdrNameFunMonoBind.
-- See comments with HsBinds.MonoBinds

isFunLhs :: RdrNameHsExpr -> [RdrNameHsExpr] -> Maybe (RdrName, Bool, [RdrNameHsExpr])
isFunLhs (OpApp l (HsVar op) fix r) es  | not (isRdrDataCon op)
			  	= Just (op, True, (l:r:es))
					| otherwise
				= case isFunLhs l es of
				    Just (op', True, j : k : es') ->
				      Just (op', True, j : OpApp k (HsVar op) fix r : es')
				    _ -> Nothing
isFunLhs (HsVar f) es | not (isRdrDataCon f)
			 	= Just (f,False,es)
isFunLhs (HsApp f e) es 	= isFunLhs f (e:es)
isFunLhs (HsPar e)   es@(_:_) 	= isFunLhs e es
isFunLhs _ _ 			= Nothing

---------------------------------------------------------------------------
-- Miscellaneous utilities

checkPrecP :: Int -> P Int
checkPrecP i | 0 <= i && i <= maxPrecedence = return i
	     | otherwise       		    = parseError "Precedence out of range"

mkRecConstrOrUpdate 
	:: RdrNameHsExpr 
	-> RdrNameHsRecordBinds
	-> P RdrNameHsExpr

mkRecConstrOrUpdate (HsVar c) fs | isRdrDataCon c
  = return (RecordCon c fs)
mkRecConstrOrUpdate exp fs@(_:_) 
  = return (RecordUpd exp fs)
mkRecConstrOrUpdate _ _
  = parseError "Empty record update"

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
	 -> (FastString, RdrName, RdrNameHsType) 
	 -> SrcLoc 
	 -> P RdrNameHsDecl
mkImport (CCall  cconv) safety (entity, v, ty) loc =
  parseCImport entity cconv safety v			 >>= \importSpec ->
  return $ ForD (ForeignImport v ty importSpec                     False loc)
mkImport (DNCall      ) _      (entity, v, ty) loc =
  parseDImport entity 					 >>= \ spec ->
  return $ ForD (ForeignImport v ty (DNImport spec) False loc)

-- parse the entity string of a foreign import declaration for the `ccall' or
-- `stdcall' calling convention'
--
parseCImport :: FastString 
	     -> CCallConv 
	     -> Safety 
	     -> RdrName 
	     -> P ForeignImport
parseCImport entity cconv safety v
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
	  _			    -> parseError "Missing ']' in entity"
      parse3 str	header isLbl = parse4 str  header isLbl nilFS
      -- check for name of C function
      parse4 ""         header isLbl lib = build (mkExtName v) header isLbl lib
      parse4 (' ':rest) header isLbl lib = parse4 rest         header isLbl lib
      parse4 str	header isLbl lib
        | all (== ' ') rest              = build (mkFastString first)  header isLbl lib
	| otherwise			 = parseError "Malformed entity string"
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
parseDImport :: FastString -> P DNCallSpec
parseDImport entity = parse0 comps
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

  d'oh = parseError "Malformed entity string"
  
-- construct a foreign export declaration
--
mkExport :: CallConv
         -> (FastString, RdrName, RdrNameHsType) 
	 -> SrcLoc 
	 -> P RdrNameHsDecl
mkExport (CCall  cconv) (entity, v, ty) loc = return $ 
  ForD (ForeignExport v ty (CExport (CExportStatic entity' cconv)) False loc)
  where
    entity' | nullFastString entity = mkExtName v
	    | otherwise		    = entity
mkExport DNCall (entity, v, ty) loc =
  parseError "Foreign export is not yet supported for .NET"

-- Supplying the ext_name in a foreign decl is optional; if it
-- isn't there, the Haskell name is assumed. Note that no transformation
-- of the Haskell name is then performed, so if you foreign export (++),
-- it's external name will be "++". Too bad; it's important because we don't
-- want z-encoding (e.g. names with z's in them shouldn't be doubled)
-- (This is why we use occNameUserString.)
--
mkExtName :: RdrName -> CLabelString
mkExtName rdrNm = mkFastString (occNameUserString (rdrNameOcc rdrNm))

-- ---------------------------------------------------------------------------
-- Make the export list for an interface

mkIfaceExports :: [RdrNameTyClDecl] -> [RdrAvailInfo]
mkIfaceExports decls = map getExport decls
  where getExport d = case d of
			TyData{}    -> tc_export
			ClassDecl{} -> tc_export
			_other      -> var_export
          where 
		tc_export  = AvailTC (rdrNameOcc (tcdName d)) 
				(map (rdrNameOcc.fst) (tyClDeclNames d))
		var_export = Avail (rdrNameOcc (tcdName d))
\end{code}


-----------------------------------------------------------------------------
-- Misc utils

\begin{code}
showRdrName :: RdrName -> String
showRdrName r = showSDoc (ppr r)

parseError :: String -> P a
parseError s = 
  getSrcLoc >>= \ loc ->
  failLocMsgP loc loc s
\end{code}
