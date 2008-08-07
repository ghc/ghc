-----------------------------------------------------------------------------
--
-- (c) The University of Glasgow 2006
--
-- The purpose of this module is to transform an HsExpr into a CoreExpr which
-- when evaluated, returns a (Meta.Q Meta.Exp) computation analogous to the
-- input HsExpr. We do this in the DsM monad, which supplies access to
-- CoreExpr's of the "smart constructors" of the Meta.Exp datatype.
--
-- It also defines a bunch of knownKeyNames, in the same way as is done
-- in prelude/PrelNames.  It's much more convenient to do it here, becuase
-- otherwise we have to recompile PrelNames whenever we add a Name, which is
-- a Royal Pain (triggers other recompilation).
-----------------------------------------------------------------------------

{-# OPTIONS -fno-warn-unused-imports #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details
-- The kludge is only needed in this module because of trac #2267.

module DsMeta( dsBracket, 
	       templateHaskellNames, qTyConName, nameTyConName,
	       liftName, expQTyConName, patQTyConName, decQTyConName, typeQTyConName,
	       decTyConName, typeTyConName, mkNameG_dName, mkNameG_vName, mkNameG_tcName,
	       quoteExpName, quotePatName
	        ) where

import {-# SOURCE #-}	DsExpr ( dsExpr )

import MatchLit
import DsUtils
import DsMonad

import qualified Language.Haskell.TH as TH

import HsSyn
import Class
import PrelNames
-- To avoid clashes with DsMeta.varName we must make a local alias for
-- OccName.varName we do this by removing varName from the import of
-- OccName above, making a qualified instance of OccName and using
-- OccNameAlias.varName where varName ws previously used in this file.
import qualified OccName

import Module
import Id
import Name
import NameEnv
import TcType
import TyCon
import TysWiredIn
import CoreSyn
import MkCore
import CoreUtils
import SrcLoc
import Unique
import BasicTypes
import Outputable
import Bag
import FastString
import ForeignCall

import Data.Maybe
import Control.Monad
import Data.List

-----------------------------------------------------------------------------
dsBracket :: HsBracket Name -> [PendingSplice] -> DsM CoreExpr
-- Returns a CoreExpr of type TH.ExpQ
-- The quoted thing is parameterised over Name, even though it has
-- been type checked.  We don't want all those type decorations!

dsBracket brack splices
  = dsExtendMetaEnv new_bit (do_brack brack)
  where
    new_bit = mkNameEnv [(n, Splice (unLoc e)) | (n,e) <- splices]

    do_brack (VarBr n)  = do { MkC e1  <- lookupOcc n ; return e1 }
    do_brack (ExpBr e)  = do { MkC e1  <- repLE e     ; return e1 }
    do_brack (PatBr p)  = do { MkC p1  <- repLP p     ; return p1 }
    do_brack (TypBr t)  = do { MkC t1  <- repLTy t    ; return t1 }
    do_brack (DecBr ds) = do { MkC ds1 <- repTopDs ds ; return ds1 }

{- -------------- Examples --------------------

  [| \x -> x |]
====>
  gensym (unpackString "x"#) `bindQ` \ x1::String ->
  lam (pvar x1) (var x1)


  [| \x -> $(f [| x |]) |]
====>
  gensym (unpackString "x"#) `bindQ` \ x1::String ->
  lam (pvar x1) (f (var x1))
-}


-------------------------------------------------------
-- 			Declarations
-------------------------------------------------------

repTopDs :: HsGroup Name -> DsM (Core (TH.Q [TH.Dec]))
repTopDs group
 = do { let { bndrs = map unLoc (groupBinders group) } ;
	ss <- mkGenSyms bndrs ;

	-- Bind all the names mainly to avoid repeated use of explicit strings.
	-- Thus	we get
	--	do { t :: String <- genSym "T" ;
	--	     return (Data t [] ...more t's... }
	-- The other important reason is that the output must mention
	-- only "T", not "Foo:T" where Foo is the current module

	
	decls <- addBinds ss (do {
			val_ds  <- rep_val_binds (hs_valds group) ;
			tycl_ds <- mapM repTyClD (hs_tyclds group) ;
			inst_ds <- mapM repInstD' (hs_instds group) ;
			for_ds <- mapM repForD (hs_fords group) ;
			-- more needed
			return (de_loc $ sort_by_loc $ val_ds ++ catMaybes tycl_ds ++ inst_ds ++ for_ds) }) ;

	decl_ty <- lookupType decQTyConName ;
	let { core_list = coreList' decl_ty decls } ;

	dec_ty <- lookupType decTyConName ;
	q_decs  <- repSequenceQ dec_ty core_list ;

	wrapNongenSyms ss q_decs
	-- Do *not* gensym top-level binders
      }

groupBinders :: HsGroup Name -> [Located Name]
groupBinders (HsGroup { hs_valds = val_decls, hs_tyclds = tycl_decls,
			hs_fords = foreign_decls })
-- Collect the binders of a Group
  = collectHsValBinders val_decls ++
    [n | d <- tycl_decls, n <- tyClDeclNames (unLoc d)] ++
    [n | L _ (ForeignImport n _ _) <- foreign_decls]


{- 	Note [Binders and occurrences]
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we desugar [d| data T = MkT |]
we want to get
	Data "T" [] [Con "MkT" []] []
and *not*
	Data "Foo:T" [] [Con "Foo:MkT" []] []
That is, the new data decl should fit into whatever new module it is
asked to fit in.   We do *not* clone, though; no need for this:
	Data "T79" ....

But if we see this:
	data T = MkT 
	foo = reifyDecl T

then we must desugar to
	foo = Data "Foo:T" [] [Con "Foo:MkT" []] []

So in repTopDs we bring the binders into scope with mkGenSyms and addBinds.
And we use lookupOcc, rather than lookupBinder
in repTyClD and repC.

-}

repTyClD :: LTyClDecl Name -> DsM (Maybe (SrcSpan, Core TH.DecQ))

repTyClD (L loc (TyData { tcdND = DataType, tcdCtxt = cxt, 
		    tcdLName = tc, tcdTyVars = tvs, 
		    tcdCons = cons, tcdDerivs = mb_derivs }))
 = do { tc1 <- lookupLOcc tc ;		-- See note [Binders and occurrences] 
        dec <- addTyVarBinds tvs $ \bndrs -> do {
      	       cxt1    <- repLContext cxt ;
               cons1   <- mapM repC cons ;
      	       cons2   <- coreList conQTyConName cons1 ;
      	       derivs1 <- repDerivs mb_derivs ;
	       bndrs1  <- coreList nameTyConName bndrs ;
      	       repData cxt1 tc1 bndrs1 cons2 derivs1 } ;
        return $ Just (loc, dec) }

repTyClD (L loc (TyData { tcdND = NewType, tcdCtxt = cxt, 
		    tcdLName = tc, tcdTyVars = tvs, 
		    tcdCons = [con], tcdDerivs = mb_derivs }))
 = do { tc1 <- lookupLOcc tc ;		-- See note [Binders and occurrences] 
        dec <- addTyVarBinds tvs $ \bndrs -> do {
      	       cxt1   <- repLContext cxt ;
               con1   <- repC con ;
      	       derivs1 <- repDerivs mb_derivs ;
	       bndrs1  <- coreList nameTyConName bndrs ;
      	       repNewtype cxt1 tc1 bndrs1 con1 derivs1 } ;
        return $ Just (loc, dec) }

repTyClD (L loc (TySynonym { tcdLName = tc, tcdTyVars = tvs, tcdSynRhs = ty }))
 = do { tc1 <- lookupLOcc tc ;		-- See note [Binders and occurrences] 
        dec <- addTyVarBinds tvs $ \bndrs -> do {
	       ty1     <- repLTy ty ;
	       bndrs1  <- coreList nameTyConName bndrs ;
	       repTySyn tc1 bndrs1 ty1 } ;
 	return (Just (loc, dec)) }

repTyClD (L loc (ClassDecl { tcdCtxt = cxt, tcdLName = cls, 
		      tcdTyVars = tvs, 
		      tcdFDs = fds,
		      tcdSigs = sigs, tcdMeths = meth_binds }))
 = do { cls1 <- lookupLOcc cls ;		-- See note [Binders and occurrences] 
    	dec  <- addTyVarBinds tvs $ \bndrs -> do {
 		  cxt1   <- repLContext cxt ;
 		  sigs1  <- rep_sigs sigs ;
 		  binds1 <- rep_binds meth_binds ;
	          fds1 <- repLFunDeps fds;
 		  decls1 <- coreList decQTyConName (sigs1 ++ binds1) ;
	          bndrs1 <- coreList nameTyConName bndrs ;
 		  repClass cxt1 cls1 bndrs1 fds1 decls1 } ;
    	return $ Just (loc, dec) }

-- Un-handled cases
repTyClD (L loc d) = putSrcSpanDs loc $
		     do { warnDs (hang ds_msg 4 (ppr d))
			; return Nothing }

-- represent fundeps
--
repLFunDeps :: [Located (FunDep Name)] -> DsM (Core [TH.FunDep])
repLFunDeps fds = do fds' <- mapM repLFunDep fds
                     fdList <- coreList funDepTyConName fds'
                     return fdList

repLFunDep :: Located (FunDep Name) -> DsM (Core TH.FunDep)
repLFunDep (L _ (xs, ys)) = do xs' <- mapM lookupBinder xs
                               ys' <- mapM lookupBinder ys
                               xs_list <- coreList nameTyConName xs'
                               ys_list <- coreList nameTyConName ys'
                               repFunDep xs_list ys_list

repInstD' :: LInstDecl Name -> DsM (SrcSpan, Core TH.DecQ)
repInstD' (L loc (InstDecl ty binds _ _))		-- Ignore user pragmas for now
 = do	{ i <- addTyVarBinds tvs $ \_ ->
		-- We must bring the type variables into scope, so their occurrences
		-- don't fail,  even though the binders don't appear in the resulting 
		-- data structure
		do {  cxt1 <- repContext cxt
		   ; inst_ty1 <- repPred (HsClassP cls tys)
		   ; ss <- mkGenSyms (collectHsBindBinders binds)
		   ; binds1 <- addBinds ss (rep_binds binds)
		   ; decls1 <- coreList decQTyConName binds1
		   ; decls2 <- wrapNongenSyms ss decls1
		   -- wrapNonGenSyms: do not clone the class op names!
		   -- They must be called 'op' etc, not 'op34'
		   ; repInst cxt1 inst_ty1 decls2 }

	; return (loc, i)}
 where
   (tvs, cxt, cls, tys) = splitHsInstDeclTy (unLoc ty)

repForD :: Located (ForeignDecl Name) -> DsM (SrcSpan, Core TH.DecQ)
repForD (L loc (ForeignImport name typ (CImport cc s ch cn cis)))
 = do MkC name' <- lookupLOcc name
      MkC typ' <- repLTy typ
      MkC cc' <- repCCallConv cc
      MkC s' <- repSafety s
      cis' <- conv_cimportspec cis
      MkC str <- coreStringLit $ static
                              ++ unpackFS ch ++ " "
                              ++ unpackFS cn ++ " "
                              ++ cis'
      dec <- rep2 forImpDName [cc', s', str, name', typ']
      return (loc, dec)
 where
    conv_cimportspec (CLabel cls) = notHandled "Foreign label" (doubleQuotes (ppr cls))
    conv_cimportspec (CFunction DynamicTarget) = return "dynamic"
    conv_cimportspec (CFunction (StaticTarget fs)) = return (unpackFS fs)
    conv_cimportspec CWrapper = return "wrapper"
    static = case cis of
                 CFunction (StaticTarget _) -> "static "
                 _ -> ""
repForD decl = notHandled "Foreign declaration" (ppr decl)

repCCallConv :: CCallConv -> DsM (Core TH.Callconv)
repCCallConv CCallConv = rep2 cCallName []
repCCallConv StdCallConv = rep2 stdCallName []
repCCallConv CmmCallConv = notHandled "repCCallConv" (ppr CmmCallConv)

repSafety :: Safety -> DsM (Core TH.Safety)
repSafety PlayRisky = rep2 unsafeName []
repSafety (PlaySafe False) = rep2 safeName []
repSafety (PlaySafe True) = rep2 threadsafeName []

ds_msg :: SDoc
ds_msg = ptext (sLit "Cannot desugar this Template Haskell declaration:")

-------------------------------------------------------
-- 			Constructors
-------------------------------------------------------

repC :: LConDecl Name -> DsM (Core TH.ConQ)
repC (L _ (ConDecl con _ [] (L _ []) details ResTyH98 _))
  = do { con1 <- lookupLOcc con ;		-- See note [Binders and occurrences] 
	 repConstr con1 details }
repC (L loc (ConDecl con expl tvs (L cloc ctxt) details ResTyH98 doc))
  = do { addTyVarBinds tvs $ \bndrs -> do {
             c' <- repC (L loc (ConDecl con expl [] (L cloc []) details ResTyH98 doc));
             ctxt' <- repContext ctxt;
             bndrs' <- coreList nameTyConName bndrs;
             rep2 forallCName [unC bndrs', unC ctxt', unC c']
         }
       }
repC (L loc con_decl)		-- GADTs
  = putSrcSpanDs loc $
    notHandled "GADT declaration" (ppr con_decl) 

repBangTy :: LBangType Name -> DsM (Core (TH.StrictTypeQ))
repBangTy ty= do 
  MkC s <- rep2 str []
  MkC t <- repLTy ty'
  rep2 strictTypeName [s, t]
  where 
    (str, ty') = case ty of
		   L _ (HsBangTy _ ty) -> (isStrictName,  ty)
		   _                   -> (notStrictName, ty)

-------------------------------------------------------
-- 			Deriving clause
-------------------------------------------------------

repDerivs :: Maybe [LHsType Name] -> DsM (Core [TH.Name])
repDerivs Nothing = coreList nameTyConName []
repDerivs (Just ctxt)
  = do { strs <- mapM rep_deriv ctxt ; 
	 coreList nameTyConName strs }
  where
    rep_deriv :: LHsType Name -> DsM (Core TH.Name)
	-- Deriving clauses must have the simple H98 form
    rep_deriv (L _ (HsPredTy (HsClassP cls []))) = lookupOcc cls
    rep_deriv other = notHandled "Non-H98 deriving clause" (ppr other)


-------------------------------------------------------
--   Signatures in a class decl, or a group of bindings
-------------------------------------------------------

rep_sigs :: [LSig Name] -> DsM [Core TH.DecQ]
rep_sigs sigs = do locs_cores <- rep_sigs' sigs
                   return $ de_loc $ sort_by_loc locs_cores

rep_sigs' :: [LSig Name] -> DsM [(SrcSpan, Core TH.DecQ)]
	-- We silently ignore ones we don't recognise
rep_sigs' sigs = do { sigs1 <- mapM rep_sig sigs ;
		     return (concat sigs1) }

rep_sig :: LSig Name -> DsM [(SrcSpan, Core TH.DecQ)]
	-- Singleton => Ok
	-- Empty     => Too hard, signature ignored
rep_sig (L loc (TypeSig nm ty)) = rep_proto nm ty loc
rep_sig _                       = return []

rep_proto :: Located Name -> LHsType Name -> SrcSpan -> DsM [(SrcSpan, Core TH.DecQ)]
rep_proto nm ty loc = do { nm1 <- lookupLOcc nm ; 
		       ty1 <- repLTy ty ; 
		       sig <- repProto nm1 ty1 ;
		       return [(loc, sig)] }


-------------------------------------------------------
-- 			Types
-------------------------------------------------------

-- gensym a list of type variables and enter them into the meta environment;
-- the computations passed as the second argument is executed in that extended
-- meta environment and gets the *new* names on Core-level as an argument
--
addTyVarBinds :: [LHsTyVarBndr Name]	         -- the binders to be added
	      -> ([Core TH.Name] -> DsM (Core (TH.Q a))) -- action in the ext env
	      -> DsM (Core (TH.Q a))
addTyVarBinds tvs m =
  do
    let names = map (hsTyVarName.unLoc) tvs
    freshNames <- mkGenSyms names
    term       <- addBinds freshNames $ do
		    bndrs <- mapM lookupBinder names 
		    m bndrs
    wrapGenSyns freshNames term

-- represent a type context
--
repLContext :: LHsContext Name -> DsM (Core TH.CxtQ)
repLContext (L _ ctxt) = repContext ctxt

repContext :: HsContext Name -> DsM (Core TH.CxtQ)
repContext ctxt = do 
	            preds    <- mapM repLPred ctxt
		    predList <- coreList typeQTyConName preds
		    repCtxt predList

-- represent a type predicate
--
repLPred :: LHsPred Name -> DsM (Core TH.TypeQ)
repLPred (L _ p) = repPred p

repPred :: HsPred Name -> DsM (Core TH.TypeQ)
repPred (HsClassP cls tys) = do
			       tcon <- repTy (HsTyVar cls)
			       tys1 <- repLTys tys
			       repTapps tcon tys1
repPred p@(HsEqualP _ _) = notHandled "Equational constraint" (ppr p)
repPred p@(HsIParam _ _) = notHandled "Implicit parameter constraint" (ppr p)

-- yield the representation of a list of types
--
repLTys :: [LHsType Name] -> DsM [Core TH.TypeQ]
repLTys tys = mapM repLTy tys

-- represent a type
--
repLTy :: LHsType Name -> DsM (Core TH.TypeQ)
repLTy (L _ ty) = repTy ty

repTy :: HsType Name -> DsM (Core TH.TypeQ)
repTy (HsForAllTy _ tvs ctxt ty)  = 
  addTyVarBinds tvs $ \bndrs -> do
    ctxt1  <- repLContext ctxt
    ty1    <- repLTy ty
    bndrs1 <- coreList nameTyConName bndrs
    repTForall bndrs1 ctxt1 ty1

repTy (HsTyVar n)
  | isTvOcc (nameOccName n)       = do 
				      tv1 <- lookupTvOcc n
				      repTvar tv1
  | otherwise		          = do 
				      tc1 <- lookupOcc n
				      repNamedTyCon tc1
repTy (HsAppTy f a)               = do 
				      f1 <- repLTy f
				      a1 <- repLTy a
				      repTapp f1 a1
repTy (HsFunTy f a)               = do 
				      f1   <- repLTy f
				      a1   <- repLTy a
				      tcon <- repArrowTyCon
				      repTapps tcon [f1, a1]
repTy (HsListTy t)		  = do
				      t1   <- repLTy t
				      tcon <- repListTyCon
				      repTapp tcon t1
repTy (HsPArrTy t)                = do
				      t1   <- repLTy t
				      tcon <- repTy (HsTyVar (tyConName parrTyCon))
				      repTapp tcon t1
repTy (HsTupleTy _ tys)	  = do
				      tys1 <- repLTys tys 
				      tcon <- repTupleTyCon (length tys)
				      repTapps tcon tys1
repTy (HsOpTy ty1 n ty2) 	  = repLTy ((nlHsTyVar (unLoc n) `nlHsAppTy` ty1) 
					   `nlHsAppTy` ty2)
repTy (HsParTy t)  	       	  = repLTy t
repTy (HsPredTy pred)             = repPred pred
repTy ty@(HsNumTy _)              = notHandled "Number types (for generics)" (ppr ty)
repTy ty			  = notHandled "Exotic form of type" (ppr ty)


-----------------------------------------------------------------------------
-- 		Expressions
-----------------------------------------------------------------------------

repLEs :: [LHsExpr Name] -> DsM (Core [TH.ExpQ])
repLEs es = do { es'  <- mapM repLE es ;
		 coreList expQTyConName es' }

-- FIXME: some of these panics should be converted into proper error messages
--	  unless we can make sure that constructs, which are plainly not
--	  supported in TH already lead to error messages at an earlier stage
repLE :: LHsExpr Name -> DsM (Core TH.ExpQ)
repLE (L loc e) = putSrcSpanDs loc (repE e)

repE :: HsExpr Name -> DsM (Core TH.ExpQ)
repE (HsVar x)            =
  do { mb_val <- dsLookupMetaEnv x 
     ; case mb_val of
	Nothing	         -> do { str <- globalVar x
			       ; repVarOrCon x str }
	Just (Bound y)   -> repVarOrCon x (coreVar y)
	Just (Splice e)  -> do { e' <- dsExpr e
			       ; return (MkC e') } }
repE e@(HsIPVar _) = notHandled "Implicit parameters" (ppr e)

	-- Remember, we're desugaring renamer output here, so
	-- HsOverlit can definitely occur
repE (HsOverLit l) = do { a <- repOverloadedLiteral l; repLit a }
repE (HsLit l)     = do { a <- repLiteral l;           repLit a }
repE (HsLam (MatchGroup [m] _)) = repLambda m
repE (HsApp x y)   = do {a <- repLE x; b <- repLE y; repApp a b}

repE (OpApp e1 op _ e2) =
  do { arg1 <- repLE e1; 
       arg2 <- repLE e2; 
       the_op <- repLE op ;
       repInfixApp arg1 the_op arg2 } 
repE (NegApp x _)        = do
			      a         <- repLE x
			      negateVar <- lookupOcc negateName >>= repVar
			      negateVar `repApp` a
repE (HsPar x)            = repLE x
repE (SectionL x y)       = do { a <- repLE x; b <- repLE y; repSectionL a b } 
repE (SectionR x y)       = do { a <- repLE x; b <- repLE y; repSectionR a b } 
repE (HsCase e (MatchGroup ms _)) = do { arg <- repLE e
				       ; ms2 <- mapM repMatchTup ms
				       ; repCaseE arg (nonEmptyCoreList ms2) }
repE (HsIf x y z)         = do
			      a <- repLE x
			      b <- repLE y
			      c <- repLE z
			      repCond a b c
repE (HsLet bs e)         = do { (ss,ds) <- repBinds bs
			       ; e2 <- addBinds ss (repLE e)
			       ; z <- repLetE ds e2
			       ; wrapGenSyns ss z }
-- FIXME: I haven't got the types here right yet
repE (HsDo DoExpr sts body _) 
 = do { (ss,zs) <- repLSts sts; 
	body'	<- addBinds ss $ repLE body;
	ret	<- repNoBindSt body';	
        e       <- repDoE (nonEmptyCoreList (zs ++ [ret]));
        wrapGenSyns ss e }
repE (HsDo ListComp sts body _)
 = do { (ss,zs) <- repLSts sts; 
	body'	<- addBinds ss $ repLE body;
	ret	<- repNoBindSt body';	
        e       <- repComp (nonEmptyCoreList (zs ++ [ret]));
        wrapGenSyns ss e }
repE e@(HsDo _ _ _ _) = notHandled "mdo and [: :]" (ppr e)
repE (ExplicitList _ es) = do { xs <- repLEs es; repListExp xs }
repE e@(ExplicitPArr _ _) = notHandled "Parallel arrays" (ppr e)
repE e@(ExplicitTuple es boxed) 
  | isBoxed boxed         = do { xs <- repLEs es; repTup xs }
  | otherwise		  = notHandled "Unboxed tuples" (ppr e)
repE (RecordCon c _ flds)
 = do { x <- lookupLOcc c;
        fs <- repFields flds;
        repRecCon x fs }
repE (RecordUpd e flds _ _ _)
 = do { x <- repLE e;
        fs <- repFields flds;
        repRecUpd x fs }

repE (ExprWithTySig e ty) = do { e1 <- repLE e; t1 <- repLTy ty; repSigExp e1 t1 }
repE (ArithSeq _ aseq) =
  case aseq of
    From e              -> do { ds1 <- repLE e; repFrom ds1 }
    FromThen e1 e2      -> do 
		             ds1 <- repLE e1
			     ds2 <- repLE e2
			     repFromThen ds1 ds2
    FromTo   e1 e2      -> do 
			     ds1 <- repLE e1
			     ds2 <- repLE e2
			     repFromTo ds1 ds2
    FromThenTo e1 e2 e3 -> do 
			     ds1 <- repLE e1
			     ds2 <- repLE e2
			     ds3 <- repLE e3
			     repFromThenTo ds1 ds2 ds3
repE (HsSpliceE (HsSplice n _)) 
  = do { mb_val <- dsLookupMetaEnv n
       ; case mb_val of
		 Just (Splice e) -> do { e' <- dsExpr e
				       ; return (MkC e') }
		 _ -> pprPanic "HsSplice" (ppr n) }
			-- Should not happen; statically checked

repE e@(PArrSeq {})      = notHandled "Parallel arrays" (ppr e)
repE e@(HsCoreAnn {})    = notHandled "Core annotations" (ppr e)
repE e@(HsSCC {})        = notHandled "Cost centres" (ppr e)
repE e@(HsTickPragma {}) = notHandled "Tick Pragma" (ppr e)
repE e@(HsBracketOut {}) = notHandled "TH brackets" (ppr e)
repE e 			 = notHandled "Expression form" (ppr e)

-----------------------------------------------------------------------------
-- Building representations of auxillary structures like Match, Clause, Stmt, 

repMatchTup ::  LMatch Name -> DsM (Core TH.MatchQ) 
repMatchTup (L _ (Match [p] _ (GRHSs guards wheres))) =
  do { ss1 <- mkGenSyms (collectPatBinders p) 
     ; addBinds ss1 $ do {
     ; p1 <- repLP p
     ; (ss2,ds) <- repBinds wheres
     ; addBinds ss2 $ do {
     ; gs    <- repGuards guards
     ; match <- repMatch p1 gs ds
     ; wrapGenSyns (ss1++ss2) match }}}
repMatchTup _ = panic "repMatchTup: case alt with more than one arg"

repClauseTup ::  LMatch Name -> DsM (Core TH.ClauseQ)
repClauseTup (L _ (Match ps _ (GRHSs guards wheres))) =
  do { ss1 <- mkGenSyms (collectPatsBinders ps) 
     ; addBinds ss1 $ do {
       ps1 <- repLPs ps
     ; (ss2,ds) <- repBinds wheres
     ; addBinds ss2 $ do {
       gs <- repGuards guards
     ; clause <- repClause ps1 gs ds
     ; wrapGenSyns (ss1++ss2) clause }}}

repGuards ::  [LGRHS Name] ->  DsM (Core TH.BodyQ)
repGuards [L _ (GRHS [] e)]
  = do {a <- repLE e; repNormal a }
repGuards other 
  = do { zs <- mapM process other;
     let {(xs, ys) = unzip zs};
	 gd <- repGuarded (nonEmptyCoreList ys);
     wrapGenSyns (concat xs) gd }
  where 
    process :: LGRHS Name -> DsM ([GenSymBind], (Core (TH.Q (TH.Guard, TH.Exp))))
    process (L _ (GRHS [L _ (ExprStmt e1 _ _)] e2))
           = do { x <- repLNormalGE e1 e2;
                  return ([], x) }
    process (L _ (GRHS ss rhs))
           = do (gs, ss') <- repLSts ss
		rhs' <- addBinds gs $ repLE rhs
                g <- repPatGE (nonEmptyCoreList ss') rhs'
                return (gs, g)

repFields :: HsRecordBinds Name -> DsM (Core [TH.Q TH.FieldExp])
repFields (HsRecFields { rec_flds = flds })
  = do	{ fnames <- mapM lookupLOcc (map hsRecFieldId flds)
	; es <- mapM repLE (map hsRecFieldArg flds)
	; fs <- zipWithM repFieldExp fnames es
	; coreList fieldExpQTyConName fs }


-----------------------------------------------------------------------------
-- Representing Stmt's is tricky, especially if bound variables
-- shadow each other. Consider:  [| do { x <- f 1; x <- f x; g x } |]
-- First gensym new names for every variable in any of the patterns.
-- both static (x'1 and x'2), and dynamic ((gensym "x") and (gensym "y"))
-- if variables didn't shaddow, the static gensym wouldn't be necessary
-- and we could reuse the original names (x and x).
--
-- do { x'1 <- gensym "x"
--    ; x'2 <- gensym "x"   
--    ; doE [ BindSt (pvar x'1) [| f 1 |]
--          , BindSt (pvar x'2) [| f x |] 
--          , NoBindSt [| g x |] 
--          ]
--    }

-- The strategy is to translate a whole list of do-bindings by building a
-- bigger environment, and a bigger set of meta bindings 
-- (like:  x'1 <- gensym "x" ) and then combining these with the translations
-- of the expressions within the Do
      
-----------------------------------------------------------------------------
-- The helper function repSts computes the translation of each sub expression
-- and a bunch of prefix bindings denoting the dynamic renaming.

repLSts :: [LStmt Name] -> DsM ([GenSymBind], [Core TH.StmtQ])
repLSts stmts = repSts (map unLoc stmts)

repSts :: [Stmt Name] -> DsM ([GenSymBind], [Core TH.StmtQ])
repSts (BindStmt p e _ _ : ss) =
   do { e2 <- repLE e 
      ; ss1 <- mkGenSyms (collectPatBinders p) 
      ; addBinds ss1 $ do {
      ; p1 <- repLP p; 
      ; (ss2,zs) <- repSts ss
      ; z <- repBindSt p1 e2
      ; return (ss1++ss2, z : zs) }}
repSts (LetStmt bs : ss) =
   do { (ss1,ds) <- repBinds bs
      ; z <- repLetSt ds
      ; (ss2,zs) <- addBinds ss1 (repSts ss)
      ; return (ss1++ss2, z : zs) } 
repSts (ExprStmt e _ _ : ss) =       
   do { e2 <- repLE e
      ; z <- repNoBindSt e2 
      ; (ss2,zs) <- repSts ss
      ; return (ss2, z : zs) }
repSts []    = return ([],[])
repSts other = notHandled "Exotic statement" (ppr other)


-----------------------------------------------------------
--			Bindings
-----------------------------------------------------------

repBinds :: HsLocalBinds Name -> DsM ([GenSymBind], Core [TH.DecQ]) 
repBinds EmptyLocalBinds
  = do	{ core_list <- coreList decQTyConName []
	; return ([], core_list) }

repBinds b@(HsIPBinds _) = notHandled "Implicit parameters" (ppr b)

repBinds (HsValBinds decs)
 = do	{ let { bndrs = map unLoc (collectHsValBinders decs) }
		-- No need to worrry about detailed scopes within
		-- the binding group, because we are talking Names
		-- here, so we can safely treat it as a mutually 
		-- recursive group
	; ss        <- mkGenSyms bndrs
	; prs       <- addBinds ss (rep_val_binds decs)
	; core_list <- coreList decQTyConName 
				(de_loc (sort_by_loc prs))
	; return (ss, core_list) }

rep_val_binds :: HsValBinds Name -> DsM [(SrcSpan, Core TH.DecQ)]
-- Assumes: all the binders of the binding are alrady in the meta-env
rep_val_binds (ValBindsOut binds sigs)
 = do { core1 <- rep_binds' (unionManyBags (map snd binds))
      ;	core2 <- rep_sigs' sigs
      ;	return (core1 ++ core2) }
rep_val_binds (ValBindsIn _ _)
 = panic "rep_val_binds: ValBindsIn"

rep_binds :: LHsBinds Name -> DsM [Core TH.DecQ]
rep_binds binds = do { binds_w_locs <- rep_binds' binds
		     ; return (de_loc (sort_by_loc binds_w_locs)) }

rep_binds' :: LHsBinds Name -> DsM [(SrcSpan, Core TH.DecQ)]
rep_binds' binds = mapM rep_bind (bagToList binds)

rep_bind :: LHsBind Name -> DsM (SrcSpan, Core TH.DecQ)
-- Assumes: all the binders of the binding are alrady in the meta-env

-- Note GHC treats declarations of a variable (not a pattern) 
-- e.g.  x = g 5 as a Fun MonoBinds. This is indicated by a single match 
-- with an empty list of patterns
rep_bind (L loc (FunBind { fun_id = fn, 
			   fun_matches = MatchGroup [L _ (Match [] _ (GRHSs guards wheres))] _ }))
 = do { (ss,wherecore) <- repBinds wheres
	; guardcore <- addBinds ss (repGuards guards)
	; fn'  <- lookupLBinder fn
	; p    <- repPvar fn'
	; ans  <- repVal p guardcore wherecore
	; ans' <- wrapGenSyns ss ans
	; return (loc, ans') }

rep_bind (L loc (FunBind { fun_id = fn, fun_matches = MatchGroup ms _ }))
 =   do { ms1 <- mapM repClauseTup ms
	; fn' <- lookupLBinder fn
        ; ans <- repFun fn' (nonEmptyCoreList ms1)
        ; return (loc, ans) }

rep_bind (L loc (PatBind { pat_lhs = pat, pat_rhs = GRHSs guards wheres }))
 =   do { patcore <- repLP pat 
        ; (ss,wherecore) <- repBinds wheres
	; guardcore <- addBinds ss (repGuards guards)
        ; ans  <- repVal patcore guardcore wherecore
	; ans' <- wrapGenSyns ss ans
        ; return (loc, ans') }

rep_bind (L _ (VarBind { var_id = v, var_rhs = e}))
 =   do { v' <- lookupBinder v 
	; e2 <- repLE e
        ; x <- repNormal e2
        ; patcore <- repPvar v'
	; empty_decls <- coreList decQTyConName [] 
        ; ans <- repVal patcore x empty_decls
        ; return (srcLocSpan (getSrcLoc v), ans) }

rep_bind (L _ (AbsBinds {})) = panic "rep_bind: AbsBinds"

-----------------------------------------------------------------------------
-- Since everything in a Bind is mutually recursive we need rename all
-- all the variables simultaneously. For example: 
-- [| AndMonoBinds (f x = x + g 2) (g x = f 1 + 2) |] would translate to
-- do { f'1 <- gensym "f"
--    ; g'2 <- gensym "g"
--    ; [ do { x'3 <- gensym "x"; fun f'1 [pvar x'3] [| x + g2 |]},
--        do { x'4 <- gensym "x"; fun g'2 [pvar x'4] [| f 1 + 2 |]}
--      ]}
-- This requires collecting the bindings (f'1 <- gensym "f"), and the 
-- environment ( f |-> f'1 ) from each binding, and then unioning them 
-- together. As we do this we collect GenSymBinds's which represent the renamed 
-- variables bound by the Bindings. In order not to lose track of these 
-- representations we build a shadow datatype MB with the same structure as 
-- MonoBinds, but which has slots for the representations


-----------------------------------------------------------------------------
-- GHC allows a more general form of lambda abstraction than specified
-- by Haskell 98. In particular it allows guarded lambda's like : 
-- (\  x | even x -> 0 | odd x -> 1) at the moment we can't represent this in
-- Haskell Template's Meta.Exp type so we punt if it isn't a simple thing like
-- (\ p1 .. pn -> exp) by causing an error.  

repLambda :: LMatch Name -> DsM (Core TH.ExpQ)
repLambda (L _ (Match ps _ (GRHSs [L _ (GRHS [] e)] EmptyLocalBinds)))
 = do { let bndrs = collectPatsBinders ps ;
      ; ss  <- mkGenSyms bndrs
      ; lam <- addBinds ss (
		do { xs <- repLPs ps; body <- repLE e; repLam xs body })
      ; wrapGenSyns ss lam }

repLambda (L _ m) = notHandled "Guarded labmdas" (pprMatch (LambdaExpr :: HsMatchContext Name) m)

  
-----------------------------------------------------------------------------
--			Patterns
-- repP deals with patterns.  It assumes that we have already
-- walked over the pattern(s) once to collect the binders, and 
-- have extended the environment.  So every pattern-bound 
-- variable should already appear in the environment.

-- Process a list of patterns
repLPs :: [LPat Name] -> DsM (Core [TH.PatQ])
repLPs ps = do { ps' <- mapM repLP ps ;
		 coreList patQTyConName ps' }

repLP :: LPat Name -> DsM (Core TH.PatQ)
repLP (L _ p) = repP p

repP :: Pat Name -> DsM (Core TH.PatQ)
repP (WildPat _)       = repPwild 
repP (LitPat l)        = do { l2 <- repLiteral l; repPlit l2 }
repP (VarPat x)        = do { x' <- lookupBinder x; repPvar x' }
repP (LazyPat p)       = do { p1 <- repLP p; repPtilde p1 }
repP (AsPat x p)       = do { x' <- lookupLBinder x; p1 <- repLP p; repPaspat x' p1 }
repP (ParPat p)        = repLP p 
repP (ListPat ps _)    = do { qs <- repLPs ps; repPlist qs }
repP (TuplePat ps _ _) = do { qs <- repLPs ps; repPtup qs }
repP (ConPatIn dc details)
 = do { con_str <- lookupLOcc dc
      ; case details of
         PrefixCon ps -> do { qs <- repLPs ps; repPcon con_str qs }
         RecCon rec   -> do { let flds = rec_flds rec
			    ; vs <- sequence $ map lookupLOcc (map hsRecFieldId flds)
                            ; ps <- sequence $ map repLP (map hsRecFieldArg flds)
                            ; fps <- zipWithM (\x y -> rep2 fieldPatName [unC x,unC y]) vs ps
                            ; fps' <- coreList fieldPatQTyConName fps
                            ; repPrec con_str fps' }
         InfixCon p1 p2 -> do { p1' <- repLP p1;
                                p2' <- repLP p2;
                                repPinfix p1' con_str p2' }
   }
repP (NPat l Nothing _)  = do { a <- repOverloadedLiteral l; repPlit a }
repP p@(NPat _ (Just _) _) = notHandled "Negative overloaded patterns" (ppr p)
repP p@(SigPatIn {})  = notHandled "Type signatures in patterns" (ppr p)
	-- The problem is to do with scoped type variables.
	-- To implement them, we have to implement the scoping rules
	-- here in DsMeta, and I don't want to do that today!
	--	 do { p' <- repLP p; t' <- repLTy t; repPsig p' t' }
	--	repPsig :: Core TH.PatQ -> Core TH.TypeQ -> DsM (Core TH.PatQ)
	--	repPsig (MkC p) (MkC t) = rep2 sigPName [p, t]

repP other = notHandled "Exotic pattern" (ppr other)

----------------------------------------------------------
-- Declaration ordering helpers

sort_by_loc :: [(SrcSpan, a)] -> [(SrcSpan, a)]
sort_by_loc xs = sortBy comp xs
    where comp x y = compare (fst x) (fst y)

de_loc :: [(a, b)] -> [b]
de_loc = map snd

----------------------------------------------------------
--	The meta-environment

-- A name/identifier association for fresh names of locally bound entities
type GenSymBind = (Name, Id)	-- Gensym the string and bind it to the Id
				-- I.e.		(x, x_id) means
				--	let x_id = gensym "x" in ...

-- Generate a fresh name for a locally bound entity

mkGenSyms :: [Name] -> DsM [GenSymBind]
-- We can use the existing name.  For example:
--	[| \x_77 -> x_77 + x_77 |]
-- desugars to
--	do { x_77 <- genSym "x"; .... }
-- We use the same x_77 in the desugared program, but with the type Bndr
-- instead of Int
--
-- We do make it an Internal name, though (hence localiseName)
--
-- Nevertheless, it's monadic because we have to generate nameTy
mkGenSyms ns = do { var_ty <- lookupType nameTyConName
		  ; return [(nm, mkLocalId (localiseName nm) var_ty) | nm <- ns] }

	     
addBinds :: [GenSymBind] -> DsM a -> DsM a
-- Add a list of fresh names for locally bound entities to the 
-- meta environment (which is part of the state carried around 
-- by the desugarer monad) 
addBinds bs m = dsExtendMetaEnv (mkNameEnv [(n,Bound id) | (n,id) <- bs]) m

-- Look up a locally bound name
--
lookupLBinder :: Located Name -> DsM (Core TH.Name)
lookupLBinder (L _ n) = lookupBinder n

lookupBinder :: Name -> DsM (Core TH.Name)
lookupBinder n 
  = do { mb_val <- dsLookupMetaEnv n;
	 case mb_val of
	    Just (Bound x) -> return (coreVar x)
	    _              -> failWithDs msg }
  where
    msg = ptext (sLit "DsMeta: failed binder lookup when desugaring a TH bracket:") <+> ppr n

-- Look up a name that is either locally bound or a global name
--
--  * If it is a global name, generate the "original name" representation (ie,
--   the <module>:<name> form) for the associated entity
--
lookupLOcc :: Located Name -> DsM (Core TH.Name)
-- Lookup an occurrence; it can't be a splice.
-- Use the in-scope bindings if they exist
lookupLOcc (L _ n) = lookupOcc n

lookupOcc :: Name -> DsM (Core TH.Name)
lookupOcc n
  = do {  mb_val <- dsLookupMetaEnv n ;
          case mb_val of
		Nothing         -> globalVar n
		Just (Bound x)  -> return (coreVar x)
		Just (Splice _) -> pprPanic "repE:lookupOcc" (ppr n) 
    }

lookupTvOcc :: Name -> DsM (Core TH.Name)
-- Type variables can't be staged and are not lexically scoped in TH
lookupTvOcc n	
  = do {  mb_val <- dsLookupMetaEnv n ;
          case mb_val of
		Just (Bound x)  -> return (coreVar x)
		_               -> failWithDs msg
    }
  where
    msg = vcat  [ ptext (sLit "Illegal lexically-scoped type variable") <+> quotes (ppr n)
		, ptext (sLit "Lexically scoped type variables are not supported by Template Haskell") ]

globalVar :: Name -> DsM (Core TH.Name)
-- Not bound by the meta-env
-- Could be top-level; or could be local
--	f x = $(g [| x |])
-- Here the x will be local
globalVar name
  | isExternalName name
  = do	{ MkC mod <- coreStringLit name_mod
        ; MkC pkg <- coreStringLit name_pkg
	; MkC occ <- occNameLit name
	; rep2 mk_varg [pkg,mod,occ] }
  | otherwise
  = do 	{ MkC occ <- occNameLit name
	; MkC uni <- coreIntLit (getKey (getUnique name))
	; rep2 mkNameLName [occ,uni] }
  where
      mod = nameModule name
      name_mod = moduleNameString (moduleName mod)
      name_pkg = packageIdString (modulePackageId mod)
      name_occ = nameOccName name
      mk_varg | OccName.isDataOcc name_occ = mkNameG_dName
	      | OccName.isVarOcc  name_occ = mkNameG_vName
	      | OccName.isTcOcc   name_occ = mkNameG_tcName
	      | otherwise 	           = pprPanic "DsMeta.globalVar" (ppr name)

lookupType :: Name 	-- Name of type constructor (e.g. TH.ExpQ)
	   -> DsM Type	-- The type
lookupType tc_name = do { tc <- dsLookupTyCon tc_name ;
		          return (mkTyConApp tc []) }

wrapGenSyns :: [GenSymBind] 
	    -> Core (TH.Q a) -> DsM (Core (TH.Q a))
-- wrapGenSyns [(nm1,id1), (nm2,id2)] y 
--	--> bindQ (gensym nm1) (\ id1 -> 
--	    bindQ (gensym nm2 (\ id2 -> 
--	    y))

wrapGenSyns binds body@(MkC b)
  = do  { var_ty <- lookupType nameTyConName
	; go var_ty binds }
  where
    [elt_ty] = tcTyConAppArgs (exprType b) 
	-- b :: Q a, so we can get the type 'a' by looking at the
	-- argument type. NB: this relies on Q being a data/newtype,
	-- not a type synonym

    go _ [] = return body
    go var_ty ((name,id) : binds)
      = do { MkC body'  <- go var_ty binds
	   ; lit_str    <- occNameLit name
	   ; gensym_app <- repGensym lit_str
	   ; repBindQ var_ty elt_ty 
		      gensym_app (MkC (Lam id body')) }

-- Just like wrapGenSym, but don't actually do the gensym
-- Instead use the existing name:
--	let x = "x" in ...
-- Only used for [Decl], and for the class ops in class 
-- and instance decls
wrapNongenSyms :: [GenSymBind] -> Core a -> DsM (Core a)
wrapNongenSyms binds (MkC body)
  = do { binds' <- mapM do_one binds ;
	 return (MkC (mkLets binds' body)) }
  where
    do_one (name,id) 
	= do { MkC lit_str <- occNameLit name
	     ; MkC var <- rep2 mkNameName [lit_str]
	     ; return (NonRec id var) }

occNameLit :: Name -> DsM (Core String)
occNameLit n = coreStringLit (occNameString (nameOccName n))


-- %*********************************************************************
-- %*									*
--		Constructing code
-- %*									*
-- %*********************************************************************

-----------------------------------------------------------------------------
-- PHANTOM TYPES for consistency. In order to make sure we do this correct 
-- we invent a new datatype which uses phantom types.

newtype Core a = MkC CoreExpr
unC :: Core a -> CoreExpr
unC (MkC x) = x

rep2 :: Name -> [ CoreExpr ] -> DsM (Core a)
rep2 n xs = do { id <- dsLookupGlobalId n
               ; return (MkC (foldl App (Var id) xs)) }

-- Then we make "repConstructors" which use the phantom types for each of the
-- smart constructors of the Meta.Meta datatypes.


-- %*********************************************************************
-- %*									*
--		The 'smart constructors'
-- %*									*
-- %*********************************************************************

--------------- Patterns -----------------
repPlit   :: Core TH.Lit -> DsM (Core TH.PatQ) 
repPlit (MkC l) = rep2 litPName [l]

repPvar :: Core TH.Name -> DsM (Core TH.PatQ)
repPvar (MkC s) = rep2 varPName [s]

repPtup :: Core [TH.PatQ] -> DsM (Core TH.PatQ)
repPtup (MkC ps) = rep2 tupPName [ps]

repPcon   :: Core TH.Name -> Core [TH.PatQ] -> DsM (Core TH.PatQ)
repPcon (MkC s) (MkC ps) = rep2 conPName [s, ps]

repPrec   :: Core TH.Name -> Core [(TH.Name,TH.PatQ)] -> DsM (Core TH.PatQ)
repPrec (MkC c) (MkC rps) = rep2 recPName [c,rps]

repPinfix :: Core TH.PatQ -> Core TH.Name -> Core TH.PatQ -> DsM (Core TH.PatQ)
repPinfix (MkC p1) (MkC n) (MkC p2) = rep2 infixPName [p1, n, p2]

repPtilde :: Core TH.PatQ -> DsM (Core TH.PatQ)
repPtilde (MkC p) = rep2 tildePName [p]

repPaspat :: Core TH.Name -> Core TH.PatQ -> DsM (Core TH.PatQ)
repPaspat (MkC s) (MkC p) = rep2 asPName [s, p]

repPwild  :: DsM (Core TH.PatQ)
repPwild = rep2 wildPName []

repPlist :: Core [TH.PatQ] -> DsM (Core TH.PatQ)
repPlist (MkC ps) = rep2 listPName [ps]

--------------- Expressions -----------------
repVarOrCon :: Name -> Core TH.Name -> DsM (Core TH.ExpQ)
repVarOrCon vc str | isDataOcc (nameOccName vc) = repCon str
	           | otherwise 		        = repVar str

repVar :: Core TH.Name -> DsM (Core TH.ExpQ)
repVar (MkC s) = rep2 varEName [s] 

repCon :: Core TH.Name -> DsM (Core TH.ExpQ)
repCon (MkC s) = rep2 conEName [s] 

repLit :: Core TH.Lit -> DsM (Core TH.ExpQ)
repLit (MkC c) = rep2 litEName [c] 

repApp :: Core TH.ExpQ -> Core TH.ExpQ -> DsM (Core TH.ExpQ)
repApp (MkC x) (MkC y) = rep2 appEName [x,y] 

repLam :: Core [TH.PatQ] -> Core TH.ExpQ -> DsM (Core TH.ExpQ)
repLam (MkC ps) (MkC e) = rep2 lamEName [ps, e]

repTup :: Core [TH.ExpQ] -> DsM (Core TH.ExpQ)
repTup (MkC es) = rep2 tupEName [es]

repCond :: Core TH.ExpQ -> Core TH.ExpQ -> Core TH.ExpQ -> DsM (Core TH.ExpQ)
repCond (MkC x) (MkC y) (MkC z) =  rep2 condEName [x,y,z] 

repLetE :: Core [TH.DecQ] -> Core TH.ExpQ -> DsM (Core TH.ExpQ)
repLetE (MkC ds) (MkC e) = rep2 letEName [ds, e] 

repCaseE :: Core TH.ExpQ -> Core [TH.MatchQ] -> DsM( Core TH.ExpQ)
repCaseE (MkC e) (MkC ms) = rep2 caseEName [e, ms]

repDoE :: Core [TH.StmtQ] -> DsM (Core TH.ExpQ)
repDoE (MkC ss) = rep2 doEName [ss]

repComp :: Core [TH.StmtQ] -> DsM (Core TH.ExpQ)
repComp (MkC ss) = rep2 compEName [ss]

repListExp :: Core [TH.ExpQ] -> DsM (Core TH.ExpQ)
repListExp (MkC es) = rep2 listEName [es]

repSigExp :: Core TH.ExpQ -> Core TH.TypeQ -> DsM (Core TH.ExpQ)
repSigExp (MkC e) (MkC t) = rep2 sigEName [e,t]

repRecCon :: Core TH.Name -> Core [TH.Q TH.FieldExp]-> DsM (Core TH.ExpQ)
repRecCon (MkC c) (MkC fs) = rep2 recConEName [c,fs]

repRecUpd :: Core TH.ExpQ -> Core [TH.Q TH.FieldExp] -> DsM (Core TH.ExpQ)
repRecUpd (MkC e) (MkC fs) = rep2 recUpdEName [e,fs]

repFieldExp :: Core TH.Name -> Core TH.ExpQ -> DsM (Core (TH.Q TH.FieldExp))
repFieldExp (MkC n) (MkC x) = rep2 fieldExpName [n,x]

repInfixApp :: Core TH.ExpQ -> Core TH.ExpQ -> Core TH.ExpQ -> DsM (Core TH.ExpQ)
repInfixApp (MkC x) (MkC y) (MkC z) = rep2 infixAppName [x,y,z]

repSectionL :: Core TH.ExpQ -> Core TH.ExpQ -> DsM (Core TH.ExpQ)
repSectionL (MkC x) (MkC y) = rep2 sectionLName [x,y]

repSectionR :: Core TH.ExpQ -> Core TH.ExpQ -> DsM (Core TH.ExpQ)
repSectionR (MkC x) (MkC y) = rep2 sectionRName [x,y]

------------ Right hand sides (guarded expressions) ----
repGuarded :: Core [TH.Q (TH.Guard, TH.Exp)] -> DsM (Core TH.BodyQ)
repGuarded (MkC pairs) = rep2 guardedBName [pairs]

repNormal :: Core TH.ExpQ -> DsM (Core TH.BodyQ)
repNormal (MkC e) = rep2 normalBName [e]

------------ Guards ----
repLNormalGE :: LHsExpr Name -> LHsExpr Name -> DsM (Core (TH.Q (TH.Guard, TH.Exp)))
repLNormalGE g e = do g' <- repLE g
                      e' <- repLE e
                      repNormalGE g' e'

repNormalGE :: Core TH.ExpQ -> Core TH.ExpQ -> DsM (Core (TH.Q (TH.Guard, TH.Exp)))
repNormalGE (MkC g) (MkC e) = rep2 normalGEName [g, e]

repPatGE :: Core [TH.StmtQ] -> Core TH.ExpQ -> DsM (Core (TH.Q (TH.Guard, TH.Exp)))
repPatGE (MkC ss) (MkC e) = rep2 patGEName [ss, e]

------------- Stmts -------------------
repBindSt :: Core TH.PatQ -> Core TH.ExpQ -> DsM (Core TH.StmtQ)
repBindSt (MkC p) (MkC e) = rep2 bindSName [p,e]

repLetSt :: Core [TH.DecQ] -> DsM (Core TH.StmtQ)
repLetSt (MkC ds) = rep2 letSName [ds]

repNoBindSt :: Core TH.ExpQ -> DsM (Core TH.StmtQ)
repNoBindSt (MkC e) = rep2 noBindSName [e]

-------------- Range (Arithmetic sequences) -----------
repFrom :: Core TH.ExpQ -> DsM (Core TH.ExpQ)
repFrom (MkC x) = rep2 fromEName [x]

repFromThen :: Core TH.ExpQ -> Core TH.ExpQ -> DsM (Core TH.ExpQ)
repFromThen (MkC x) (MkC y) = rep2 fromThenEName [x,y]

repFromTo :: Core TH.ExpQ -> Core TH.ExpQ -> DsM (Core TH.ExpQ)
repFromTo (MkC x) (MkC y) = rep2 fromToEName [x,y]

repFromThenTo :: Core TH.ExpQ -> Core TH.ExpQ -> Core TH.ExpQ -> DsM (Core TH.ExpQ)
repFromThenTo (MkC x) (MkC y) (MkC z) = rep2 fromThenToEName [x,y,z]

------------ Match and Clause Tuples -----------
repMatch :: Core TH.PatQ -> Core TH.BodyQ -> Core [TH.DecQ] -> DsM (Core TH.MatchQ)
repMatch (MkC p) (MkC bod) (MkC ds) = rep2 matchName [p, bod, ds]

repClause :: Core [TH.PatQ] -> Core TH.BodyQ -> Core [TH.DecQ] -> DsM (Core TH.ClauseQ)
repClause (MkC ps) (MkC bod) (MkC ds) = rep2 clauseName [ps, bod, ds]

-------------- Dec -----------------------------
repVal :: Core TH.PatQ -> Core TH.BodyQ -> Core [TH.DecQ] -> DsM (Core TH.DecQ)
repVal (MkC p) (MkC b) (MkC ds) = rep2 valDName [p, b, ds]

repFun :: Core TH.Name -> Core [TH.ClauseQ] -> DsM (Core TH.DecQ)  
repFun (MkC nm) (MkC b) = rep2 funDName [nm, b]

repData :: Core TH.CxtQ -> Core TH.Name -> Core [TH.Name] -> Core [TH.ConQ] -> Core [TH.Name] -> DsM (Core TH.DecQ)
repData (MkC cxt) (MkC nm) (MkC tvs) (MkC cons) (MkC derivs)
    = rep2 dataDName [cxt, nm, tvs, cons, derivs]

repNewtype :: Core TH.CxtQ -> Core TH.Name -> Core [TH.Name] -> Core TH.ConQ -> Core [TH.Name] -> DsM (Core TH.DecQ)
repNewtype (MkC cxt) (MkC nm) (MkC tvs) (MkC con) (MkC derivs)
    = rep2 newtypeDName [cxt, nm, tvs, con, derivs]

repTySyn :: Core TH.Name -> Core [TH.Name] -> Core TH.TypeQ -> DsM (Core TH.DecQ)
repTySyn (MkC nm) (MkC tvs) (MkC rhs) = rep2 tySynDName [nm, tvs, rhs]

repInst :: Core TH.CxtQ -> Core TH.TypeQ -> Core [TH.DecQ] -> DsM (Core TH.DecQ)
repInst (MkC cxt) (MkC ty) (MkC ds) = rep2 instanceDName [cxt, ty, ds]

repClass :: Core TH.CxtQ -> Core TH.Name -> Core [TH.Name] -> Core [TH.FunDep] -> Core [TH.DecQ] -> DsM (Core TH.DecQ)
repClass (MkC cxt) (MkC cls) (MkC tvs) (MkC fds) (MkC ds) = rep2 classDName [cxt, cls, tvs, fds, ds]

repFunDep :: Core [TH.Name] -> Core [TH.Name] -> DsM (Core TH.FunDep)
repFunDep (MkC xs) (MkC ys) = rep2 funDepName [xs, ys]

repProto :: Core TH.Name -> Core TH.TypeQ -> DsM (Core TH.DecQ)
repProto (MkC s) (MkC ty) = rep2 sigDName [s, ty]

repCtxt :: Core [TH.TypeQ] -> DsM (Core TH.CxtQ)
repCtxt (MkC tys) = rep2 cxtName [tys]

repConstr :: Core TH.Name -> HsConDeclDetails Name
          -> DsM (Core TH.ConQ)
repConstr con (PrefixCon ps)
    = do arg_tys  <- mapM repBangTy ps
         arg_tys1 <- coreList strictTypeQTyConName arg_tys
         rep2 normalCName [unC con, unC arg_tys1]
repConstr con (RecCon ips)
    = do arg_vs   <- mapM lookupLOcc (map cd_fld_name ips)
         arg_tys  <- mapM repBangTy (map cd_fld_type ips)
         arg_vtys <- zipWithM (\x y -> rep2 varStrictTypeName [unC x, unC y])
                              arg_vs arg_tys
         arg_vtys' <- coreList varStrictTypeQTyConName arg_vtys
         rep2 recCName [unC con, unC arg_vtys']
repConstr con (InfixCon st1 st2)
    = do arg1 <- repBangTy st1
         arg2 <- repBangTy st2
         rep2 infixCName [unC arg1, unC con, unC arg2]

------------ Types -------------------

repTForall :: Core [TH.Name] -> Core TH.CxtQ -> Core TH.TypeQ -> DsM (Core TH.TypeQ)
repTForall (MkC tvars) (MkC ctxt) (MkC ty)
    = rep2 forallTName [tvars, ctxt, ty]

repTvar :: Core TH.Name -> DsM (Core TH.TypeQ)
repTvar (MkC s) = rep2 varTName [s]

repTapp :: Core TH.TypeQ -> Core TH.TypeQ -> DsM (Core TH.TypeQ)
repTapp (MkC t1) (MkC t2) = rep2 appTName [t1,t2]

repTapps :: Core TH.TypeQ -> [Core TH.TypeQ] -> DsM (Core TH.TypeQ)
repTapps f []     = return f
repTapps f (t:ts) = do { f1 <- repTapp f t; repTapps f1 ts }

--------- Type constructors --------------

repNamedTyCon :: Core TH.Name -> DsM (Core TH.TypeQ)
repNamedTyCon (MkC s) = rep2 conTName [s]

repTupleTyCon :: Int -> DsM (Core TH.TypeQ)
-- Note: not Core Int; it's easier to be direct here
repTupleTyCon i = rep2 tupleTName [mkIntExprInt i]

repArrowTyCon :: DsM (Core TH.TypeQ)
repArrowTyCon = rep2 arrowTName []

repListTyCon :: DsM (Core TH.TypeQ)
repListTyCon = rep2 listTName []


----------------------------------------------------------
--		Literals

repLiteral :: HsLit -> DsM (Core TH.Lit)
repLiteral lit 
  = do lit' <- case lit of
                   HsIntPrim i    -> mk_integer i
                   HsWordPrim w   -> mk_integer w
                   HsInt i        -> mk_integer i
                   HsFloatPrim r  -> mk_rational r
                   HsDoublePrim r -> mk_rational r
                   _ -> return lit
       lit_expr <- dsLit lit'
       case mb_lit_name of
	  Just lit_name -> rep2 lit_name [lit_expr]
	  Nothing -> notHandled "Exotic literal" (ppr lit)
  where
    mb_lit_name = case lit of
		 HsInteger _ _  -> Just integerLName
		 HsInt     _    -> Just integerLName
		 HsIntPrim _    -> Just intPrimLName
		 HsWordPrim _   -> Just wordPrimLName
		 HsFloatPrim _  -> Just floatPrimLName
		 HsDoublePrim _ -> Just doublePrimLName
		 HsChar _       -> Just charLName
		 HsString _     -> Just stringLName
		 HsRat _ _      -> Just rationalLName
		 _              -> Nothing

mk_integer :: Integer -> DsM HsLit
mk_integer  i = do integer_ty <- lookupType integerTyConName
                   return $ HsInteger i integer_ty
mk_rational :: Rational -> DsM HsLit
mk_rational r = do rat_ty <- lookupType rationalTyConName
                   return $ HsRat r rat_ty
mk_string :: FastString -> DsM HsLit
mk_string s = return $ HsString s

repOverloadedLiteral :: HsOverLit Name -> DsM (Core TH.Lit)
repOverloadedLiteral (OverLit { ol_val = val})
  = do { lit <- mk_lit val; repLiteral lit }
	-- The type Rational will be in the environment, becuase 
	-- the smart constructor 'TH.Syntax.rationalL' uses it in its type,
	-- and rationalL is sucked in when any TH stuff is used

mk_lit :: OverLitVal -> DsM HsLit
mk_lit (HsIntegral i)   = mk_integer  i
mk_lit (HsFractional f) = mk_rational f
mk_lit (HsIsString s)   = mk_string   s
              
--------------- Miscellaneous -------------------

repGensym :: Core String -> DsM (Core (TH.Q TH.Name))
repGensym (MkC lit_str) = rep2 newNameName [lit_str]

repBindQ :: Type -> Type	-- a and b
	 -> Core (TH.Q a) -> Core (a -> TH.Q b) -> DsM (Core (TH.Q b))
repBindQ ty_a ty_b (MkC x) (MkC y) 
  = rep2 bindQName [Type ty_a, Type ty_b, x, y] 

repSequenceQ :: Type -> Core [TH.Q a] -> DsM (Core (TH.Q [a]))
repSequenceQ ty_a (MkC list)
  = rep2 sequenceQName [Type ty_a, list]

------------ Lists and Tuples -------------------
-- turn a list of patterns into a single pattern matching a list

coreList :: Name	-- Of the TyCon of the element type
	 -> [Core a] -> DsM (Core [a])
coreList tc_name es 
  = do { elt_ty <- lookupType tc_name; return (coreList' elt_ty es) }

coreList' :: Type 	-- The element type
	  -> [Core a] -> Core [a]
coreList' elt_ty es = MkC (mkListExpr elt_ty (map unC es ))

nonEmptyCoreList :: [Core a] -> Core [a]
  -- The list must be non-empty so we can get the element type
  -- Otherwise use coreList
nonEmptyCoreList [] 	      = panic "coreList: empty argument"
nonEmptyCoreList xs@(MkC x:_) = MkC (mkListExpr (exprType x) (map unC xs))

coreStringLit :: String -> DsM (Core String)
coreStringLit s = do { z <- mkStringExpr s; return(MkC z) }

coreIntLit :: Int -> DsM (Core Int)
coreIntLit i = return (MkC (mkIntExprInt i))

coreVar :: Id -> Core TH.Name	-- The Id has type Name
coreVar id = MkC (Var id)

----------------- Failure -----------------------
notHandled :: String -> SDoc -> DsM a
notHandled what doc = failWithDs msg
  where
    msg = hang (text what <+> ptext (sLit "not (yet) handled by Template Haskell")) 
	     2 doc


-- %************************************************************************
-- %*									*
--		The known-key names for Template Haskell
-- %*									*
-- %************************************************************************

-- To add a name, do three things
-- 
--  1) Allocate a key
--  2) Make a "Name"
--  3) Add the name to knownKeyNames

templateHaskellNames :: [Name]
-- The names that are implicitly mentioned by ``bracket''
-- Should stay in sync with the import list of DsMeta

templateHaskellNames = [
    returnQName, bindQName, sequenceQName, newNameName, liftName,
    mkNameName, mkNameG_vName, mkNameG_dName, mkNameG_tcName, mkNameLName, 

    -- Lit
    charLName, stringLName, integerLName, intPrimLName, wordPrimLName,
    floatPrimLName, doublePrimLName, rationalLName,
    -- Pat
    litPName, varPName, tupPName, conPName, tildePName, infixPName,
    asPName, wildPName, recPName, listPName, sigPName,
    -- FieldPat
    fieldPatName,
    -- Match
    matchName,
    -- Clause
    clauseName,
    -- Exp
    varEName, conEName, litEName, appEName, infixEName,
    infixAppName, sectionLName, sectionRName, lamEName, tupEName,
    condEName, letEName, caseEName, doEName, compEName,
    fromEName, fromThenEName, fromToEName, fromThenToEName,
    listEName, sigEName, recConEName, recUpdEName,
    -- FieldExp
    fieldExpName,
    -- Body
    guardedBName, normalBName,
    -- Guard
    normalGEName, patGEName,
    -- Stmt
    bindSName, letSName, noBindSName, parSName,
    -- Dec
    funDName, valDName, dataDName, newtypeDName, tySynDName,
    classDName, instanceDName, sigDName, forImpDName,
    -- Cxt
    cxtName,
    -- Strict
    isStrictName, notStrictName,
    -- Con
    normalCName, recCName, infixCName, forallCName,
    -- StrictType
    strictTypeName,
    -- VarStrictType
    varStrictTypeName,
    -- Type
    forallTName, varTName, conTName, appTName,
    tupleTName, arrowTName, listTName,
    -- Callconv
    cCallName, stdCallName,
    -- Safety
    unsafeName,
    safeName,
    threadsafeName,
    -- FunDep
    funDepName,

    -- And the tycons
    qTyConName, nameTyConName, patTyConName, fieldPatTyConName, matchQTyConName,
    clauseQTyConName, expQTyConName, fieldExpTyConName, stmtQTyConName,
    decQTyConName, conQTyConName, strictTypeQTyConName,
    varStrictTypeQTyConName, typeQTyConName, expTyConName, decTyConName,
    typeTyConName, matchTyConName, clauseTyConName, patQTyConName,
    fieldPatQTyConName, fieldExpQTyConName, funDepTyConName,

    -- Quasiquoting
    quoteExpName, quotePatName]

thSyn, thLib, qqLib :: Module
thSyn = mkTHModule (fsLit "Language.Haskell.TH.Syntax")
thLib = mkTHModule (fsLit "Language.Haskell.TH.Lib")
qqLib = mkTHModule (fsLit "Language.Haskell.TH.Quote")

mkTHModule :: FastString -> Module
mkTHModule m = mkModule thPackageId (mkModuleNameFS m)

libFun, libTc, thFun, thTc, qqFun :: FastString -> Unique -> Name
libFun = mk_known_key_name OccName.varName thLib
libTc  = mk_known_key_name OccName.tcName  thLib
thFun  = mk_known_key_name OccName.varName thSyn
thTc   = mk_known_key_name OccName.tcName  thSyn
qqFun  = mk_known_key_name OccName.varName qqLib

-------------------- TH.Syntax -----------------------
qTyConName, nameTyConName, fieldExpTyConName, patTyConName,
    fieldPatTyConName, expTyConName, decTyConName, typeTyConName,
    matchTyConName, clauseTyConName, funDepTyConName :: Name
qTyConName        = thTc (fsLit "Q")            qTyConKey
nameTyConName     = thTc (fsLit "Name")         nameTyConKey
fieldExpTyConName = thTc (fsLit "FieldExp")     fieldExpTyConKey
patTyConName      = thTc (fsLit "Pat")          patTyConKey
fieldPatTyConName = thTc (fsLit "FieldPat")     fieldPatTyConKey
expTyConName      = thTc (fsLit "Exp")          expTyConKey
decTyConName      = thTc (fsLit "Dec")          decTyConKey
typeTyConName     = thTc (fsLit "Type")         typeTyConKey
matchTyConName    = thTc (fsLit "Match")        matchTyConKey
clauseTyConName   = thTc (fsLit "Clause")       clauseTyConKey
funDepTyConName   = thTc (fsLit "FunDep")       funDepTyConKey

returnQName, bindQName, sequenceQName, newNameName, liftName,
    mkNameName, mkNameG_vName, mkNameG_dName, mkNameG_tcName,
    mkNameLName :: Name
returnQName   = thFun (fsLit "returnQ")   returnQIdKey
bindQName     = thFun (fsLit "bindQ")     bindQIdKey
sequenceQName = thFun (fsLit "sequenceQ") sequenceQIdKey
newNameName    = thFun (fsLit "newName")   newNameIdKey
liftName      = thFun (fsLit "lift")      liftIdKey
mkNameName     = thFun (fsLit "mkName")     mkNameIdKey
mkNameG_vName  = thFun (fsLit "mkNameG_v")  mkNameG_vIdKey
mkNameG_dName  = thFun (fsLit "mkNameG_d")  mkNameG_dIdKey
mkNameG_tcName = thFun (fsLit "mkNameG_tc") mkNameG_tcIdKey
mkNameLName    = thFun (fsLit "mkNameL")    mkNameLIdKey


-------------------- TH.Lib -----------------------
-- data Lit = ...
charLName, stringLName, integerLName, intPrimLName, wordPrimLName,
    floatPrimLName, doublePrimLName, rationalLName :: Name
charLName       = libFun (fsLit "charL")       charLIdKey
stringLName     = libFun (fsLit "stringL")     stringLIdKey
integerLName    = libFun (fsLit "integerL")    integerLIdKey
intPrimLName    = libFun (fsLit "intPrimL")    intPrimLIdKey
wordPrimLName   = libFun (fsLit "wordPrimL")   wordPrimLIdKey
floatPrimLName  = libFun (fsLit "floatPrimL")  floatPrimLIdKey
doublePrimLName = libFun (fsLit "doublePrimL") doublePrimLIdKey
rationalLName   = libFun (fsLit "rationalL")     rationalLIdKey

-- data Pat = ...
litPName, varPName, tupPName, conPName, infixPName, tildePName,
    asPName, wildPName, recPName, listPName, sigPName :: Name
litPName   = libFun (fsLit "litP")   litPIdKey
varPName   = libFun (fsLit "varP")   varPIdKey
tupPName   = libFun (fsLit "tupP")   tupPIdKey
conPName   = libFun (fsLit "conP")   conPIdKey
infixPName = libFun (fsLit "infixP") infixPIdKey
tildePName = libFun (fsLit "tildeP") tildePIdKey
asPName    = libFun (fsLit "asP")    asPIdKey
wildPName  = libFun (fsLit "wildP")  wildPIdKey
recPName   = libFun (fsLit "recP")   recPIdKey
listPName  = libFun (fsLit "listP")  listPIdKey
sigPName   = libFun (fsLit "sigP")   sigPIdKey

-- type FieldPat = ...
fieldPatName :: Name
fieldPatName = libFun (fsLit "fieldPat") fieldPatIdKey

-- data Match = ...
matchName :: Name
matchName = libFun (fsLit "match") matchIdKey

-- data Clause = ...
clauseName :: Name
clauseName = libFun (fsLit "clause") clauseIdKey

-- data Exp = ...
varEName, conEName, litEName, appEName, infixEName, infixAppName,
    sectionLName, sectionRName, lamEName, tupEName, condEName,
    letEName, caseEName, doEName, compEName :: Name
varEName        = libFun (fsLit "varE")        varEIdKey
conEName        = libFun (fsLit "conE")        conEIdKey
litEName        = libFun (fsLit "litE")        litEIdKey
appEName        = libFun (fsLit "appE")        appEIdKey
infixEName      = libFun (fsLit "infixE")      infixEIdKey
infixAppName    = libFun (fsLit "infixApp")    infixAppIdKey
sectionLName    = libFun (fsLit "sectionL")    sectionLIdKey
sectionRName    = libFun (fsLit "sectionR")    sectionRIdKey
lamEName        = libFun (fsLit "lamE")        lamEIdKey
tupEName        = libFun (fsLit "tupE")        tupEIdKey
condEName       = libFun (fsLit "condE")       condEIdKey
letEName        = libFun (fsLit "letE")        letEIdKey
caseEName       = libFun (fsLit "caseE")       caseEIdKey
doEName         = libFun (fsLit "doE")         doEIdKey
compEName       = libFun (fsLit "compE")       compEIdKey
-- ArithSeq skips a level
fromEName, fromThenEName, fromToEName, fromThenToEName :: Name
fromEName       = libFun (fsLit "fromE")       fromEIdKey
fromThenEName   = libFun (fsLit "fromThenE")   fromThenEIdKey
fromToEName     = libFun (fsLit "fromToE")     fromToEIdKey
fromThenToEName = libFun (fsLit "fromThenToE") fromThenToEIdKey
-- end ArithSeq
listEName, sigEName, recConEName, recUpdEName :: Name
listEName       = libFun (fsLit "listE")       listEIdKey
sigEName        = libFun (fsLit "sigE")        sigEIdKey
recConEName     = libFun (fsLit "recConE")     recConEIdKey
recUpdEName     = libFun (fsLit "recUpdE")     recUpdEIdKey

-- type FieldExp = ...
fieldExpName :: Name
fieldExpName = libFun (fsLit "fieldExp") fieldExpIdKey

-- data Body = ...
guardedBName, normalBName :: Name
guardedBName = libFun (fsLit "guardedB") guardedBIdKey
normalBName  = libFun (fsLit "normalB")  normalBIdKey

-- data Guard = ...
normalGEName, patGEName :: Name
normalGEName = libFun (fsLit "normalGE") normalGEIdKey
patGEName    = libFun (fsLit "patGE")    patGEIdKey

-- data Stmt = ...
bindSName, letSName, noBindSName, parSName :: Name
bindSName   = libFun (fsLit "bindS")   bindSIdKey
letSName    = libFun (fsLit "letS")    letSIdKey
noBindSName = libFun (fsLit "noBindS") noBindSIdKey
parSName    = libFun (fsLit "parS")    parSIdKey

-- data Dec = ...
funDName, valDName, dataDName, newtypeDName, tySynDName, classDName,
    instanceDName, sigDName, forImpDName :: Name
funDName      = libFun (fsLit "funD")      funDIdKey
valDName      = libFun (fsLit "valD")      valDIdKey
dataDName     = libFun (fsLit "dataD")     dataDIdKey
newtypeDName  = libFun (fsLit "newtypeD")  newtypeDIdKey
tySynDName    = libFun (fsLit "tySynD")    tySynDIdKey
classDName    = libFun (fsLit "classD")    classDIdKey
instanceDName = libFun (fsLit "instanceD") instanceDIdKey
sigDName      = libFun (fsLit "sigD")      sigDIdKey
forImpDName   = libFun (fsLit "forImpD")   forImpDIdKey

-- type Ctxt = ...
cxtName :: Name
cxtName = libFun (fsLit "cxt") cxtIdKey

-- data Strict = ...
isStrictName, notStrictName :: Name
isStrictName      = libFun  (fsLit "isStrict")      isStrictKey
notStrictName     = libFun  (fsLit "notStrict")     notStrictKey

-- data Con = ...
normalCName, recCName, infixCName, forallCName :: Name
normalCName = libFun (fsLit "normalC") normalCIdKey
recCName    = libFun (fsLit "recC")    recCIdKey
infixCName  = libFun (fsLit "infixC")  infixCIdKey
forallCName  = libFun (fsLit "forallC")  forallCIdKey

-- type StrictType = ...
strictTypeName :: Name
strictTypeName    = libFun  (fsLit "strictType")    strictTKey

-- type VarStrictType = ...
varStrictTypeName :: Name
varStrictTypeName = libFun  (fsLit "varStrictType") varStrictTKey

-- data Type = ...
forallTName, varTName, conTName, tupleTName, arrowTName,
    listTName, appTName :: Name
forallTName = libFun (fsLit "forallT") forallTIdKey
varTName    = libFun (fsLit "varT")    varTIdKey
conTName    = libFun (fsLit "conT")    conTIdKey
tupleTName  = libFun (fsLit "tupleT") tupleTIdKey
arrowTName  = libFun (fsLit "arrowT") arrowTIdKey
listTName   = libFun (fsLit "listT")  listTIdKey
appTName    = libFun (fsLit "appT")    appTIdKey

-- data Callconv = ...
cCallName, stdCallName :: Name
cCallName = libFun (fsLit "cCall") cCallIdKey
stdCallName = libFun (fsLit "stdCall") stdCallIdKey

-- data Safety = ...
unsafeName, safeName, threadsafeName :: Name
unsafeName     = libFun (fsLit "unsafe") unsafeIdKey
safeName       = libFun (fsLit "safe") safeIdKey
threadsafeName = libFun (fsLit "threadsafe") threadsafeIdKey

-- data FunDep = ...
funDepName :: Name
funDepName     = libFun (fsLit "funDep") funDepIdKey

matchQTyConName, clauseQTyConName, expQTyConName, stmtQTyConName,
    decQTyConName, conQTyConName, strictTypeQTyConName,
    varStrictTypeQTyConName, typeQTyConName, fieldExpQTyConName,
    patQTyConName, fieldPatQTyConName :: Name
matchQTyConName         = libTc (fsLit "MatchQ")        matchQTyConKey
clauseQTyConName        = libTc (fsLit "ClauseQ")       clauseQTyConKey
expQTyConName           = libTc (fsLit "ExpQ")          expQTyConKey
stmtQTyConName          = libTc (fsLit "StmtQ")         stmtQTyConKey
decQTyConName           = libTc (fsLit "DecQ")          decQTyConKey
conQTyConName           = libTc (fsLit "ConQ")          conQTyConKey
strictTypeQTyConName    = libTc (fsLit "StrictTypeQ")    strictTypeQTyConKey
varStrictTypeQTyConName = libTc (fsLit "VarStrictTypeQ") varStrictTypeQTyConKey
typeQTyConName          = libTc (fsLit "TypeQ")          typeQTyConKey
fieldExpQTyConName      = libTc (fsLit "FieldExpQ")      fieldExpQTyConKey
patQTyConName           = libTc (fsLit "PatQ")           patQTyConKey
fieldPatQTyConName      = libTc (fsLit "FieldPatQ")      fieldPatQTyConKey

-- quasiquoting
quoteExpName, quotePatName :: Name
quoteExpName	    = qqFun (fsLit "quoteExp") quoteExpKey
quotePatName	    = qqFun (fsLit "quotePat") quotePatKey

-- TyConUniques available: 100-129
-- Check in PrelNames if you want to change this

expTyConKey, matchTyConKey, clauseTyConKey, qTyConKey, expQTyConKey,
    decQTyConKey, patTyConKey, matchQTyConKey, clauseQTyConKey,
    stmtQTyConKey, conQTyConKey, typeQTyConKey, typeTyConKey,
    decTyConKey, varStrictTypeQTyConKey, strictTypeQTyConKey,
    fieldExpTyConKey, fieldPatTyConKey, nameTyConKey, patQTyConKey,
    fieldPatQTyConKey, fieldExpQTyConKey, funDepTyConKey :: Unique
expTyConKey             = mkPreludeTyConUnique 100
matchTyConKey           = mkPreludeTyConUnique 101
clauseTyConKey          = mkPreludeTyConUnique 102
qTyConKey               = mkPreludeTyConUnique 103
expQTyConKey            = mkPreludeTyConUnique 104
decQTyConKey            = mkPreludeTyConUnique 105
patTyConKey             = mkPreludeTyConUnique 106
matchQTyConKey          = mkPreludeTyConUnique 107
clauseQTyConKey         = mkPreludeTyConUnique 108
stmtQTyConKey           = mkPreludeTyConUnique 109
conQTyConKey            = mkPreludeTyConUnique 110
typeQTyConKey           = mkPreludeTyConUnique 111
typeTyConKey            = mkPreludeTyConUnique 112
decTyConKey             = mkPreludeTyConUnique 113
varStrictTypeQTyConKey  = mkPreludeTyConUnique 114
strictTypeQTyConKey     = mkPreludeTyConUnique 115
fieldExpTyConKey        = mkPreludeTyConUnique 116
fieldPatTyConKey        = mkPreludeTyConUnique 117
nameTyConKey            = mkPreludeTyConUnique 118
patQTyConKey            = mkPreludeTyConUnique 119
fieldPatQTyConKey       = mkPreludeTyConUnique 120
fieldExpQTyConKey       = mkPreludeTyConUnique 121
funDepTyConKey          = mkPreludeTyConUnique 122

-- IdUniques available: 200-399
-- If you want to change this, make sure you check in PrelNames

returnQIdKey, bindQIdKey, sequenceQIdKey, liftIdKey, newNameIdKey,
    mkNameIdKey, mkNameG_vIdKey, mkNameG_dIdKey, mkNameG_tcIdKey,
    mkNameLIdKey :: Unique
returnQIdKey        = mkPreludeMiscIdUnique 200
bindQIdKey          = mkPreludeMiscIdUnique 201
sequenceQIdKey      = mkPreludeMiscIdUnique 202
liftIdKey           = mkPreludeMiscIdUnique 203
newNameIdKey         = mkPreludeMiscIdUnique 204
mkNameIdKey          = mkPreludeMiscIdUnique 205
mkNameG_vIdKey       = mkPreludeMiscIdUnique 206
mkNameG_dIdKey       = mkPreludeMiscIdUnique 207
mkNameG_tcIdKey      = mkPreludeMiscIdUnique 208
mkNameLIdKey         = mkPreludeMiscIdUnique 209


-- data Lit = ...
charLIdKey, stringLIdKey, integerLIdKey, intPrimLIdKey, wordPrimLIdKey,
    floatPrimLIdKey, doublePrimLIdKey, rationalLIdKey :: Unique
charLIdKey        = mkPreludeMiscIdUnique 210
stringLIdKey      = mkPreludeMiscIdUnique 211
integerLIdKey     = mkPreludeMiscIdUnique 212
intPrimLIdKey     = mkPreludeMiscIdUnique 213
wordPrimLIdKey    = mkPreludeMiscIdUnique 214
floatPrimLIdKey   = mkPreludeMiscIdUnique 215
doublePrimLIdKey  = mkPreludeMiscIdUnique 216
rationalLIdKey    = mkPreludeMiscIdUnique 217

-- data Pat = ...
litPIdKey, varPIdKey, tupPIdKey, conPIdKey, infixPIdKey, tildePIdKey,
    asPIdKey, wildPIdKey, recPIdKey, listPIdKey, sigPIdKey :: Unique
litPIdKey         = mkPreludeMiscIdUnique 220
varPIdKey         = mkPreludeMiscIdUnique 221
tupPIdKey         = mkPreludeMiscIdUnique 222
conPIdKey         = mkPreludeMiscIdUnique 223
infixPIdKey       = mkPreludeMiscIdUnique 312
tildePIdKey       = mkPreludeMiscIdUnique 224
asPIdKey          = mkPreludeMiscIdUnique 225
wildPIdKey        = mkPreludeMiscIdUnique 226
recPIdKey         = mkPreludeMiscIdUnique 227
listPIdKey        = mkPreludeMiscIdUnique 228
sigPIdKey         = mkPreludeMiscIdUnique 229

-- type FieldPat = ...
fieldPatIdKey :: Unique
fieldPatIdKey       = mkPreludeMiscIdUnique 230

-- data Match = ...
matchIdKey :: Unique
matchIdKey          = mkPreludeMiscIdUnique 231

-- data Clause = ...
clauseIdKey :: Unique
clauseIdKey         = mkPreludeMiscIdUnique 232

-- data Exp = ...
varEIdKey, conEIdKey, litEIdKey, appEIdKey, infixEIdKey, infixAppIdKey,
    sectionLIdKey, sectionRIdKey, lamEIdKey, tupEIdKey, condEIdKey,
    letEIdKey, caseEIdKey, doEIdKey, compEIdKey,
    fromEIdKey, fromThenEIdKey, fromToEIdKey, fromThenToEIdKey,
    listEIdKey, sigEIdKey, recConEIdKey, recUpdEIdKey :: Unique
varEIdKey         = mkPreludeMiscIdUnique 240
conEIdKey         = mkPreludeMiscIdUnique 241
litEIdKey         = mkPreludeMiscIdUnique 242
appEIdKey         = mkPreludeMiscIdUnique 243
infixEIdKey       = mkPreludeMiscIdUnique 244
infixAppIdKey       = mkPreludeMiscIdUnique 245
sectionLIdKey       = mkPreludeMiscIdUnique 246
sectionRIdKey       = mkPreludeMiscIdUnique 247
lamEIdKey         = mkPreludeMiscIdUnique 248
tupEIdKey         = mkPreludeMiscIdUnique 249
condEIdKey        = mkPreludeMiscIdUnique 250
letEIdKey         = mkPreludeMiscIdUnique 251
caseEIdKey        = mkPreludeMiscIdUnique 252
doEIdKey          = mkPreludeMiscIdUnique 253
compEIdKey        = mkPreludeMiscIdUnique 254
fromEIdKey        = mkPreludeMiscIdUnique 255
fromThenEIdKey    = mkPreludeMiscIdUnique 256
fromToEIdKey      = mkPreludeMiscIdUnique 257
fromThenToEIdKey  = mkPreludeMiscIdUnique 258
listEIdKey        = mkPreludeMiscIdUnique 259
sigEIdKey         = mkPreludeMiscIdUnique 260
recConEIdKey      = mkPreludeMiscIdUnique 261
recUpdEIdKey      = mkPreludeMiscIdUnique 262

-- type FieldExp = ...
fieldExpIdKey :: Unique
fieldExpIdKey       = mkPreludeMiscIdUnique 265

-- data Body = ...
guardedBIdKey, normalBIdKey :: Unique
guardedBIdKey     = mkPreludeMiscIdUnique 266
normalBIdKey      = mkPreludeMiscIdUnique 267

-- data Guard = ...
normalGEIdKey, patGEIdKey :: Unique
normalGEIdKey     = mkPreludeMiscIdUnique 310
patGEIdKey        = mkPreludeMiscIdUnique 311

-- data Stmt = ...
bindSIdKey, letSIdKey, noBindSIdKey, parSIdKey :: Unique
bindSIdKey       = mkPreludeMiscIdUnique 268
letSIdKey        = mkPreludeMiscIdUnique 269
noBindSIdKey     = mkPreludeMiscIdUnique 270
parSIdKey        = mkPreludeMiscIdUnique 271

-- data Dec = ...
funDIdKey, valDIdKey, dataDIdKey, newtypeDIdKey, tySynDIdKey,
    classDIdKey, instanceDIdKey, sigDIdKey, forImpDIdKey :: Unique
funDIdKey         = mkPreludeMiscIdUnique 272
valDIdKey         = mkPreludeMiscIdUnique 273
dataDIdKey        = mkPreludeMiscIdUnique 274
newtypeDIdKey     = mkPreludeMiscIdUnique 275
tySynDIdKey       = mkPreludeMiscIdUnique 276
classDIdKey       = mkPreludeMiscIdUnique 277
instanceDIdKey    = mkPreludeMiscIdUnique 278
sigDIdKey         = mkPreludeMiscIdUnique 279
forImpDIdKey      = mkPreludeMiscIdUnique 297

-- type Cxt = ...
cxtIdKey :: Unique
cxtIdKey            = mkPreludeMiscIdUnique 280

-- data Strict = ...
isStrictKey, notStrictKey :: Unique
isStrictKey         = mkPreludeMiscIdUnique 281
notStrictKey        = mkPreludeMiscIdUnique 282

-- data Con = ...
normalCIdKey, recCIdKey, infixCIdKey, forallCIdKey :: Unique
normalCIdKey      = mkPreludeMiscIdUnique 283
recCIdKey         = mkPreludeMiscIdUnique 284
infixCIdKey       = mkPreludeMiscIdUnique 285
forallCIdKey      = mkPreludeMiscIdUnique 288

-- type StrictType = ...
strictTKey :: Unique
strictTKey        = mkPreludeMiscIdUnique 286

-- type VarStrictType = ...
varStrictTKey :: Unique
varStrictTKey     = mkPreludeMiscIdUnique 287

-- data Type = ...
forallTIdKey, varTIdKey, conTIdKey, tupleTIdKey, arrowTIdKey,
    listTIdKey, appTIdKey :: Unique
forallTIdKey      = mkPreludeMiscIdUnique 290
varTIdKey         = mkPreludeMiscIdUnique 291
conTIdKey         = mkPreludeMiscIdUnique 292
tupleTIdKey       = mkPreludeMiscIdUnique 294
arrowTIdKey       = mkPreludeMiscIdUnique 295
listTIdKey        = mkPreludeMiscIdUnique 296
appTIdKey         = mkPreludeMiscIdUnique 293

-- data Callconv = ...
cCallIdKey, stdCallIdKey :: Unique
cCallIdKey      = mkPreludeMiscIdUnique 300
stdCallIdKey    = mkPreludeMiscIdUnique 301

-- data Safety = ...
unsafeIdKey, safeIdKey, threadsafeIdKey :: Unique
unsafeIdKey     = mkPreludeMiscIdUnique 305
safeIdKey       = mkPreludeMiscIdUnique 306
threadsafeIdKey = mkPreludeMiscIdUnique 307

-- data FunDep = ...
funDepIdKey :: Unique
funDepIdKey = mkPreludeMiscIdUnique 320

-- quasiquoting
quoteExpKey, quotePatKey :: Unique
quoteExpKey = mkPreludeMiscIdUnique 321
quotePatKey = mkPreludeMiscIdUnique 322

