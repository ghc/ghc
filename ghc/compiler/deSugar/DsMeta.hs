-----------------------------------------------------------------------------
-- The purpose of this module is to transform an HsExpr into a CoreExpr which
-- when evaluated, returns a (Meta.Q Meta.Exp) computation analogous to the
-- input HsExpr. We do this in the DsM monad, which supplies access to
-- CoreExpr's of the "smart constructors" of the Meta.Exp datatype.
-----------------------------------------------------------------------------


module DsMeta( dsBracket ) where

#include "HsVersions.h"

import {-# SOURCE #-}	DsExpr ( dsExpr )

import DsUtils    ( mkListExpr, mkStringLit, mkCoreTup,
		    mkIntExpr, mkCharExpr )
import DsMonad

import qualified Language.Haskell.THSyntax as M

import HsSyn  	  ( Pat(..), HsExpr(..), Stmt(..), HsLit(..), HsOverLit(..),
		    Match(..), GRHSs(..), GRHS(..), HsBracket(..),
                    HsStmtContext(ListComp,DoExpr), ArithSeqInfo(..),
		    HsBinds(..), MonoBinds(..), HsConDetails(..),
		    HsDecl(..), TyClDecl(..), ForeignDecl(..),
		    PendingSplice,
		    placeHolderType, tyClDeclNames,
		    collectHsBinders, 
		    collectPatBinders, collectPatsBinders
		  )

import Name       ( Name, nameOccName, nameModule )
import OccName	  ( isDataOcc, occNameUserString )
import Module	  ( moduleUserString )
import PrelNames  ( intLName,charLName,
                    plitName, pvarName, ptupName, pconName,
                    ptildeName, paspatName, pwildName, 
                    varName, conName, litName, appName, lamName,
                    tupName, doEName, compName, 
                    listExpName, condName, letEName, caseEName,
                    infixAppName, guardedName, normalName,
		    bindStName, letStName, noBindStName, 
		    fromName, fromThenName, fromToName, fromThenToName,
		    funName, valName, matchName, clauseName,
		    liftName, gensymName, bindQName, 
		    matTyConName, expTyConName, clsTyConName,
		    pattTyConName, exprTyConName, declTyConName
                  )
                  
import Id         ( Id )
import NameEnv
import Type       ( Type, mkGenTyConApp )
import TysWiredIn ( stringTy )
import CoreSyn
import CoreUtils  ( exprType )
import Panic	  ( panic )

import Outputable
import FastString	( mkFastString )
 
-----------------------------------------------------------------------------
dsBracket :: HsBracket Name -> [PendingSplice] -> DsM CoreExpr
-- Returns a CoreExpr of type M.Expr
-- The quoted thing is parameterised over Name, even though it has
-- been type checked.  We don't want all those type decorations!

dsBracket (ExpBr e) splices
  = dsExtendMetaEnv new_bit (repE e)	`thenDs` \ (MkC new_e) ->
    returnDs new_e
  where
    new_bit = mkNameEnv [(n, Splice e) | (n,e) <- splices]


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


-----------------------------------------------------------------------------      
-- 				repD

{-
repDs :: [HsDecl Name] -> DsM (Core [M.Decl])
repDs decls
  = do { ds' <- mapM repD ds ;
	 coreList declTyConName ds' }

repD :: HsDecl Name -> DsM (Core M.Decl)
repD (TyClD (TyData { tcdND = DataType, tcdCtxt = [], 
		      tcdName = tc, tcdTyVars = tvs, 
		      tcdCons = cons, tcdDerivs = mb_derivs })) 
 = do { tc1  <- localVar tc ;
	cons1 <- mapM repCon cons ;
	tvs1  <- repTvs tvs ;
	cons2 <- coreList consTyConName cons1 ;
	derivs1 <- repDerivs mb_derivs ;
	derivs2 <- coreList stringTyConName derivs1 ;
	repData tc1 tvs1 cons2 derivs2 }

repD (TyClD (ClassD { tcdCtxt = cxt, tcdName = cls, 
		      tcdTyVars = tvs, tcdFDs = [], 
		      tcdSigs = sigs, tcdMeths = Just decls 
	}))
 = do { cls1 <- localVar cls ;
	tvs1 <- repTvs tvs ;
	cxt1 <- repCtxt cxt ;
	sigs1 <- repSigs sigs ;
	repClass cxt1 cls1 tvs1 sigs1 }

repD (InstD (InstDecl ty binds _ _ loc))
	-- Ignore user pragmas for now
 = do { cls1 <- localVar cls ;
	cxt1 <- repCtxt cxt ;
	tys1 <- repTys tys ;
	binds1 <- repMonoBind binds ;
	binds2 <- coreList declTyConName binds1 ;
	repInst ... binds2 }
 where
   (tvs, cxt, cls, tys) = splitHsInstDeclTy ty

-- Un-handled cases
repD d = do { dsWarn (hang (ptext SLIT("Cannot desugar this Template Haskell declaration:"))
		     4  (ppr d)) ;
	      return (ValD EmptyBinds)	-- A sort of empty decl
	 }

repTvs :: [HsTyVarBndr Name] -> DsM (Core [String])
repTvs tvs = do { tvs1 <- mapM (localVar . hsTyVarName) tvs ;
		  coreList stringTyConName tvs1 } 

repCtxt :: HsContext Name -> DsM (Core M.Ctxt)
repCtxt ctxt 
 = do { 

repTy :: HsType Name -> DsM (Core M.Type)
repTy ty@(HsForAllTy _ cxt ty)
  = pprPanic "repTy" (ppr ty)

repTy (HsTyVar tv)
  = do { tv1 <- localVar tv ; repTvar tv1 }

repTy (HsAppTy f a) = do { f1 <- repTy f ; a1 <- repTy a ; repTapp f1 a2 }
repTy (HsFunTy f a) = do { f1 <- repTy f ; a1 <- repTy a ; tcon <- repArrowTyCon ; repTapps tcon [f1,a1] }
repTy (HsListTy t)  = do { t1 <- repTy t ; list <- repListTyCon ; repTapp tcon t1 }

repTy (HsTupleTy tc tys)
  = do 
repTy (HsOpTy ty1 HsArrow ty2) 	  = repTy (HsFunTy ty1 ty2)
repTy (HsOpTy ty1 (HsTyOp n)   	  = repTy ((HsTyVar n `HsAppTy` ty1) `HsAppTy` ty2)
repTy (HsParTy t)  	       	  = repTy t
repTy (HsPredTy (HsClassP c tys)) = repTy (foldl HsApp (HsTyVar c) tys)

  | HsTupleTy		HsTupCon
			[HsType name]	-- Element types (length gives arity)

  | HsKindSig		(HsType name)	-- (ty :: kind)
			Kind		-- A type with a kind signature
-}

-----------------------------------------------------------------------------      
-- Using the phantom type constructors "repConstructor" we define repE
-- This ensures we keep the types of the CoreExpr objects we build are
-- consistent with their real types.

repEs :: [HsExpr Name] -> DsM (Core [M.Expr])
repEs es = do { es'  <- mapM repE es ;
		coreList exprTyConName es' }

repE :: HsExpr Name -> DsM (Core M.Expr)
repE (HsVar x)
  = do { mb_val <- dsLookupMetaEnv x 
       ; case mb_val of
	  Nothing	   -> do { str <- globalVar x
				 ; if constructor x then
					repCon str
				   else
					repVar str }
	  Just (Bound y)   -> repVar (coreVar y)
	  Just (Splice e)  -> do { e' <- dsExpr e
				 ; return (MkC e') } }

repE (HsIPVar x)    = panic "Can't represent implicit parameters"
repE (HsLit l)      = do { a <- repLiteral l;           repLit a }
repE (HsOverLit l)  = do { a <- repOverloadedLiteral l; repLit a }

repE (HsSplice n e) 
  = do { mb_val <- dsLookupMetaEnv n
       ; case mb_val of
	     Just (Splice e) -> do { e' <- dsExpr e
				   ; return (MkC e') }
	     other	     -> pprPanic "HsSplice" (ppr n) }
			

repE (HsLam m)      = repLambda m
repE (HsApp x y)    = do {a <- repE x; b <- repE y; repApp a b}
repE (NegApp x nm)  = panic "No negate yet"
repE (SectionL x y) = do { a <- repE x; b <- repE y; repSectionL a b } 
repE (SectionR x y) = do { a <- repE x; b <- repE y; repSectionR a b } 

repE (OpApp e1 (HsVar op) fix e2) = 
     do { arg1 <- repE e1; 
	  arg2 <- repE e2; 
 	  mb_val <- dsLookupMetaEnv op;
          the_op <- case mb_val of {
			Nothing        -> globalVar op ;
		 	Just (Bound x) -> return (coreVar x) ;
			other	       -> pprPanic "repE:OpApp" (ppr op) } ;
	  repInfixApp arg1 the_op arg2 } 

repE (HsCase e ms loc)
  = do { arg <- repE e
       ; ms2 <- mapM repMatchTup ms
       ; repCaseE arg (nonEmptyCoreList ms2) }

-- 	I havn't got the types here right yet
repE (HsDo DoExpr sts _ ty loc)      = do { (ss,zs) <- repSts sts; 
					    e       <- repDoE (nonEmptyCoreList zs);
					    combine expTyConName ss e }
repE (HsDo ListComp sts _ ty loc) = do { (ss,zs) <- repSts sts; 
					  e       <- repComp (nonEmptyCoreList zs);
					  combine expTyConName ss e }

repE (ArithSeqIn (From e)) 		= do { ds1 <- repE e; repFrom ds1 }
repE (ArithSeqIn (FromThen e1 e2))      = do { ds1 <- repE e1; ds2 <- repE e2; 
					       repFromThen ds1 ds2 }
repE (ArithSeqIn (FromTo   e1 e2))      = do { ds1 <- repE e1; ds2 <- repE e2; 
					       repFromTo   ds1 ds2 }
repE (ArithSeqIn (FromThenTo e1 e2 e3)) = do { ds1 <- repE e1; ds2 <- repE e2; 
					       ds3 <- repE e3; repFromThenTo ds1 ds2 ds3 }

repE (HsIf x y z loc)
  = do { a <- repE x; b <- repE y; c <- repE z; repCond a b c } 

repE (HsLet bs e) = 
   do { (ss,ds) <- repDecs bs
      ; e2 <- addBinds ss (repE e)
      ; z <- repLetE ds e2
      ; combine expTyConName ss z }
repE (HsWith _ _ _) = panic "No with for implicit parameters yet"
repE (ExplicitList ty es) = 
     do { xs <- repEs es; repListExp xs } 
repE (ExplicitTuple es boxed) = 
     do { xs <- repEs es; repTup xs }
repE (ExplicitPArr ty es) = panic "No parallel arrays yet"
repE (RecordConOut _ _ _) = panic "No record construction yet"
repE (RecordUpdOut _ _ _ _) = panic "No record update yet"
repE (ExprWithTySig e ty) = panic "No expressions with type signatures yet"


-----------------------------------------------------------------------------
-- Building representations of auxillary structures like Match, Clause, Stmt, 

repMatchTup ::  Match Name -> DsM (Core M.Mtch) 
repMatchTup (Match [p] ty (GRHSs guards wheres ty2)) = 
  do { ss1 <- mkGenSyms (collectPatBinders p) 
     ; addBinds ss1 $ do {
     ; p1 <- repP p
     ; (ss2,ds) <- repDecs wheres
     ; addBinds ss2 $ do {
     ; gs    <- repGuards guards
     ; match <- repMatch p1 gs ds
     ; combine matTyConName (ss1++ss2) match }}}

repClauseTup ::  Match Name -> DsM (Core M.Clse)
repClauseTup (Match ps ty (GRHSs guards wheres ty2)) = 
  do { ss1 <- mkGenSyms (collectPatsBinders ps) 
     ; addBinds ss1 $ do {
       ps1 <- repPs ps
     ; (ss2,ds) <- repDecs wheres
     ; addBinds ss2 $ do {
       gs <- repGuards guards
     ; clause <- repClause ps1 gs ds
     ; combine clsTyConName (ss1++ss2) clause }}}

repGuards ::  [GRHS Name] ->  DsM (Core M.Rihs)
repGuards [GRHS[ResultStmt e loc] loc2] 
  = do {a <- repE e; repNormal a }
repGuards other 
  = do { zs <- mapM process other; 
	 repGuarded (nonEmptyCoreList (map corePair zs)) }
  where 
    process (GRHS [ExprStmt e1 ty loc,ResultStmt e2 _] _)
           = do { x <- repE e1; y <- repE e2; return (x, y) }
    process other = panic "Non Haskell 98 guarded body"


-----------------------------------------------------------------------------
-- Representing Stmt's is tricky, especially if bound variables
-- shaddow each other. Consider:  [| do { x <- f 1; x <- f x; g x } |]
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

repSts :: [Stmt Name] -> DsM ([GenSymBind], [Core M.Stmt])
repSts [ResultStmt e loc] = 
   do { a <- repE e
      ; e1 <- repNoBindSt a
      ; return ([], [e1]) }
repSts (BindStmt p e loc : ss) =
   do { e2 <- repE e 
      ; ss1 <- mkGenSyms (collectPatBinders p) 
      ; addBinds ss1 $ do {
      ; p1 <- repP p; 
      ; (ss2,zs) <- repSts ss
      ; z <- repBindSt p1 e2
      ; return (ss1++ss2, z : zs) }}
repSts (LetStmt bs : ss) =
   do { (ss1,ds) <- repDecs bs
      ; z <- repLetSt ds
      ; (ss2,zs) <- addBinds ss1 (repSts ss)
      ; return (ss1++ss2, z : zs) } 
repSts (ExprStmt e ty loc : ss) =       
   do { e2 <- repE e
      ; z <- repNoBindSt e2 
      ; (ss2,zs) <- repSts ss
      ; return (ss2, z : zs) }
repSts other = panic "Exotic Stmt in meta brackets"      



repDecs :: HsBinds Name -> DsM ([GenSymBind], Core [M.Decl]) 
repDecs decs
 = do { let { bndrs = collectHsBinders decs } ;
	ss <- mkGenSyms bndrs ;
	core <- addBinds ss (rep_decs decs) ;
	core_list <- coreList declTyConName core ;
	return (ss, core_list) }

rep_decs :: HsBinds Name -> DsM [Core M.Decl] 
rep_decs EmptyBinds = return []
rep_decs (ThenBinds x y)
 = do { core1 <- rep_decs x
      ; core2 <- rep_decs y
      ; return (core1 ++ core2) }
rep_decs (MonoBind bs sigs _)
 = do { core1 <- repMonoBind bs
      ;	core2 <- rep_sigs sigs
      ;	return (core1 ++ core2) }

rep_sigs sigs = return []	-- Incomplete!

repMonoBind :: MonoBinds Name -> DsM [Core M.Decl]
repMonoBind EmptyMonoBinds     = return []
repMonoBind (AndMonoBinds x y) = do { x1 <- repMonoBind x; 
					y1 <- repMonoBind y; 
					return (x1 ++ y1) }

-- Note GHC treats declarations of a variable (not a pattern) 
-- e.g.  x = g 5 as a Fun MonoBinds. This is indicated by a single match 
-- with an empty list of patterns
repMonoBind (FunMonoBind fn infx [Match [] ty (GRHSs guards wheres ty2)] loc) 
 = do { (ss,wherecore) <- repDecs wheres
	; guardcore <- addBinds ss (repGuards guards)
	; fn' <- lookupBinder fn
	; p   <- repPvar fn'
	; ans <- repVal p guardcore wherecore
	; return [ans] }

repMonoBind (FunMonoBind fn infx ms loc)
 =   do { ms1 <- mapM repClauseTup ms
	; fn' <- lookupBinder fn
        ; ans <- repFun fn' (nonEmptyCoreList ms1)
        ; return [ans] }

repMonoBind (PatMonoBind pat (GRHSs guards wheres ty2) loc)
 =   do { patcore <- repP pat 
        ; (ss,wherecore) <- repDecs wheres
	; guardcore <- addBinds ss (repGuards guards)
        ; ans <- repVal patcore guardcore wherecore
        ; return [ans] }

repMonoBind (VarMonoBind v e)  
 =   do { v' <- lookupBinder v 
	; e2 <- repE e
        ; x <- repNormal e2
        ; patcore <- repPvar v'
	; empty_decls <- coreList declTyConName [] 
        ; ans <- repVal patcore x empty_decls
        ; return [ans] }

-----------------------------------------------------------------------------
-- Since everything in a MonoBind is mutually recursive we need rename all
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
--	Gathering binders

hsDeclsBinders :: [HsDecl Name] -> [Name]
hsDeclsBinders ds = concat (map hsDeclBinders ds)

hsDeclBinders (ValD b)  = collectHsBinders b
hsDeclBinders (TyClD d) = map fst (tyClDeclNames d)
hsDeclBinders (ForD d)  = forDeclBinders d
hsDeclBinders other     = []

forDeclBinders (ForeignImport n _ _ _ _) = [n]
forDeclBinders other			 = []


-----------------------------------------------------------------------------
-- GHC seems to allow a more general form of lambda abstraction than specified
-- by Haskell 98. In particular it allows guarded lambda's like : 
-- (\  x | even x -> 0 | odd x -> 1) at the moment we can't represent this in
-- Haskell Template's Meta.Exp type so we punt if it isn't a simple thing like
-- (\ p1 .. pn -> exp) by causing an error.  

repLambda :: Match Name -> DsM (Core M.Expr)
repLambda (Match ps _ (GRHSs [GRHS [ResultStmt e _ ] _ ] 
		             EmptyBinds _))
 = do { let bndrs = collectPatsBinders ps ;
      ; ss <- mkGenSyms bndrs
      ; lam <- addBinds ss (
		do { xs <- repPs ps; body <- repE e; repLam xs body })
      ; combine expTyConName ss lam }

repLambda z = panic "Can't represent a guarded lambda in Template Haskell"  

  
-----------------------------------------------------------------------------
--			repP
-- repP deals with patterns.  It assumes that we have already
-- walked over the pattern(s) once to collect the binders, and 
-- have extended the environment.  So every pattern-bound 
-- variable should already appear in the environment.

-- Process a list of patterns
repPs :: [Pat Name] -> DsM (Core [M.Patt])
repPs ps = do { ps' <- mapM repP ps ;
		coreList pattTyConName ps' }

repP :: Pat Name -> DsM (Core M.Patt)
repP (WildPat _)     = repPwild 
repP (LitPat l)      = do { l2 <- repLiteral l; repPlit l2 }
repP (VarPat x)      = do { x' <- lookupBinder x; repPvar x' }
repP (LazyPat p)     = do { p1 <- repP p; repPtilde p1 }
repP (AsPat x p)     = do { x' <- lookupBinder x; p1 <- repP p; repPaspat x' p1 }
repP (ParPat p)      = repP p 
repP (ListPat ps _)  = repListPat ps
repP (TuplePat ps _) = do { qs <- repPs ps; repPtup qs }
repP (ConPatIn dc details)
 = do { con_str <- globalVar dc
      ; case details of
         PrefixCon ps   -> do { qs <- repPs ps; repPcon con_str qs }
         RecCon pairs   -> error "No records in template haskell yet"
         InfixCon p1 p2 -> do { qs <- repPs [p1,p2]; repPcon con_str qs }
   }
repP other = panic "Exotic pattern inside meta brackets"

repListPat :: [Pat Name] -> DsM (Core M.Patt)     
repListPat [] 	  = do { nil_con <- coreStringLit "[]"
		       ; nil_args <- coreList pattTyConName [] 
	               ; repPcon nil_con nil_args }
repListPat (p:ps) = do { p2 <- repP p 
		       ; ps2 <- repListPat ps
		       ; cons_con <- coreStringLit ":"
		       ; repPcon cons_con (nonEmptyCoreList [p2,ps2]) }


----------------------------------------------------------
--		Literals

repLiteral :: HsLit -> DsM (Core M.Lit)
repLiteral (HsInt i)  = rep2 intLName [mkIntExpr i]
repLiteral (HsChar c) = rep2 charLName [mkCharExpr c]
repLiteral x = panic "trying to represent exotic literal"

repOverloadedLiteral :: HsOverLit -> DsM(Core M.Lit)
repOverloadedLiteral (HsIntegral i _)   = rep2 intLName [mkIntExpr i]
repOverloadedLiteral (HsFractional f _) = panic "Cant do fractional literals yet"

              
----------------------------------------------------------
--	The meta-environment

type GenSymBind = (Name, Id)	-- Gensym the string and bind it to the Id
				-- I.e.		(x, x_id) means
				--	let x_id = gensym "x" in ...

addBinds :: [GenSymBind] -> DsM a -> DsM a
addBinds bs m = dsExtendMetaEnv (mkNameEnv [(n,Bound id) | (n,id) <- bs]) m

lookupBinder :: Name -> DsM (Core String)
lookupBinder n 
  = do { mb_val <- dsLookupMetaEnv n;
	 case mb_val of
	    Just (Bound id) -> return (MkC (Var id))
	    other	    -> pprPanic "Failed binder lookup:" (ppr n) }

mkGenSym :: Name -> DsM GenSymBind
mkGenSym nm = do { id <- newUniqueId nm stringTy; return (nm,id) }

mkGenSyms :: [Name] -> DsM [GenSymBind]
mkGenSyms ns = mapM mkGenSym ns
	     
lookupType :: Name 	-- Name of type constructor (e.g. M.Expr)
	   -> DsM Type	-- The type
lookupType tc_name = do { tc <- dsLookupTyCon tc_name ;
		          return (mkGenTyConApp tc []) }

-- combine[ x1 <- e1, x2 <- e2 ] y 
--	--> bindQ e1 (\ x1 -> bindQ e2 (\ x2 -> y))

combine :: Name 	-- Name of the type (consructor) for 'a'
	-> [GenSymBind] 
	-> Core (M.Q a) -> DsM (Core (M.Q a))
combine tc_name binds body@(MkC b)
  = do { elt_ty <- lookupType tc_name
       ; go elt_ty binds }
  where
    go elt_ty [] = return body
    go elt_ty ((name,id) : binds)
      = do { MkC body'  <- go elt_ty binds
	   ; lit_str    <- localVar name
	   ; gensym_app <- repGensym lit_str
	   ; repBindQ stringTy elt_ty 
		      gensym_app (MkC (Lam id body')) }

constructor :: Name -> Bool
constructor x = isDataOcc (nameOccName x)

void = placeHolderType

string :: String -> HsExpr Id
string s = HsLit (HsString (mkFastString s))


-- %*********************************************************************
-- %*									*
--		Constructing code
-- %*									*
-- %*********************************************************************

-----------------------------------------------------------------------------
-- PHANTOM TYPES for consistency. In order to make sure we do this correct 
-- we invent a new datatype which uses phantom types.

newtype Core a = MkC CoreExpr
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
repPlit   :: Core M.Lit -> DsM (Core M.Patt) 
repPlit (MkC l) = rep2 plitName [l]

repPvar :: Core String -> DsM (Core M.Patt)
repPvar (MkC s) = rep2 pvarName [s]

repPtup :: Core [M.Patt] -> DsM (Core M.Patt)
repPtup (MkC ps) = rep2 ptupName [ps]

repPcon   :: Core String -> Core [M.Patt] -> DsM (Core M.Patt)
repPcon (MkC s) (MkC ps) = rep2 pconName [s, ps]

repPtilde :: Core M.Patt -> DsM (Core M.Patt)
repPtilde (MkC p) = rep2 ptildeName [p]

repPaspat :: Core String -> Core M.Patt -> DsM (Core M.Patt)
repPaspat (MkC s) (MkC p) = rep2 paspatName [s, p]

repPwild  :: DsM (Core M.Patt)
repPwild = rep2 pwildName []

--------------- Expressions -----------------
repVar :: Core String -> DsM (Core M.Expr)
repVar (MkC s) = rep2 varName [s] 

repCon :: Core String -> DsM (Core M.Expr)
repCon (MkC s) = rep2 conName [s] 

repLit :: Core M.Lit -> DsM (Core M.Expr)
repLit (MkC c) = rep2 litName [c] 

repApp :: Core M.Expr -> Core M.Expr -> DsM (Core M.Expr)
repApp (MkC x) (MkC y) = rep2 appName [x,y] 

repLam :: Core [M.Patt] -> Core M.Expr -> DsM (Core M.Expr)
repLam (MkC ps) (MkC e) = rep2 lamName [ps, e]

repTup :: Core [M.Expr] -> DsM (Core M.Expr)
repTup (MkC es) = rep2 tupName [es]

repCond :: Core M.Expr -> Core M.Expr -> Core M.Expr -> DsM (Core M.Expr)
repCond (MkC x) (MkC y) (MkC z) =  rep2 condName [x,y,z] 

repLetE :: Core [M.Decl] -> Core M.Expr -> DsM (Core M.Expr)
repLetE (MkC ds) (MkC e) = rep2 letEName [ds, e] 

repCaseE :: Core M.Expr -> Core [M.Mtch] -> DsM( Core M.Expr)
repCaseE (MkC e) (MkC ms) = rep2 caseEName [e, ms]

repDoE :: Core [M.Stmt] -> DsM (Core M.Expr)
repDoE (MkC ss) = rep2 doEName [ss]

repComp :: Core [M.Stmt] -> DsM (Core M.Expr)
repComp (MkC ss) = rep2 compName [ss]

repListExp :: Core [M.Expr] -> DsM (Core M.Expr)
repListExp (MkC es) = rep2 listExpName [es]

repInfixApp :: Core M.Expr -> Core String -> Core M.Expr -> DsM (Core M.Expr)
repInfixApp (MkC x) (MkC y) (MkC z) = rep2 infixAppName [x,y,z]

repSectionL :: Core M.Expr -> Core M.Expr -> DsM (Core M.Expr)
repSectionL (MkC x) (MkC y) = rep2 infixAppName [x,y]

repSectionR :: Core M.Expr -> Core M.Expr -> DsM (Core M.Expr)
repSectionR (MkC x) (MkC y) = rep2 infixAppName [x,y]

------------ Right hand sides (guarded expressions) ----
repGuarded :: Core [(M.Expr, M.Expr)] -> DsM (Core M.Rihs)
repGuarded (MkC pairs) = rep2 guardedName [pairs]

repNormal :: Core M.Expr -> DsM (Core M.Rihs)
repNormal (MkC e) = rep2 normalName [e]

------------- Statements -------------------
repBindSt :: Core M.Patt -> Core M.Expr -> DsM (Core M.Stmt)
repBindSt (MkC p) (MkC e) = rep2 bindStName [p,e]

repLetSt :: Core [M.Decl] -> DsM (Core M.Stmt)
repLetSt (MkC ds) = rep2 letStName [ds]

repNoBindSt :: Core M.Expr -> DsM (Core M.Stmt)
repNoBindSt (MkC e) = rep2 noBindStName [e]

-------------- DotDot (Arithmetic sequences) -----------
repFrom :: Core M.Expr -> DsM (Core M.Expr)
repFrom (MkC x) = rep2 fromName [x]

repFromThen :: Core M.Expr -> Core M.Expr -> DsM (Core M.Expr)
repFromThen (MkC x) (MkC y) = rep2 fromThenName [x,y]

repFromTo :: Core M.Expr -> Core M.Expr -> DsM (Core M.Expr)
repFromTo (MkC x) (MkC y) = rep2 fromToName [x,y]

repFromThenTo :: Core M.Expr -> Core M.Expr -> Core M.Expr -> DsM (Core M.Expr)
repFromThenTo (MkC x) (MkC y) (MkC z) = rep2 fromThenToName [x,y,z]

------------ Match and Clause Tuples -----------
repMatch :: Core M.Patt -> Core M.Rihs -> Core [M.Decl] -> DsM (Core M.Mtch)
repMatch (MkC p) (MkC bod) (MkC ds) = rep2 matchName [p, bod, ds]

repClause :: Core [M.Patt] -> Core M.Rihs -> Core [M.Decl] -> DsM (Core M.Clse)
repClause (MkC ps) (MkC bod) (MkC ds) = rep2 clauseName [ps, bod, ds]

-------------- Dec -----------------------------
repVal :: Core M.Patt -> Core M.Rihs -> Core [M.Decl] -> DsM (Core M.Decl)
repVal (MkC p) (MkC b) (MkC ds) = rep2 valName [p, b, ds]

repFun :: Core String -> Core [M.Clse] -> DsM (Core M.Decl)  
repFun (MkC nm) (MkC b) = rep2 funName [nm, b]

{-
repData :: Core String -> Core [String] -> Core [M.Cons] -> Core [String] -> DsM (Core M.Decl)
repData (MkC nm) (MkC tvs) (MkC cons) (MkC derivs) = rep2 dataDName [nm, tvs, cons, derivs]

repInst :: Core M.Ctxt -> Core M.Type -> Core [M.Decl]
repInst (MkC cxt) (MkC ty) (Core ds) = rep2 instanceDName [cxt, ty, ds]

repClass :: Core M.Ctxt -> Core String -> Core [String] -> Core [M.Decl] -> DsM (Core M.Decl)
repClass (MkC cxt) (MkC cls) (MkC tvs) (MkC ds) = rep2 classDName [cxt, cls, tvs, ds]

repProto :: Core String -> Core M.Type -> DsM (Core M.Decl)
repProto (MkC s) (MkC ty) = rep2 protoName [s, ty]

------------ Types -------------------

repTvar :: Core String -> DsM (Core M.Type)
repTvar (MkC s) = rep2 tvarName [s]

repTapp :: Core M.Type -> Core M.Type -> DsM (Core M.Type)
repTapp (MkC t1) (MkC t2) = rep2 tappName [t1,t2]

repTapps :: Core M.Type -> [Core M.Type] -> DsM (Core M.Type)
repTapps f []     = return f
repTapps f (t:ts) = do { f1 <- repTapp f t; repTapps f1 ts }


repNamedTyCon :: Core String -> DsM (Core M.Type)
repNamedTyCon (MkC s) = rep2 namedTyConName [s]

repTupleTyCon :: Core Int -> DsM (Core M.Tag)
repTupleTyCon (MkC i) = rep2 tupleTyConName [i]

repArrowTyCon :: DsM (Core M.Type)
repArrowTyCon = rep2 arrowTyConName []

repListTyCon :: DsM (Core M.Tag)
repListTyCon = rep2 listTyConName []

-}

--------------- Miscellaneous -------------------

repLift :: Core e -> DsM (Core M.Expr)
repLift (MkC x) = rep2 liftName [x]

repGensym :: Core String -> DsM (Core (M.Q String))
repGensym (MkC lit_str) = rep2 gensymName [lit_str]

repBindQ :: Type -> Type	-- a and b
	 -> Core (M.Q a) -> Core (a -> M.Q b) -> DsM (Core (M.Q b))
repBindQ ty_a ty_b (MkC x) (MkC y) 
  = rep2 bindQName [Type ty_a, Type ty_b, x, y] 

------------ Lists and Tuples -------------------
-- turn a list of patterns into a single pattern matching a list

coreList :: Name	-- Of the TyCon of the element type
	 -> [Core a] -> DsM (Core [a])
coreList tc_name es 
  = do { elt_ty <- lookupType tc_name
       ; let es' = map unC es 
       ; return (MkC (mkListExpr elt_ty es')) }

nonEmptyCoreList :: [Core a] -> Core [a]
  -- The list must be non-empty so we can get the element type
  -- Otherwise use coreList
nonEmptyCoreList [] 	      = panic "coreList: empty argument"
nonEmptyCoreList xs@(MkC x:_) = MkC (mkListExpr (exprType x) (map unC xs))

corePair :: (Core a, Core b) -> Core (a,b)
corePair (MkC x, MkC y) = MkC (mkCoreTup [x,y])

globalVar :: Name -> DsM (Core String)
globalVar n = coreStringLit (name_mod ++ ":" ++ name_occ)
 	    where
	      name_mod = moduleUserString (nameModule n)
	      name_occ = occNameUserString (nameOccName n)

localVar :: Name -> DsM (Core String)
localVar n = coreStringLit (occNameUserString (nameOccName n))

coreStringLit :: String -> DsM (Core String)
coreStringLit s = do { z <- mkStringLit s; return(MkC z) }

coreVar :: Id -> Core String	-- The Id has type String
coreVar id = MkC (Var id)
