%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

This module converts Template Haskell syntax into HsSyn


\begin{code}
module Convert( convertToHsExpr, convertToHsDecls, convertToHsType, thRdrName ) where

#include "HsVersions.h"

import Language.Haskell.TH as TH hiding (sigP)
import Language.Haskell.TH.Syntax as TH

import HsSyn as Hs
import qualified Class (FunDep)
import RdrName	( RdrName, mkRdrUnqual, mkRdrQual, mkOrig, getRdrName, nameRdrName )
import qualified Name	( Name, mkInternalName, getName )
import Module   ( ModuleName, mkModuleName, mkModule )
import RdrHsSyn	( mkClassDecl, mkTyData )
import qualified OccName
import PackageConfig    ( PackageId, stringToPackageId )
import OccName	( startsVarId, startsVarSym, startsConId, startsConSym,
		  pprNameSpace )
import SrcLoc	( Located(..), SrcSpan )
import Type	( Type )
import TysWiredIn ( unitTyCon, tupleTyCon, tupleCon, trueDataCon, nilDataCon, consDataCon )
import BasicTypes( Boxity(..) ) 
import ForeignCall ( Safety(..), CCallConv(..), CCallTarget(..),
                     CExportSpec(..)) 
import Char 	( isAscii, isAlphaNum, isAlpha )
import List	( partition )
import Unique	( Unique, mkUniqueGrimily )
import ErrUtils ( Message )
import GLAEXTS	( Int(..), Int# )
import SrcLoc	( noSrcLoc )
import Bag	( listToBag )
import FastString
import Outputable



-------------------------------------------------------------------
--		The external interface

convertToHsDecls :: SrcSpan -> [TH.Dec] -> Either Message [LHsDecl RdrName]
convertToHsDecls loc ds = initCvt loc (mapM cvtTop ds)

convertToHsExpr :: SrcSpan -> TH.Exp -> Either Message (LHsExpr RdrName)
convertToHsExpr loc e 
  = case initCvt loc (cvtl e) of
	Left msg  -> Left (msg $$ (ptext SLIT("When converting TH expression")
				    <+> text (show e)))
	Right res -> Right res

convertToHsType :: SrcSpan -> TH.Type -> Either Message (LHsType RdrName)
convertToHsType loc t = initCvt loc (cvtType t)


-------------------------------------------------------------------
newtype CvtM a = CvtM { unCvtM :: SrcSpan -> Either Message a }
	-- Push down the source location;
	-- Can fail, with a single error message

-- NB: If the conversion succeeds with (Right x), there should 
--     be no exception values hiding in x
-- Reason: so a (head []) in TH code doesn't subsequently
-- 	   make GHC crash when it tries to walk the generated tree

-- Use the loc everywhere, for lack of anything better
-- In particular, we want it on binding locations, so that variables bound in
-- the spliced-in declarations get a location that at least relates to the splice point

instance Monad CvtM where
  return x       = CvtM $ \loc -> Right x
  (CvtM m) >>= k = CvtM $ \loc -> case m loc of
				    Left err -> Left err
				    Right v  -> unCvtM (k v) loc

initCvt :: SrcSpan -> CvtM a -> Either Message a
initCvt loc (CvtM m) = m loc

force :: a -> CvtM a
force a = a `seq` return a

failWith :: Message -> CvtM a
failWith m = CvtM (\loc -> Left full_msg)
   where
     full_msg = m $$ ptext SLIT("When splicing generated code into the program")

returnL :: a -> CvtM (Located a)
returnL x = CvtM (\loc -> Right (L loc x))

wrapL :: CvtM a -> CvtM (Located a)
wrapL (CvtM m) = CvtM (\loc -> case m loc of
			  Left err -> Left err
			  Right v  -> Right (L loc v))

-------------------------------------------------------------------
cvtTop :: TH.Dec -> CvtM (LHsDecl RdrName)
cvtTop d@(TH.ValD _ _ _) = do { L loc d' <- cvtBind d; return (L loc $ Hs.ValD d') }
cvtTop d@(TH.FunD _ _)   = do { L loc d' <- cvtBind d; return (L loc $ Hs.ValD d') }
cvtTop (TH.SigD nm typ)  = do  { nm' <- vNameL nm
				; ty' <- cvtType typ
				; returnL $ Hs.SigD (TypeSig nm' ty') }

cvtTop (TySynD tc tvs rhs)
  = do	{ tc' <- tconNameL tc
	; tvs' <- cvtTvs tvs
	; rhs' <- cvtType rhs
	; returnL $ TyClD (TySynonym tc' tvs' rhs') }

cvtTop (DataD ctxt tc tvs constrs derivs)
  = do	{ stuff <- cvt_tycl_hdr ctxt tc tvs
	; cons' <- mapM cvtConstr constrs
	; derivs' <- cvtDerivs derivs
	; returnL $ TyClD (mkTyData DataType stuff Nothing cons' derivs') }


cvtTop (NewtypeD ctxt tc tvs constr derivs)
  = do	{ stuff <- cvt_tycl_hdr ctxt tc tvs
	; con' <- cvtConstr constr
	; derivs' <- cvtDerivs derivs
	; returnL $ TyClD (mkTyData NewType stuff Nothing [con'] derivs') }

cvtTop (ClassD ctxt cl tvs fds decs)
  = do	{ stuff <- cvt_tycl_hdr ctxt cl tvs
	; fds'  <- mapM cvt_fundep fds
	; (binds', sigs') <- cvtBindsAndSigs decs
	; returnL $ TyClD $ mkClassDecl stuff fds' sigs' binds' }

cvtTop (InstanceD tys ty decs)
  = do 	{ (binds', sigs') <- cvtBindsAndSigs decs
	; ctxt' <- cvtContext tys
	; L loc pred' <- cvtPred ty
	; inst_ty' <- returnL $ mkImplicitHsForAllTy ctxt' (L loc (HsPredTy pred'))
	; returnL $ InstD (InstDecl inst_ty' binds' sigs') }

cvtTop (ForeignD ford) = do { ford' <- cvtForD ford; returnL $ ForD ford' }

cvt_tycl_hdr cxt tc tvs
  = do	{ cxt' <- cvtContext cxt
	; tc'  <- tconNameL tc
	; tvs' <- cvtTvs tvs
	; return (cxt', tc', tvs') }

---------------------------------------------------
-- 	Data types
-- Can't handle GADTs yet
---------------------------------------------------

cvtConstr (NormalC c strtys)
  = do	{ c'   <- cNameL c 
	; cxt' <- returnL []
	; tys' <- mapM cvt_arg strtys
	; returnL $ ConDecl c' Explicit noExistentials cxt' (PrefixCon tys') ResTyH98 }

cvtConstr (RecC c varstrtys)
  = do 	{ c'    <- cNameL c 
	; cxt'  <- returnL []
	; args' <- mapM cvt_id_arg varstrtys
	; returnL $ ConDecl c' Explicit noExistentials cxt' (RecCon args') ResTyH98 }

cvtConstr (InfixC st1 c st2)
  = do 	{ c' <- cNameL c 
	; cxt' <- returnL []
	; st1' <- cvt_arg st1
	; st2' <- cvt_arg st2
	; returnL $ ConDecl c' Explicit noExistentials cxt' (InfixCon st1' st2') ResTyH98 }

cvtConstr (ForallC tvs ctxt (ForallC tvs' ctxt' con'))
  = cvtConstr (ForallC (tvs ++ tvs') (ctxt ++ ctxt') con')

cvtConstr (ForallC tvs ctxt con)
  = do	{ L _ con' <- cvtConstr con
	; tvs'  <- cvtTvs tvs
	; ctxt' <- cvtContext ctxt
	; case con' of
	    ConDecl l _ [] (L _ []) x ResTyH98
	      -> returnL $ ConDecl l Explicit tvs' ctxt' x ResTyH98
	    c -> panic "ForallC: Can't happen" }

cvt_arg (IsStrict, ty)  = do { ty' <- cvtType ty; returnL $ HsBangTy HsStrict ty' }
cvt_arg (NotStrict, ty) = cvtType ty

cvt_id_arg (i, str, ty) = do { i' <- vNameL i
			     ; ty' <- cvt_arg (str,ty)
			     ; return (i', ty') }

cvtDerivs [] = return Nothing
cvtDerivs cs = do { cs' <- mapM cvt_one cs
		  ; return (Just cs') }
	where
	  cvt_one c = do { c' <- tconName c
			 ; returnL $ HsPredTy $ HsClassP c' [] }

cvt_fundep :: FunDep -> CvtM (Located (Class.FunDep RdrName))
cvt_fundep (FunDep xs ys) = do { xs' <- mapM tName xs; ys' <- mapM tName ys; returnL (xs', ys') }

noExistentials = []

------------------------------------------
-- 	Foreign declarations
------------------------------------------

cvtForD :: Foreign -> CvtM (ForeignDecl RdrName)
cvtForD (ImportF callconv safety from nm ty)
  | Just (c_header, cis) <- parse_ccall_impent (TH.nameBase nm) from
  = do	{ nm' <- vNameL nm
	; ty' <- cvtType ty
	; let i = CImport (cvt_conv callconv) safety' c_header nilFS cis
	; return $ ForeignImport nm' ty' i False }

  | otherwise
  = failWith $ text (show from)<+> ptext SLIT("is not a valid ccall impent")
  where 
    safety' = case safety of
                     Unsafe     -> PlayRisky
                     Safe       -> PlaySafe False
                     Threadsafe -> PlaySafe True

cvtForD (ExportF callconv as nm ty)
  = do	{ nm' <- vNameL nm
	; ty' <- cvtType ty
	; let e = CExport (CExportStatic (mkFastString as) (cvt_conv callconv))
 	; return $ ForeignExport nm' ty' e False }

cvt_conv CCall   = CCallConv
cvt_conv StdCall = StdCallConv

parse_ccall_impent :: String -> String -> Maybe (FastString, CImportSpec)
parse_ccall_impent nm s
 = case lex_ccall_impent s of
       Just ["dynamic"] -> Just (nilFS, CFunction DynamicTarget)
       Just ["wrapper"] -> Just (nilFS, CWrapper)
       Just ("static":ts) -> parse_ccall_impent_static nm ts
       Just ts -> parse_ccall_impent_static nm ts
       Nothing -> Nothing

parse_ccall_impent_static :: String
                          -> [String]
                          -> Maybe (FastString, CImportSpec)
parse_ccall_impent_static nm ts
 = let ts' = case ts of
                 [       "&", cid] -> [       cid]
                 [fname, "&"     ] -> [fname     ]
                 [fname, "&", cid] -> [fname, cid]
                 _                 -> ts
   in case ts' of
          [       cid] | is_cid cid -> Just (nilFS,              mk_cid cid)
          [fname, cid] | is_cid cid -> Just (mkFastString fname, mk_cid cid)
          [          ]              -> Just (nilFS,              mk_cid nm)
          [fname     ]              -> Just (mkFastString fname, mk_cid nm)
          _                         -> Nothing
    where is_cid :: String -> Bool
          is_cid x = all (/= '.') x && (isAlpha (head x) || head x == '_')
          mk_cid :: String -> CImportSpec
          mk_cid  = CFunction . StaticTarget . mkFastString

lex_ccall_impent :: String -> Maybe [String]
lex_ccall_impent "" = Just []
lex_ccall_impent ('&':xs) = fmap ("&":) $ lex_ccall_impent xs
lex_ccall_impent (' ':xs) = lex_ccall_impent xs
lex_ccall_impent ('\t':xs) = lex_ccall_impent xs
lex_ccall_impent xs = case span is_valid xs of
                          ("", _) -> Nothing
                          (t, xs') -> fmap (t:) $ lex_ccall_impent xs'
    where is_valid :: Char -> Bool
          is_valid c = isAscii c && (isAlphaNum c || c `elem` "._")


---------------------------------------------------
--		Declarations
---------------------------------------------------

cvtDecs :: [TH.Dec] -> CvtM (HsLocalBinds RdrName)
cvtDecs [] = return EmptyLocalBinds
cvtDecs ds = do { (binds,sigs) <- cvtBindsAndSigs ds
		; return (HsValBinds (ValBindsIn binds sigs)) }

cvtBindsAndSigs ds 
  = do { binds' <- mapM cvtBind binds; sigs' <- mapM cvtSig sigs
       ; return (listToBag binds', sigs') }
  where 
    (sigs, binds) = partition is_sig ds

    is_sig (TH.SigD _ _) = True
    is_sig other	 = False

cvtSig (TH.SigD nm ty)
  = do { nm' <- vNameL nm; ty' <- cvtType ty; returnL (Hs.TypeSig nm' ty') }

cvtBind :: TH.Dec -> CvtM (LHsBind RdrName)
-- Used only for declarations in a 'let/where' clause,
-- not for top level decls
cvtBind (TH.ValD (TH.VarP s) body ds) 
  = do	{ s' <- vNameL s
	; cl' <- cvtClause (Clause [] body ds)
	; returnL $ mkFunBind s' [cl'] }

cvtBind (TH.FunD nm cls)
  = do	{ nm' <- vNameL nm
	; cls' <- mapM cvtClause cls
	; returnL $ mkFunBind nm' cls' }

cvtBind (TH.ValD p body ds)
  = do	{ p' <- cvtPat p
	; g' <- cvtGuard body
	; ds' <- cvtDecs ds
	; returnL $ PatBind { pat_lhs = p', pat_rhs = GRHSs g' ds', 
			      pat_rhs_ty = void, bind_fvs = placeHolderNames } }

cvtBind d 
  = failWith (sep [ptext SLIT("Illegal kind of declaration in where clause"),
		   nest 2 (text (TH.pprint d))])

cvtClause :: TH.Clause -> CvtM (Hs.LMatch RdrName)
cvtClause (Clause ps body wheres)
  = do	{ ps' <- cvtPats ps
	; g'  <- cvtGuard body
	; ds' <- cvtDecs wheres
	; returnL $ Hs.Match ps' Nothing (GRHSs g' ds') }


-------------------------------------------------------------------
--		Expressions
-------------------------------------------------------------------

cvtl :: TH.Exp -> CvtM (LHsExpr RdrName)
cvtl e = wrapL (cvt e)
  where
    cvt (VarE s) 	= do { s' <- vName s; return $ HsVar s' }
    cvt (ConE s) 	= do { s' <- cName s; return $ HsVar s' }
    cvt (LitE l) 
      | overloadedLit l = do { l' <- cvtOverLit l; return $ HsOverLit l' }
      | otherwise	= do { l' <- cvtLit l;     return $ HsLit l' }

    cvt (AppE x y)     = do { x' <- cvtl x; y' <- cvtl y; return $ HsApp x' y' }
    cvt (LamE ps e)    = do { ps' <- cvtPats ps; e' <- cvtl e 
			    ; return $ HsLam (mkMatchGroup [mkSimpleMatch ps' e']) }
    cvt (TupE [e])     = cvt e
    cvt (TupE es)      = do { es' <- mapM cvtl es; return $ ExplicitTuple es' Boxed }
    cvt (CondE x y z)  = do { x' <- cvtl x; y' <- cvtl y; z' <- cvtl z
			    ; return $ HsIf x' y' z' }
    cvt (LetE ds e)    = do { ds' <- cvtDecs ds; e' <- cvtl e; return $ HsLet ds' e' }
    cvt (CaseE e ms)   = do { e' <- cvtl e; ms' <- mapM cvtMatch ms
			    ; return $ HsCase e' (mkMatchGroup ms') }
    cvt (DoE ss)       = cvtHsDo DoExpr ss
    cvt (CompE ss)     = cvtHsDo ListComp ss
    cvt (ArithSeqE dd) = do { dd' <- cvtDD dd; return $ ArithSeq noPostTcExpr dd' }
    cvt (ListE xs)     = do { xs' <- mapM cvtl xs; return $ ExplicitList void xs' }
    cvt (InfixE (Just x) s (Just y)) = do { x' <- cvtl x; s' <- cvtl s; y' <- cvtl y
					  ; e' <- returnL $ OpApp x' s' undefined y'
					  ; return $ HsPar e' }
    cvt (InfixE Nothing  s (Just y)) = do { s' <- cvtl s; y' <- cvtl y
					  ; return $ SectionR s' y' }
    cvt (InfixE (Just x) s Nothing ) = do { x' <- cvtl x; s' <- cvtl s
					  ; return $ SectionL x' s' }
    cvt (InfixE Nothing  s Nothing ) = cvt s	-- Can I indicate this is an infix thing?

    cvt (SigE e t)	 = do { e' <- cvtl e; t' <- cvtType t
			      ; return $ ExprWithTySig e' t' }
    cvt (RecConE c flds) = do { c' <- cNameL c
			      ; flds' <- mapM cvtFld flds
			      ; return $ RecordCon c' noPostTcExpr flds' }
    cvt (RecUpdE e flds) = do { e' <- cvtl e
			      ; flds' <- mapM cvtFld flds
			      ; return $ RecordUpd e' flds' placeHolderType placeHolderType }

cvtFld (v,e) = do { v' <- vNameL v; e' <- cvtl e; return (v',e') }

cvtDD :: Range -> CvtM (ArithSeqInfo RdrName)
cvtDD (FromR x) 	  = do { x' <- cvtl x; return $ From x' }
cvtDD (FromThenR x y)     = do { x' <- cvtl x; y' <- cvtl y; return $ FromThen x' y' }
cvtDD (FromToR x y)       = do { x' <- cvtl x; y' <- cvtl y; return $ FromTo x' y' }
cvtDD (FromThenToR x y z) = do { x' <- cvtl x; y' <- cvtl y; z' <- cvtl z; return $ FromThenTo x' y' z' }

-------------------------------------
-- 	Do notation and statements
-------------------------------------

cvtHsDo do_or_lc stmts
  = do	{ stmts' <- cvtStmts stmts
	; let body = case last stmts' of
			L _ (ExprStmt body _ _) -> body
	; return $ HsDo do_or_lc (init stmts') body void }

cvtStmts = mapM cvtStmt 

cvtStmt :: TH.Stmt -> CvtM (Hs.LStmt RdrName)
cvtStmt (NoBindS e)    = do { e' <- cvtl e; returnL $ mkExprStmt e' }
cvtStmt (TH.BindS p e) = do { p' <- cvtPat p; e' <- cvtl e; returnL $ mkBindStmt p' e' }
cvtStmt (TH.LetS ds)   = do { ds' <- cvtDecs ds; returnL $ LetStmt ds' }
cvtStmt (TH.ParS dss)  = do { dss' <- mapM cvt_one dss; returnL $ ParStmt dss' }
		       where
			 cvt_one ds = do { ds' <- cvtStmts ds; return (ds', undefined) }

cvtMatch :: TH.Match -> CvtM (Hs.LMatch RdrName)
cvtMatch (TH.Match p body decs)
  = do 	{ p' <- cvtPat p
	; g' <- cvtGuard body
	; decs' <- cvtDecs decs
	; returnL $ Hs.Match [p'] Nothing (GRHSs g' decs') }

cvtGuard :: TH.Body -> CvtM [LGRHS RdrName]
cvtGuard (GuardedB pairs) = mapM cvtpair pairs
cvtGuard (NormalB e)      = do { e' <- cvtl e; g' <- returnL $ GRHS [] e'; return [g'] }

cvtpair :: (TH.Guard, TH.Exp) -> CvtM (LGRHS RdrName)
cvtpair (NormalG ge,rhs) = do { ge' <- cvtl ge; rhs' <- cvtl rhs
			      ; g' <- returnL $ mkBindStmt truePat ge'
			      ; returnL $ GRHS [g'] rhs' }
cvtpair (PatG gs,rhs)    = do { gs' <- cvtStmts gs; rhs' <- cvtl rhs
			      ; returnL $ GRHS gs' rhs' }

cvtOverLit :: Lit -> CvtM (HsOverLit RdrName)
cvtOverLit (IntegerL i)  = do { force i; return $ mkHsIntegral i }
cvtOverLit (RationalL r) = do { force r; return $ mkHsFractional r }
-- An Integer is like an an (overloaded) '3' in a Haskell source program
-- Similarly 3.5 for fractionals

cvtLit :: Lit -> CvtM HsLit
cvtLit (IntPrimL i)    = do { force i; return $ HsIntPrim i }
cvtLit (FloatPrimL f)  = do { force f; return $ HsFloatPrim f }
cvtLit (DoublePrimL f) = do { force f; return $ HsDoublePrim f }
cvtLit (CharL c)       = do { force c; return $ HsChar c }
cvtLit (StringL s)     = do { let { s' = mkFastString s }; force s'; return $ HsString s' }

cvtPats :: [TH.Pat] -> CvtM [Hs.LPat RdrName]
cvtPats pats = mapM cvtPat pats

cvtPat :: TH.Pat -> CvtM (Hs.LPat RdrName)
cvtPat pat = wrapL (cvtp pat)

cvtp :: TH.Pat -> CvtM (Hs.Pat RdrName)
cvtp (TH.LitP l)
  | overloadedLit l   = do { l' <- cvtOverLit l
		 	   ; return (mkNPat l' Nothing) }
		 		  -- Not right for negative patterns; 
		 		  -- need to think about that!
  | otherwise	      = do { l' <- cvtLit l; return $ Hs.LitPat l' }
cvtp (TH.VarP s)      = do { s' <- vName s; return $ Hs.VarPat s' }
cvtp (TupP [p])       = cvtp p
cvtp (TupP ps)        = do { ps' <- cvtPats ps; return $ TuplePat ps' Boxed void }
cvtp (ConP s ps)      = do { s' <- cNameL s; ps' <- cvtPats ps; return $ ConPatIn s' (PrefixCon ps') }
cvtp (InfixP p1 s p2) = do { s' <- cNameL s; p1' <- cvtPat p1; p2' <- cvtPat p2
			   ; return $ ConPatIn s' (InfixCon p1' p2') }
cvtp (TildeP p)       = do { p' <- cvtPat p; return $ LazyPat p' }
cvtp (TH.AsP s p)     = do { s' <- vNameL s; p' <- cvtPat p; return $ AsPat s' p' }
cvtp TH.WildP         = return $ WildPat void
cvtp (RecP c fs)      = do { c' <- cNameL c; fs' <- mapM cvtPatFld fs 
		  	   ; return $ ConPatIn c' $ Hs.RecCon fs' }
cvtp (ListP ps)       = do { ps' <- cvtPats ps; return $ ListPat ps' void }
cvtp (SigP p t)       = do { p' <- cvtPat p; t' <- cvtType t; return $ SigPatIn p' t' }

cvtPatFld (s,p) = do { s' <- vNameL s; p' <- cvtPat p; return (s',p') }

-----------------------------------------------------------
--	Types and type variables

cvtTvs :: [TH.Name] -> CvtM [LHsTyVarBndr RdrName]
cvtTvs tvs = mapM cvt_tv tvs

cvt_tv tv = do { tv' <- tName tv; returnL $ UserTyVar tv' }

cvtContext :: Cxt -> CvtM (LHsContext RdrName)
cvtContext tys = do { preds' <- mapM cvtPred tys; returnL preds' }

cvtPred :: TH.Type -> CvtM (LHsPred RdrName)
cvtPred ty 
  = do	{ (head, tys') <- split_ty_app ty
	; case head of
	    ConT tc -> do { tc' <- tconName tc; returnL $ HsClassP tc' tys' }
	    VarT tv -> do { tv' <- tName tv;    returnL $ HsClassP tv' tys' }
	    other   -> failWith (ptext SLIT("Malformed predicate") <+> text (TH.pprint ty)) }

cvtType :: TH.Type -> CvtM (LHsType RdrName)
cvtType ty = do { (head, tys') <- split_ty_app ty
		; case head of
		    TupleT n | length tys' == n -> returnL (HsTupleTy Boxed tys')
		             | n == 0    -> mk_apps (HsTyVar (getRdrName unitTyCon)) tys'
		             | otherwise -> mk_apps (HsTyVar (getRdrName (tupleTyCon Boxed n))) tys'
		    ArrowT | [x',y'] <- tys' -> returnL (HsFunTy x' y')
		    ListT  | [x']    <- tys' -> returnL (HsListTy x')
		    VarT nm -> do { nm' <- tName nm;    mk_apps (HsTyVar nm') tys' }
		    ConT nm -> do { nm' <- tconName nm; mk_apps (HsTyVar nm') tys' }

		    ForallT tvs cxt ty | null tys' -> do { tvs' <- cvtTvs tvs
							 ; cxt' <- cvtContext cxt
							 ; ty'  <- cvtType ty
							 ; returnL $ mkExplicitHsForAllTy tvs' cxt' ty' }
		    otherwise -> failWith (ptext SLIT("Malformed type") <+> text (show ty))
	     }
  where
    mk_apps head []       = returnL head
    mk_apps head (ty:tys) = do { head' <- returnL head; mk_apps (HsAppTy head' ty) tys }

split_ty_app :: TH.Type -> CvtM (TH.Type, [LHsType RdrName])
split_ty_app ty = go ty []
  where
    go (AppT f a) as' = do { a' <- cvtType a; go f (a':as') }
    go f as 	      = return (f,as)

-----------------------------------------------------------


-----------------------------------------------------------
-- some useful things

truePat  = nlConPat (getRdrName trueDataCon)  []

overloadedLit :: Lit -> Bool
-- True for literals that Haskell treats as overloaded
overloadedLit (IntegerL  l) = True
overloadedLit (RationalL l) = True
overloadedLit l	            = False

void :: Type.Type
void = placeHolderType

--------------------------------------------------------------------
--	Turning Name back into RdrName
--------------------------------------------------------------------

-- variable names
vNameL, cNameL, tconNameL :: TH.Name -> CvtM (Located RdrName)
vName,  cName,  tName,  tconName  :: TH.Name -> CvtM RdrName

vNameL n = wrapL (vName n)
vName n = cvtName OccName.varName n

-- Constructor function names; this is Haskell source, hence srcDataName
cNameL n = wrapL (cName n)
cName n = cvtName OccName.dataName n 

-- Type variable names
tName n = cvtName OccName.tvName n

-- Type Constructor names
tconNameL n = wrapL (tconName n)
tconName n = cvtName OccName.tcClsName n

cvtName :: OccName.NameSpace -> TH.Name -> CvtM RdrName
cvtName ctxt_ns (TH.Name occ flavour)
  | not (okOcc ctxt_ns occ_str) = failWith (badOcc ctxt_ns occ_str)
  | otherwise 		        = force (thRdrName ctxt_ns occ_str flavour)
  where
    occ_str = TH.occString occ

okOcc :: OccName.NameSpace -> String -> Bool
okOcc _  []      = False
okOcc ns str@(c:_) 
  | OccName.isVarName ns = startsVarId c || startsVarSym c
  | otherwise 	 	 = startsConId c || startsConSym c || str == "[]"

badOcc :: OccName.NameSpace -> String -> SDoc
badOcc ctxt_ns occ 
  = ptext SLIT("Illegal") <+> pprNameSpace ctxt_ns
	<+> ptext SLIT("name:") <+> quotes (text occ)

thRdrName :: OccName.NameSpace -> String -> TH.NameFlavour -> RdrName
-- This turns a Name into a RdrName
-- The passed-in name space tells what the context is expecting;
--	use it unless the TH name knows what name-space it comes
-- 	from, in which case use the latter
--
-- ToDo: we may generate silly RdrNames, by passing a name space
--       that doesn't match the string, like VarName ":+", 
-- 	 which will give confusing error messages later
-- 
-- The strict applications ensure that any buried exceptions get forced
thRdrName ctxt_ns occ (TH.NameG th_ns pkg mod) = (mkOrig $! (mkModule (mk_pkg pkg) (mk_mod mod))) $! (mk_occ (mk_ghc_ns th_ns) occ)
thRdrName ctxt_ns occ (TH.NameL uniq)      = nameRdrName $! (((Name.mkInternalName $! (mk_uniq uniq)) $! (mk_occ ctxt_ns occ)) noSrcLoc)
thRdrName ctxt_ns occ (TH.NameQ mod)       = (mkRdrQual  $! (mk_mod mod)) $! (mk_occ ctxt_ns occ)
thRdrName ctxt_ns occ (TH.NameU uniq)      = mkRdrUnqual $! (mk_uniq_occ ctxt_ns occ uniq)
thRdrName ctxt_ns occ TH.NameS
  | Just name <- isBuiltInOcc ctxt_ns occ  = nameRdrName $! name
  | otherwise			           = mkRdrUnqual $! (mk_occ ctxt_ns occ)

isBuiltInOcc :: OccName.NameSpace -> String -> Maybe Name.Name
-- Built in syntax isn't "in scope" so an Unqual RdrName won't do
-- We must generate an Exact name, just as the parser does
isBuiltInOcc ctxt_ns occ
  = case occ of
	":" 		 -> Just (Name.getName consDataCon)
	"[]"		 -> Just (Name.getName nilDataCon)
	"()"		 -> Just (tup_name 0)
	'(' : ',' : rest -> go_tuple 2 rest
	other		 -> Nothing
  where
    go_tuple n ")" 	    = Just (tup_name n)
    go_tuple n (',' : rest) = go_tuple (n+1) rest
    go_tuple n other	    = Nothing

    tup_name n 
	| OccName.isTcClsName ctxt_ns = Name.getName (tupleTyCon Boxed n)
	| otherwise 		      = Name.getName (tupleCon Boxed n)

mk_uniq_occ :: OccName.NameSpace -> String -> Int# -> OccName.OccName
mk_uniq_occ ns occ uniq 
  = OccName.mkOccName ns (occ ++ '[' : shows (mk_uniq uniq) "]")
	-- The idea here is to make a name that 
	-- a) the user could not possibly write, and
	-- b) cannot clash with another NameU
	-- Previously I generated an Exact RdrName with mkInternalName.
	-- This works fine for local binders, but does not work at all for
	-- top-level binders, which must have External Names, since they are
	-- rapidly baked into data constructors and the like.  Baling out
	-- and generating an unqualified RdrName here is the simple solution

-- The packing and unpacking is rather turgid :-(
mk_occ :: OccName.NameSpace -> String -> OccName.OccName
mk_occ ns occ = OccName.mkOccNameFS ns (mkFastString occ)

mk_ghc_ns :: TH.NameSpace -> OccName.NameSpace
mk_ghc_ns TH.DataName  = OccName.dataName
mk_ghc_ns TH.TcClsName = OccName.tcClsName
mk_ghc_ns TH.VarName   = OccName.varName

mk_mod :: TH.ModName -> ModuleName
mk_mod mod = mkModuleName (TH.modString mod)

mk_pkg :: TH.ModName -> PackageId
mk_pkg pkg = stringToPackageId (TH.pkgString pkg)

mk_uniq :: Int# -> Unique
mk_uniq u = mkUniqueGrimily (I# u)
\end{code}

