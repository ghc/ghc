%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

This module converts Template Haskell syntax into HsSyn


\begin{code}
module Convert( convertToHsExpr, convertToHsDecls ) where

#include "HsVersions.h"

import Language.Haskell.THSyntax as Meta

import HsSyn as Hs
	(	HsExpr(..), HsLit(..), ArithSeqInfo(..), 
		HsStmtContext(..), TyClDecl(..),
		Match(..), GRHSs(..), GRHS(..), HsPred(..),
		HsDecl(..), TyClDecl(..), InstDecl(..), ConDecl(..),
		Stmt(..), HsBinds(..), MonoBinds(..), Sig(..),
		Pat(..), HsConDetails(..), HsOverLit, BangType(..),
		placeHolderType, HsType(..), HsTupCon(..),
		HsTyVarBndr(..), HsContext,
		mkSimpleMatch, mkHsForAllTy
	) 

import RdrName	( RdrName, mkRdrUnqual, mkRdrQual, mkOrig )
import Module   ( mkModuleName )
import RdrHsSyn	( mkHsIntegral, mkHsFractional, mkClassDecl, mkTyData )
import OccName
import SrcLoc	( SrcLoc, generatedSrcLoc )
import TyCon	( DataConDetails(..) )
import Type	( Type )
import BasicTypes( Boxity(..), RecFlag(Recursive), 
		   NewOrData(..), StrictnessMark(..) )
import ForeignCall ( Safety(..), CCallConv(..), CCallTarget(..),
                     CExportSpec(..)) 
import HsDecls ( CImportSpec(..), ForeignImport(..), ForeignExport(..),
                 ForeignDecl(..) )
import FastString( FastString, mkFastString, nilFS )
import Char 	( ord, isAscii, isAlphaNum, isAlpha )
import List	( partition )
import ErrUtils (Message)
import Outputable


-------------------------------------------------------------------
convertToHsDecls :: [Meta.Dec] -> [Either (HsDecl RdrName) Message]
convertToHsDecls ds = map cvt_top ds

mk_con con = case con of
	NormalC c strtys
	 -> ConDecl (cName c) noExistentials noContext
		  (PrefixCon (map mk_arg strtys)) loc0
	RecC c varstrtys
	 -> ConDecl (cName c) noExistentials noContext
		  (RecCon (map mk_id_arg varstrtys)) loc0
	InfixC st1 c st2
	 -> ConDecl (cName c) noExistentials noContext
		  (InfixCon (mk_arg st1) (mk_arg st2)) loc0
  where
    mk_arg (IsStrict, ty) = BangType MarkedUserStrict (cvtType ty)
    mk_arg (NotStrict, ty) = BangType NotMarkedStrict (cvtType ty)

    mk_id_arg (i, IsStrict, ty)
        = (vName i, BangType MarkedUserStrict (cvtType ty))
    mk_id_arg (i, NotStrict, ty)
        = (vName i, BangType NotMarkedStrict (cvtType ty))

mk_derivs [] = Nothing
mk_derivs cs = Just [HsClassP (tconName c) [] | c <- cs]

cvt_top :: Meta.Dec -> Either (HsDecl RdrName) Message
cvt_top d@(Meta.ValD _ _ _) = Left $ Hs.ValD (cvtd d)
cvt_top d@(Meta.FunD _ _)   = Left $ Hs.ValD (cvtd d)
 
cvt_top (TySynD tc tvs rhs)
  = Left $ TyClD (TySynonym (tconName tc) (cvt_tvs tvs) (cvtType rhs) loc0)

cvt_top (DataD ctxt tc tvs constrs derivs)
  = Left $ TyClD (mkTyData DataType 
                           (cvt_context ctxt, tconName tc, cvt_tvs tvs)
                           (DataCons (map mk_con constrs))
                           (mk_derivs derivs) loc0)

cvt_top (NewtypeD ctxt tc tvs constr derivs)
  = Left $ TyClD (mkTyData NewType 
                           (cvt_context ctxt, tconName tc, cvt_tvs tvs)
                           (DataCons [mk_con constr])
                           (mk_derivs derivs) loc0)

cvt_top (ClassD ctxt cl tvs decs)
  = Left $ TyClD (mkClassDecl (cvt_context ctxt, tconName cl, cvt_tvs tvs)
                              noFunDeps sigs
			      (Just binds) loc0)
  where
    (binds,sigs) = cvtBindsAndSigs decs

cvt_top (InstanceD tys ty decs)
  = Left $ InstD (InstDecl inst_ty binds sigs Nothing loc0)
  where
    (binds, sigs) = cvtBindsAndSigs decs
    inst_ty = HsForAllTy Nothing 
			 (cvt_context tys) 
			 (HsPredTy (cvt_pred ty))

cvt_top (Meta.SigD nm typ) = Left $ Hs.SigD (Sig (vName nm) (cvtType typ) loc0)

cvt_top (ForeignD (ImportF callconv safety from nm typ))
 = case parsed of
       Just (c_header, cis) ->
           let i = CImport callconv' safety' c_header nilFS cis
           in Left $ ForD (ForeignImport (vName nm) (cvtType typ) i False loc0)
       Nothing -> Right $     text (show from)
                          <+> ptext SLIT("is not a valid ccall impent")
    where callconv' = case callconv of
                          CCall -> CCallConv
                          StdCall -> StdCallConv
          safety' = case safety of
                        Unsafe     -> PlayRisky
                        Safe       -> PlaySafe False
                        Threadsafe -> PlaySafe True
          parsed = parse_ccall_impent nm from

cvt_top (ForeignD (ExportF callconv as nm typ))
 = let e = CExport (CExportStatic (mkFastString as) callconv')
   in Left $ ForD (ForeignExport (vName nm) (cvtType typ) e False loc0)
    where callconv' = case callconv of
                          CCall -> CCallConv
                          StdCall -> StdCallConv

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

noContext      = []
noExistentials = []
noFunDeps      = []

-------------------------------------------------------------------
convertToHsExpr :: Meta.Exp -> HsExpr RdrName
convertToHsExpr = cvt

cvt (VarE s) 	  = HsVar (vName s)
cvt (ConE s) 	  = HsVar (cName s)
cvt (LitE l) 
  | overloadedLit l = HsOverLit (cvtOverLit l)
  | otherwise	    = HsLit (cvtLit l)

cvt (AppE x y)     = HsApp (cvt x) (cvt y)
cvt (LamE ps e)    = HsLam (mkSimpleMatch (map cvtp ps) (cvt e) void loc0)
cvt (TupE [e])	  = cvt e
cvt (TupE es)	  = ExplicitTuple(map cvt es) Boxed
cvt (CondE x y z)  = HsIf (cvt x) (cvt y) (cvt z) loc0
cvt (LetE ds e)	  = HsLet (cvtdecs ds) (cvt e)
cvt (CaseE e ms)   = HsCase (cvt e) (map cvtm ms) loc0
cvt (DoE ss)	  = HsDo DoExpr (cvtstmts ss) [] void loc0
cvt (CompE ss)     = HsDo ListComp (cvtstmts ss) [] void loc0
cvt (ArithSeqE dd) = ArithSeqIn (cvtdd dd)
cvt (ListE xs)  = ExplicitList void (map cvt xs)
cvt (InfixE (Just x) s (Just y))
    = HsPar (OpApp (cvt x) (cvt s) undefined (cvt y))
cvt (InfixE Nothing  s (Just y)) = SectionR (cvt s) (cvt y)
cvt (InfixE (Just x) s Nothing ) = SectionL (cvt x) (cvt s)
cvt (InfixE Nothing  s Nothing ) = cvt s	-- Can I indicate this is an infix thing?
cvt (SigE e t)		= ExprWithTySig (cvt e) (cvtType t)
cvt (RecConE c flds) = RecordCon (cName c) (map (\(x,y) -> (vName x, cvt y)) flds)
cvt (RecUpdE e flds) = RecordUpd (cvt e) (map (\(x,y) -> (vName x, cvt y)) flds)

cvtdecs :: [Meta.Dec] -> HsBinds RdrName
cvtdecs [] = EmptyBinds
cvtdecs ds = MonoBind binds sigs Recursive
	   where
	     (binds, sigs) = cvtBindsAndSigs ds

cvtBindsAndSigs ds 
  = (cvtds non_sigs, map cvtSig sigs)
  where 
    (sigs, non_sigs) = partition sigP ds

cvtSig (Meta.SigD nm typ) = Hs.Sig (vName nm) (cvtType typ) loc0

cvtds :: [Meta.Dec] -> MonoBinds RdrName
cvtds []     = EmptyMonoBinds
cvtds (d:ds) = AndMonoBinds (cvtd d) (cvtds ds)

cvtd :: Meta.Dec -> MonoBinds RdrName
-- Used only for declarations in a 'let/where' clause,
-- not for top level decls
cvtd (Meta.ValD (Meta.VarP s) body ds) = FunMonoBind (vName s) False 
					  [cvtclause (Clause [] body ds)] loc0
cvtd (FunD nm cls)   	    = FunMonoBind (vName nm) False (map cvtclause cls) loc0
cvtd (Meta.ValD p body ds)	    = PatMonoBind (cvtp p) (GRHSs (cvtguard body) 
							  (cvtdecs ds) 
							  void) loc0
cvtd x = panic "Illegal kind of declaration in where clause" 


cvtclause :: Meta.Clause -> Hs.Match RdrName
cvtclause (Clause ps body wheres)
    = Hs.Match (map cvtp ps) Nothing (GRHSs (cvtguard body) (cvtdecs wheres) void)



cvtdd :: Range -> ArithSeqInfo RdrName
cvtdd (FromR x) 	      = (From (cvt x))
cvtdd (FromThenR x y)     = (FromThen (cvt x) (cvt y))
cvtdd (FromToR x y)	      = (FromTo (cvt x) (cvt y))
cvtdd (FromThenToR x y z) = (FromThenTo (cvt x) (cvt y) (cvt z))


cvtstmts :: [Meta.Stmt] -> [Hs.Stmt RdrName]
cvtstmts []		       = [] -- this is probably an error as every [stmt] should end with ResultStmt
cvtstmts [NoBindS e]           = [ResultStmt (cvt e) loc0]      -- when its the last element use ResultStmt
cvtstmts (NoBindS e : ss)      = ExprStmt (cvt e) void loc0     : cvtstmts ss
cvtstmts (Meta.BindS p e : ss) = BindStmt (cvtp p) (cvt e) loc0 : cvtstmts ss
cvtstmts (Meta.LetS ds : ss)   = LetStmt (cvtdecs ds)	    : cvtstmts ss
cvtstmts (Meta.ParS dss : ss)  = ParStmt [(cvtstmts ds, undefined) | ds <- dss] : cvtstmts ss

cvtm :: Meta.Match -> Hs.Match RdrName
cvtm (Meta.Match p body wheres)
    = Hs.Match [cvtp p] Nothing (GRHSs (cvtguard body) (cvtdecs wheres) void)
                             
cvtguard :: Meta.Body -> [GRHS RdrName]
cvtguard (GuardedB pairs) = map cvtpair pairs
cvtguard (NormalB e) 	 = [GRHS [  ResultStmt (cvt e) loc0 ] loc0]

cvtpair :: (Meta.Exp,Meta.Exp) -> GRHS RdrName
cvtpair (x,y) = GRHS [Hs.BindStmt truePat (cvt x) loc0,
		      ResultStmt (cvt y) loc0] loc0

cvtOverLit :: Lit -> HsOverLit
cvtOverLit (IntegerL i)  = mkHsIntegral i
cvtOverLit (RationalL r) = mkHsFractional r
-- An Integer is like an an (overloaded) '3' in a Haskell source program
-- Similarly 3.5 for fractionals

cvtLit :: Lit -> HsLit
cvtLit (IntPrimL i)    = HsIntPrim i
cvtLit (FloatPrimL f)  = HsFloatPrim f
cvtLit (DoublePrimL f) = HsDoublePrim f
cvtLit (CharL c)       = HsChar (ord c)
cvtLit (StringL s)     = HsString (mkFastString s)

cvtp :: Meta.Pat -> Hs.Pat RdrName
cvtp (Meta.LitP l)
  | overloadedLit l = NPatIn (cvtOverLit l) Nothing	-- Not right for negative
							-- patterns; need to think
							-- about that!
  | otherwise	    = Hs.LitPat (cvtLit l)
cvtp (Meta.VarP s)     = Hs.VarPat(vName s)
cvtp (TupP [p])   = cvtp p
cvtp (TupP ps)    = TuplePat (map cvtp ps) Boxed
cvtp (ConP s ps)  = ConPatIn (cName s) (PrefixCon (map cvtp ps))
cvtp (TildeP p)   = LazyPat (cvtp p)
cvtp (Meta.AsP s p) = AsPat (vName s) (cvtp p)
cvtp Meta.WildP   = WildPat void
cvtp (RecP c fs)  = ConPatIn (cName c) $ Hs.RecCon (map (\(s,p) -> (vName s,cvtp p)) fs)
cvtp (ListP ps)   = ListPat (map cvtp ps) void

-----------------------------------------------------------
--	Types and type variables

cvt_tvs :: [String] -> [HsTyVarBndr RdrName]
cvt_tvs tvs = map (UserTyVar . tName) tvs

cvt_context :: Cxt -> HsContext RdrName 
cvt_context tys = map cvt_pred tys

cvt_pred :: Meta.Type -> HsPred RdrName
cvt_pred ty = case split_ty_app ty of
	   	(ConT tc, tys) -> HsClassP (tconName tc) (map cvtType tys)
		other -> panic "Malformed predicate"

cvtType :: Meta.Type -> HsType RdrName
cvtType ty = trans (root ty [])
  where root (AppT a b) zs = root a (cvtType b : zs)
        root t zs 	   = (t,zs)

        trans (TupleT n,args)
            | length args == n = HsTupleTy (HsTupCon Boxed n) args
            | n == 0 = foldl HsAppTy (HsTyVar (tconName "()")) args
            | otherwise = foldl HsAppTy (HsTyVar (tconName ("(" ++ replicate (n-1) ',' ++ ")"))) args
        trans (ArrowT,   [x,y]) = HsFunTy x y
        trans (ListT,    [x])   = HsListTy x

	trans (VarT nm, args)	    = foldl HsAppTy (HsTyVar (tName nm)) args
        trans (ConT tc, args)       = foldl HsAppTy (HsTyVar (tconName tc)) args

	trans (ForallT tvs cxt ty, []) = mkHsForAllTy (Just (cvt_tvs tvs))
				  		      (cvt_context cxt)
				      		      (cvtType ty)

split_ty_app :: Meta.Type -> (Meta.Type, [Meta.Type])
split_ty_app ty = go ty []
  where
    go (AppT f a) as = go f (a:as)
    go f as 	     = (f,as)

-----------------------------------------------------------
sigP :: Dec -> Bool
sigP (Meta.SigD _ _) = True
sigP other	 = False


-----------------------------------------------------------
-- some useful things

truePat  = ConPatIn (cName "True") (PrefixCon [])
falsePat = ConPatIn (cName "False") (PrefixCon [])

overloadedLit :: Lit -> Bool
-- True for literals that Haskell treats as overloaded
overloadedLit (IntegerL  l) = True
overloadedLit (RationalL l) = True
overloadedLit l	            = False

void :: Type.Type
void = placeHolderType

loc0 :: SrcLoc
loc0 = generatedSrcLoc

-- variable names
vName :: String -> RdrName
vName = mkName varName

-- Constructor function names; this is Haskell source, hence srcDataName
cName :: String -> RdrName
cName = mkName srcDataName

-- Type variable names
tName :: String -> RdrName
tName = mkName tvName

-- Type Constructor names
tconName = mkName tcName

mkName :: NameSpace -> String -> RdrName
-- Parse the string to see if it has a "." or ":" in it
-- so we know whether to generate a qualified or original name
-- It's a bit tricky because we need to parse 
--	Foo.Baz.x as Qual Foo.Baz x
-- So we parse it from back to front

mkName ns str
  = split [] (reverse str)
  where
    split occ [] = mkRdrUnqual (mk_occ occ)
    split occ (c:d:rev) 	-- 'd' is the last char before the separator
	|  is_sep c 		-- E.g.		Fo.x	d='o'
	&& isAlphaNum d		--		Fo.+:	d='+' perhaps
	= mk_qual (reverse (d:rev)) c occ
    split occ (c:rev) = split (c:occ) rev

    mk_qual mod '.' occ = mkRdrQual (mk_mod mod) (mk_occ occ)
    mk_qual mod ':' occ = mkOrig    (mk_mod mod) (mk_occ occ)

    mk_occ occ = mkOccFS ns (mkFastString occ)
    mk_mod mod = mkModuleName mod

    is_sep '.' 	 = True
    is_sep ':' 	 = True
    is_sep other = False
\end{code}

