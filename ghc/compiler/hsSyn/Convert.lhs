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
import ForeignCall ( Safety(..), CCallConv(..), CCallTarget(..) )
import HsDecls ( CImportSpec(..), ForeignImport(..), ForeignDecl(..) )
import FastString( FastString, mkFastString, nilFS )
import Char 	( ord, isAscii, isAlphaNum, isAlpha )
import List	( partition )
import ErrUtils (Message)
import Outputable


-------------------------------------------------------------------
convertToHsDecls :: [Meta.Dec] -> [Either (HsDecl RdrName) Message]
convertToHsDecls ds = map cvt_top ds

mk_con con = case con of
	NormalCon c strtys
	 -> ConDecl (cName c) noExistentials noContext
		  (PrefixCon (map mk_arg strtys)) loc0
	Meta.RecCon c varstrtys
	 -> ConDecl (cName c) noExistentials noContext
		  (Hs.RecCon (map mk_id_arg varstrtys)) loc0
	Meta.InfixCon st1 c st2
	 -> ConDecl (cName c) noExistentials noContext
		  (Hs.InfixCon (mk_arg st1) (mk_arg st2)) loc0
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
cvt_top d@(ValDec _ _ _) = Left $ ValD (cvtd d)
cvt_top d@(FunDec _ _)   = Left $ ValD (cvtd d)
 
cvt_top (TySynDec tc tvs rhs)
  = Left $ TyClD (TySynonym (tconName tc) (cvt_tvs tvs) (cvtType rhs) loc0)

cvt_top (DataDec ctxt tc tvs constrs derivs)
  = Left $ TyClD (mkTyData DataType 
                           (cvt_context ctxt, tconName tc, cvt_tvs tvs)
                           (DataCons (map mk_con constrs))
                           (mk_derivs derivs) loc0)

cvt_top (NewtypeDec ctxt tc tvs constr derivs)
  = Left $ TyClD (mkTyData NewType 
                           (cvt_context ctxt, tconName tc, cvt_tvs tvs)
                           (DataCons [mk_con constr])
                           (mk_derivs derivs) loc0)

cvt_top (ClassDec ctxt cl tvs decs)
  = Left $ TyClD (mkClassDecl (cvt_context ctxt, tconName cl, cvt_tvs tvs)
                              noFunDeps sigs
			      (Just binds) loc0)
  where
    (binds,sigs) = cvtBindsAndSigs decs

cvt_top (InstanceDec tys ty decs)
  = Left $ InstD (InstDecl inst_ty binds sigs Nothing loc0)
  where
    (binds, sigs) = cvtBindsAndSigs decs
    inst_ty = HsForAllTy Nothing 
			 (cvt_context tys) 
			 (HsPredTy (cvt_pred ty))

cvt_top (SigDec nm typ) = Left $ SigD (Sig (vName nm) (cvtType typ) loc0)

cvt_top (ForeignDec (ImportForeign callconv safety from nm typ))
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

cvt (VarExp s) 	  = HsVar (vName s)
cvt (ConExp s) 	  = HsVar (cName s)
cvt (LitExp l) 
  | overloadedLit l = HsOverLit (cvtOverLit l)
  | otherwise	    = HsLit (cvtLit l)

cvt (AppExp x y)     = HsApp (cvt x) (cvt y)
cvt (LamExp ps e)    = HsLam (mkSimpleMatch (map cvtp ps) (cvt e) void loc0)
cvt (TupExp [e])	  = cvt e
cvt (TupExp es)	  = ExplicitTuple(map cvt es) Boxed
cvt (CondExp x y z)  = HsIf (cvt x) (cvt y) (cvt z) loc0
cvt (LetExp ds e)	  = HsLet (cvtdecs ds) (cvt e)
cvt (CaseExp e ms)   = HsCase (cvt e) (map cvtm ms) loc0
cvt (DoExp ss)	  = HsDo DoExpr (cvtstmts ss) [] void loc0
cvt (CompExp ss)     = HsDo ListComp (cvtstmts ss) [] void loc0
cvt (ArithSeqExp dd) = ArithSeqIn (cvtdd dd)
cvt (ListExp xs)  = ExplicitList void (map cvt xs)
cvt (InfixExp (Just x) s (Just y))
    = HsPar (OpApp (cvt x) (cvt s) undefined (cvt y))
cvt (InfixExp Nothing  s (Just y)) = SectionR (cvt s) (cvt y)
cvt (InfixExp (Just x) s Nothing ) = SectionL (cvt x) (cvt s)
cvt (InfixExp Nothing  s Nothing ) = cvt s	-- Can I indicate this is an infix thing?
cvt (SigExp e t)		= ExprWithTySig (cvt e) (cvtType t)
cvt (RecConExp c flds) = RecordCon (cName c) (map (\(x,y) -> (vName x, cvt y)) flds)
cvt (RecUpdExp e flds) = RecordUpd (cvt e) (map (\(x,y) -> (vName x, cvt y)) flds)

cvtdecs :: [Meta.Dec] -> HsBinds RdrName
cvtdecs [] = EmptyBinds
cvtdecs ds = MonoBind binds sigs Recursive
	   where
	     (binds, sigs) = cvtBindsAndSigs ds

cvtBindsAndSigs ds 
  = (cvtds non_sigs, map cvtSig sigs)
  where 
    (sigs, non_sigs) = partition sigP ds

cvtSig (SigDec nm typ) = Sig (vName nm) (cvtType typ) loc0

cvtds :: [Meta.Dec] -> MonoBinds RdrName
cvtds []     = EmptyMonoBinds
cvtds (d:ds) = AndMonoBinds (cvtd d) (cvtds ds)

cvtd :: Meta.Dec -> MonoBinds RdrName
-- Used only for declarations in a 'let/where' clause,
-- not for top level decls
cvtd (ValDec (Meta.VarPat s) body ds) = FunMonoBind (vName s) False 
					  [cvtclause (Clause [] body ds)] loc0
cvtd (FunDec nm cls)   	    = FunMonoBind (vName nm) False (map cvtclause cls) loc0
cvtd (ValDec p body ds)	    = PatMonoBind (cvtp p) (GRHSs (cvtguard body) 
							  (cvtdecs ds) 
							  void) loc0
cvtd x = panic "Illegal kind of declaration in where clause" 


cvtclause :: Meta.Clause -> Hs.Match RdrName
cvtclause (Clause ps body wheres)
    = Hs.Match (map cvtp ps) Nothing (GRHSs (cvtguard body) (cvtdecs wheres) void)



cvtdd :: Meta.DotDot -> ArithSeqInfo RdrName
cvtdd (FromDotDot x) 	      = (Hs.From (cvt x))
cvtdd (FromThenDotDot x y)     = (Hs.FromThen (cvt x) (cvt y))
cvtdd (FromToDotDot x y)	      = (Hs.FromTo (cvt x) (cvt y))
cvtdd (FromThenToDotDot x y z) = (Hs.FromThenTo (cvt x) (cvt y) (cvt z))


cvtstmts :: [Meta.Stmt] -> [Hs.Stmt RdrName]
cvtstmts [] = [] -- this is probably an error as every [stmt] should end with ResultStmt
cvtstmts [NoBindStmt e]      = [ResultStmt (cvt e) loc0]      -- when its the last element use ResultStmt
cvtstmts (NoBindStmt e : ss) = ExprStmt (cvt e) void loc0     : cvtstmts ss
cvtstmts (Meta.BindStmt p e : ss) = Hs.BindStmt (cvtp p) (cvt e) loc0 : cvtstmts ss
cvtstmts (Meta.LetStmt ds : ss)   = Hs.LetStmt (cvtdecs ds)	    : cvtstmts ss
cvtstmts (Meta.ParStmt dss : ss)  = Hs.ParStmt(map cvtstmts dss)      : cvtstmts ss


cvtm :: Meta.Match -> Hs.Match RdrName
cvtm (Meta.Match p body wheres)
    = Hs.Match [cvtp p] Nothing (GRHSs (cvtguard body) (cvtdecs wheres) void)
                             
cvtguard :: Meta.RHS -> [GRHS RdrName]
cvtguard (GuardedRHS pairs) = map cvtpair pairs
cvtguard (NormalRHS e) 	 = [GRHS [  ResultStmt (cvt e) loc0 ] loc0]

cvtpair :: (Meta.Exp,Meta.Exp) -> GRHS RdrName
cvtpair (x,y) = GRHS [Hs.BindStmt truePat (cvt x) loc0,
		      ResultStmt (cvt y) loc0] loc0

cvtOverLit :: Lit -> HsOverLit
cvtOverLit (IntegerLit i)  = mkHsIntegral i
cvtOverLit (RationalLit r) = mkHsFractional r
-- An Integer is like an an (overloaded) '3' in a Haskell source program
-- Similarly 3.5 for fractionals

cvtLit :: Lit -> HsLit
cvtLit (IntPrimLit i)    = HsIntPrim i
cvtLit (FloatPrimLit f)  = HsFloatPrim f
cvtLit (DoublePrimLit f) = HsDoublePrim f
cvtLit (CharLit c)       = HsChar (ord c)
cvtLit (StringLit s)     = HsString (mkFastString s)

cvtp :: Meta.Pat -> Hs.Pat RdrName
cvtp (Meta.LitPat l)
  | overloadedLit l = NPatIn (cvtOverLit l) Nothing	-- Not right for negative
							-- patterns; need to think
							-- about that!
  | otherwise	    = Hs.LitPat (cvtLit l)
cvtp (Meta.VarPat s)     = Hs.VarPat(vName s)
cvtp (TupPat [p])   = cvtp p
cvtp (TupPat ps)    = TuplePat (map cvtp ps) Boxed
cvtp (ConPat s ps)  = ConPatIn (cName s) (PrefixCon (map cvtp ps))
cvtp (TildePat p)   = LazyPat (cvtp p)
cvtp (Meta.AsPat s p) = Hs.AsPat (vName s) (cvtp p)
cvtp Meta.WildPat        = Hs.WildPat void
cvtp (RecPat c fs)  = ConPatIn (cName c) $ Hs.RecCon (map (\(s,p) -> (vName s,cvtp p)) fs)

-----------------------------------------------------------
--	Types and type variables

cvt_tvs :: [String] -> [HsTyVarBndr RdrName]
cvt_tvs tvs = map (UserTyVar . tName) tvs

cvt_context :: Cxt -> HsContext RdrName 
cvt_context tys = map cvt_pred tys

cvt_pred :: Typ -> HsPred RdrName
cvt_pred ty = case split_ty_app ty of
	   	(ConTyp (ConNameTag tc), tys) -> HsClassP (tconName tc) (map cvtType tys)
		other -> panic "Malformed predicate"

cvtType :: Meta.Typ -> HsType RdrName
cvtType ty = trans (root ty [])
  where root (AppTyp a b) zs = root a (cvtType b : zs)
        root t zs 	   = (t,zs)

        trans (ConTyp (TupleTag n),args) | length args == n
				    = HsTupleTy (HsTupCon Boxed n) args
        trans (ConTyp ArrowTag,   [x,y]) = HsFunTy x y
        trans (ConTyp ListTag,    [x])   = HsListTy x

	trans (VarTyp nm, args)	    = foldl HsAppTy (HsTyVar (tName nm)) args
        trans (ConTyp tc, args)       = foldl HsAppTy (HsTyVar (tc_name tc)) args

	trans (ForallTyp tvs cxt ty, []) = mkHsForAllTy (Just (cvt_tvs tvs))
				  		      (cvt_context cxt)
				      		      (cvtType ty)

	tc_name (ConNameTag nm) = tconName nm
	tc_name ArrowTag	      = tconName "->"
	tc_name ListTag	      = tconName "[]"
	tc_name (TupleTag 0)     = tconName "()"
   	tc_name (TupleTag n)     = tconName ("(" ++ replicate (n-1) ',' ++ ")")

split_ty_app :: Typ -> (Typ, [Typ])
split_ty_app ty = go ty []
  where
    go (AppTyp f a) as = go f (a:as)
    go f as 	     = (f,as)

-----------------------------------------------------------
sigP :: Dec -> Bool
sigP (SigDec _ _) = True
sigP other	 = False


-----------------------------------------------------------
-- some useful things

truePat  = ConPatIn (cName "True") (PrefixCon [])
falsePat = ConPatIn (cName "False") (PrefixCon [])

overloadedLit :: Lit -> Bool
-- True for literals that Haskell treats as overloaded
overloadedLit (IntegerLit  l) = True
overloadedLit (RationalLit l) = True
overloadedLit l	              = False

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

