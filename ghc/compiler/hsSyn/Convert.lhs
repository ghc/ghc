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


cvt_top :: Meta.Dec -> Either (HsDecl RdrName) Message
cvt_top d@(Val _ _ _) = Left $ ValD (cvtd d)
cvt_top d@(Fun _ _)   = Left $ ValD (cvtd d)
 
cvt_top (TySyn tc tvs rhs)
  = Left $ TyClD (TySynonym (tconName tc) (cvt_tvs tvs) (cvtType rhs) loc0)

cvt_top (Data tc tvs constrs derivs)
  = Left $ TyClD (mkTyData DataType 
                           (noContext, tconName tc, cvt_tvs tvs)
                           (DataCons (map mk_con constrs))
                           (mk_derivs derivs) loc0)
  where
    mk_con (Constr c tys)
	= ConDecl (cName c) noExistentials noContext
		    (PrefixCon (map mk_arg tys)) loc0

    mk_arg ty = BangType NotMarkedStrict (cvtType ty)

    mk_derivs [] = Nothing
    mk_derivs cs = Just [HsClassP (tconName c) [] | c <- cs]

cvt_top (Class ctxt cl tvs decs)
  = Left $ TyClD (mkClassDecl (cvt_context ctxt, tconName cl, cvt_tvs tvs)
                              noFunDeps
                              sigs (Just binds) loc0)
  where
    (binds,sigs) = cvtBindsAndSigs decs

cvt_top (Instance tys ty decs)
  = Left $ InstD (InstDecl inst_ty binds sigs Nothing loc0)
  where
    (binds, sigs) = cvtBindsAndSigs decs
    inst_ty = HsForAllTy Nothing 
			 (cvt_context tys) 
			 (HsPredTy (cvt_pred ty))

cvt_top (Proto nm typ) = Left $ SigD (Sig (vName nm) (cvtType typ) loc0)

cvt_top (Foreign (Import callconv safety from nm typ))
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

cvt (Var s) 	  = HsVar(vName s)
cvt (Con s) 	  = HsVar(cName s)
cvt (Lit l) 
  | overloadedLit l = HsOverLit (cvtOverLit l)
  | otherwise	    = HsLit (cvtLit l)

cvt (App x y)     = HsApp (cvt x) (cvt y)
cvt (Lam ps e)    = HsLam (mkSimpleMatch (map cvtp ps) (cvt e) void loc0)
cvt (Tup [e])	  = cvt e
cvt (Tup es)	  = ExplicitTuple(map cvt es) Boxed
cvt (Cond x y z)  = HsIf (cvt x) (cvt y) (cvt z) loc0
cvt (Let ds e)	  = HsLet (cvtdecs ds) (cvt e)
cvt (Case e ms)   = HsCase (cvt e) (map cvtm ms) loc0
cvt (Do ss)	  = HsDo DoExpr (cvtstmts ss) [] void loc0
cvt (Comp ss)     = HsDo ListComp (cvtstmts ss) [] void loc0
cvt (ArithSeq dd) = ArithSeqIn (cvtdd dd)
cvt (ListExp xs)  = ExplicitList void (map cvt xs)
cvt (Infix (Just x) s (Just y))
    = HsPar (OpApp (cvt x) (cvt s) undefined (cvt y))
cvt (Infix Nothing  s (Just y)) = SectionR (cvt s) (cvt y)
cvt (Infix (Just x) s Nothing ) = SectionL (cvt x) (cvt s)
cvt (Infix Nothing  s Nothing ) = cvt s	-- Can I indicate this is an infix thing?
cvt (SigExp e t)		= ExprWithTySig (cvt e) (cvtType t)

cvtdecs :: [Meta.Dec] -> HsBinds RdrName
cvtdecs [] = EmptyBinds
cvtdecs ds = MonoBind binds sigs Recursive
	   where
	     (binds, sigs) = cvtBindsAndSigs ds

cvtBindsAndSigs ds 
  = (cvtds non_sigs, map cvtSig sigs)
  where 
    (sigs, non_sigs) = partition sigP ds

cvtSig (Proto nm typ) = Sig (vName nm) (cvtType typ) loc0

cvtds :: [Meta.Dec] -> MonoBinds RdrName
cvtds []     = EmptyMonoBinds
cvtds (d:ds) = AndMonoBinds (cvtd d) (cvtds ds)

cvtd :: Meta.Dec -> MonoBinds RdrName
-- Used only for declarations in a 'let/where' clause,
-- not for top level decls
cvtd (Val (Pvar s) body ds) = FunMonoBind (vName s) False 
					  [cvtclause (Clause [] body ds)] loc0
cvtd (Fun nm cls)   	    = FunMonoBind (vName nm) False (map cvtclause cls) loc0
cvtd (Val p body ds)	    = PatMonoBind (cvtp p) (GRHSs (cvtguard body) 
							  (cvtdecs ds) 
							  void) loc0
cvtd x = panic "Illegal kind of declaration in where clause" 


cvtclause :: Meta.Clause (Meta.Pat) (Meta.Exp) (Meta.Dec) -> Hs.Match RdrName
cvtclause (Clause ps body wheres)
    = Match (map cvtp ps) Nothing (GRHSs (cvtguard body) (cvtdecs wheres) void)



cvtdd :: Meta.DDt -> ArithSeqInfo RdrName
cvtdd (Meta.From x) 	      = (Hs.From (cvt x))
cvtdd (Meta.FromThen x y)     = (Hs.FromThen (cvt x) (cvt y))
cvtdd (Meta.FromTo x y)	      = (Hs.FromTo (cvt x) (cvt y))
cvtdd (Meta.FromThenTo x y z) = (Hs.FromThenTo (cvt x) (cvt y) (cvt z))


cvtstmts :: [Meta.Stm] -> [Hs.Stmt RdrName]
cvtstmts [] = [] -- this is probably an error as every [stmt] should end with ResultStmt
cvtstmts [NoBindSt e]      = [ResultStmt (cvt e) loc0]      -- when its the last element use ResultStmt
cvtstmts (NoBindSt e : ss) = ExprStmt (cvt e) void loc0     : cvtstmts ss
cvtstmts (BindSt p e : ss) = BindStmt (cvtp p) (cvt e) loc0 : cvtstmts ss
cvtstmts (LetSt ds : ss)   = LetStmt (cvtdecs ds)	    : cvtstmts ss
cvtstmts (ParSt dss : ss)  = ParStmt(map cvtstmts dss)      : cvtstmts ss


cvtm :: Meta.Mat -> Hs.Match RdrName
cvtm (Mat p body wheres)
    = Match [cvtp p] Nothing (GRHSs (cvtguard body) (cvtdecs wheres) void)
                             
cvtguard :: Meta.Rhs -> [GRHS RdrName]
cvtguard (Guarded pairs) = map cvtpair pairs
cvtguard (Normal e) 	 = [GRHS [  ResultStmt (cvt e) loc0 ] loc0]

cvtpair :: (Meta.Exp,Meta.Exp) -> GRHS RdrName
cvtpair (x,y) = GRHS [BindStmt truePat (cvt x) loc0,
		      ResultStmt (cvt y) loc0] loc0

cvtOverLit :: Lit -> HsOverLit
cvtOverLit (Integer i)  = mkHsIntegral i
cvtOverLit (Rational r) = mkHsFractional r
-- An Integer is like an an (overloaded) '3' in a Haskell source program
-- Similarly 3.5 for fractionals

cvtLit :: Lit -> HsLit
cvtLit (Char c)	  = HsChar (ord c)
cvtLit (String s) = HsString (mkFastString s)

cvtp :: Meta.Pat -> Hs.Pat RdrName
cvtp (Plit l)
  | overloadedLit l = NPatIn (cvtOverLit l) Nothing	-- Not right for negative
							-- patterns; need to think
							-- about that!
  | otherwise	    = LitPat (cvtLit l)
cvtp (Pvar s)     = VarPat(vName s)
cvtp (Ptup [p])   = cvtp p
cvtp (Ptup ps)    = TuplePat (map cvtp ps) Boxed
cvtp (Pcon s ps)  = ConPatIn (cName s) (PrefixCon (map cvtp ps))
cvtp (Ptilde p)   = LazyPat (cvtp p)
cvtp (Paspat s p) = AsPat (vName s) (cvtp p)
cvtp Pwild        = WildPat void

-----------------------------------------------------------
--	Types and type variables

cvt_tvs :: [String] -> [HsTyVarBndr RdrName]
cvt_tvs tvs = map (UserTyVar . tName) tvs

cvt_context :: Cxt -> HsContext RdrName 
cvt_context tys = map cvt_pred tys

cvt_pred :: Typ -> HsPred RdrName
cvt_pred ty = case split_ty_app ty of
	   	(Tcon (TconName tc), tys) -> HsClassP (tconName tc) (map cvtType tys)
		other -> panic "Malformed predicate"

cvtType :: Meta.Typ -> HsType RdrName
cvtType ty = trans (root ty [])
  where root (Tapp a b) zs = root a (cvtType b : zs)
        root t zs 	   = (t,zs)

        trans (Tcon (Tuple n),args) | length args == n
				    = HsTupleTy (HsTupCon Boxed n) args
        trans (Tcon Arrow,   [x,y]) = HsFunTy x y
        trans (Tcon List,    [x])   = HsListTy x

	trans (Tvar nm, args)	    = foldl HsAppTy (HsTyVar (tName nm)) args
        trans (Tcon tc, args)       = foldl HsAppTy (HsTyVar (tc_name tc)) args

	trans (TForall tvs cxt ty, []) = mkHsForAllTy (Just (cvt_tvs tvs))
				  		      (cvt_context cxt)
				      		      (cvtType ty)

	tc_name (TconName nm) = tconName nm
	tc_name Arrow	      = tconName "->"
	tc_name List	      = tconName "[]"
	tc_name (Tuple 0)     = tconName "()"
   	tc_name (Tuple n)     = tconName ("(" ++ replicate (n-1) ',' ++ ")")

split_ty_app :: Typ -> (Typ, [Typ])
split_ty_app ty = go ty []
  where
    go (Tapp f a) as = go f (a:as)
    go f as 	     = (f,as)

-----------------------------------------------------------
sigP :: Dec -> Bool
sigP (Proto _ _) = True
sigP other	 = False


-----------------------------------------------------------
-- some useful things

truePat  = ConPatIn (cName "True") (PrefixCon [])
falsePat = ConPatIn (cName "False") (PrefixCon [])

overloadedLit :: Lit -> Bool
-- True for literals that Haskell treats as overloaded
overloadedLit (Integer  l) = True
overloadedLit (Rational l) = True
overloadedLit l	           = False

void :: Type.Type
void = placeHolderType

loc0 :: SrcLoc
loc0 = generatedSrcLoc

-- variable names
vName :: String -> RdrName
vName = mkName varName

-- Constructor function names
cName :: String -> RdrName
cName = mkName dataName

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
