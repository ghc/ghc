%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

This module converts Template Haskell syntax into HsSyn


\begin{code}
module Convert( convertToHsExpr, convertToHsDecls ) where

#include "HsVersions.h"

import Language.Haskell.TH.THSyntax as TH
import Language.Haskell.TH.THLib    as TH	-- Pretty printing

import HsSyn as Hs
	(	HsExpr(..), HsLit(..), ArithSeqInfo(..), 
		HsStmtContext(..), TyClDecl(..), HsBang(..),
		Match(..), GRHSs(..), GRHS(..), HsPred(..),
		HsDecl(..), TyClDecl(..), InstDecl(..), ConDecl(..),
		Stmt(..), HsBinds(..), MonoBinds(..), Sig(..),
		Pat(..), HsConDetails(..), HsOverLit, BangType(..),
		placeHolderType, HsType(..), HsExplicitForAll(..),
		HsTyVarBndr(..), HsContext,
		mkSimpleMatch, mkImplicitHsForAllTy, mkExplicitHsForAllTy
	) 

import RdrName	( RdrName, mkRdrUnqual, mkRdrQual, mkOrig, nameRdrName, getRdrName )
import Module   ( ModuleName, mkModuleName )
import RdrHsSyn	( mkHsIntegral, mkHsFractional, mkClassDecl, mkTyData )
import Name	( mkInternalName )
import qualified OccName
import SrcLoc	( SrcLoc, generatedSrcLoc )
import Type	( Type )
import TysWiredIn ( unitTyCon, tupleTyCon, trueDataCon, falseDataCon )
import BasicTypes( Boxity(..), RecFlag(Recursive), NewOrData(..) )
import ForeignCall ( Safety(..), CCallConv(..), CCallTarget(..),
                     CExportSpec(..)) 
import HsDecls ( CImportSpec(..), ForeignImport(..), ForeignExport(..),
                 ForeignDecl(..) )
import FastString( FastString, mkFastString, nilFS )
import Char 	( ord, isAscii, isAlphaNum, isAlpha )
import List	( partition )
import SrcLoc	( noSrcLoc )
import Unique	( Unique, mkUniqueGrimily )
import ErrUtils (Message)
import GLAEXTS	( Int#, Int(..) )
import Outputable


-------------------------------------------------------------------
convertToHsDecls :: [TH.Dec] -> [Either (HsDecl RdrName) Message]
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
    mk_arg (IsStrict, ty)  = BangType HsStrict (cvtType ty)
    mk_arg (NotStrict, ty) = BangType HsNoBang (cvtType ty)

    mk_id_arg (i, IsStrict, ty)
        = (vName i, BangType HsStrict (cvtType ty))
    mk_id_arg (i, NotStrict, ty)
        = (vName i, BangType HsNoBang (cvtType ty))

mk_derivs [] = Nothing
mk_derivs cs = Just [HsClassP (tconName c) [] | c <- cs]

cvt_top :: TH.Dec -> Either (HsDecl RdrName) Message
cvt_top d@(TH.ValD _ _ _) = Left $ Hs.ValD (cvtd d)
cvt_top d@(TH.FunD _ _)   = Left $ Hs.ValD (cvtd d)
 
cvt_top (TySynD tc tvs rhs)
  = Left $ TyClD (TySynonym (tconName tc) (cvt_tvs tvs) (cvtType rhs) loc0)

cvt_top (DataD ctxt tc tvs constrs derivs)
  = Left $ TyClD (mkTyData DataType 
                           (cvt_context ctxt, tconName tc, cvt_tvs tvs)
                           (map mk_con constrs)
                           (mk_derivs derivs) loc0)

cvt_top (NewtypeD ctxt tc tvs constr derivs)
  = Left $ TyClD (mkTyData NewType 
                           (cvt_context ctxt, tconName tc, cvt_tvs tvs)
                           [mk_con constr]
                           (mk_derivs derivs) loc0)

cvt_top (ClassD ctxt cl tvs decs)
  = Left $ TyClD (mkClassDecl (cvt_context ctxt, tconName cl, cvt_tvs tvs)
                              noFunDeps sigs
			      binds loc0)
  where
    (binds,sigs) = cvtBindsAndSigs decs

cvt_top (InstanceD tys ty decs)
  = Left $ InstD (InstDecl inst_ty binds sigs loc0)
  where
    (binds, sigs) = cvtBindsAndSigs decs
    inst_ty = mkImplicitHsForAllTy (cvt_context tys) (HsPredTy (cvt_pred ty))

cvt_top (TH.SigD nm typ) = Left $ Hs.SigD (Sig (vName nm) (cvtType typ) loc0)

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
          parsed = parse_ccall_impent (TH.nameBase nm) from

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
convertToHsExpr :: TH.Exp -> HsExpr RdrName
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

cvtdecs :: [TH.Dec] -> HsBinds RdrName
cvtdecs [] = EmptyBinds
cvtdecs ds = MonoBind binds sigs Recursive
	   where
	     (binds, sigs) = cvtBindsAndSigs ds

cvtBindsAndSigs ds 
  = (cvtds non_sigs, map cvtSig sigs)
  where 
    (sigs, non_sigs) = partition sigP ds

cvtSig (TH.SigD nm typ) = Hs.Sig (vName nm) (cvtType typ) loc0

cvtds :: [TH.Dec] -> MonoBinds RdrName
cvtds []     = EmptyMonoBinds
cvtds (d:ds) = AndMonoBinds (cvtd d) (cvtds ds)

cvtd :: TH.Dec -> MonoBinds RdrName
-- Used only for declarations in a 'let/where' clause,
-- not for top level decls
cvtd (TH.ValD (TH.VarP s) body ds) = FunMonoBind (vName s) False 
					  [cvtclause (Clause [] body ds)] loc0
cvtd (FunD nm cls)   	    = FunMonoBind (vName nm) False (map cvtclause cls) loc0
cvtd (TH.ValD p body ds)	    = PatMonoBind (cvtp p) (GRHSs (cvtguard body) 
							  (cvtdecs ds) 
							  void) loc0

cvtd d = cvtPanic "Illegal kind of declaration in where clause" 
		  (text (show (TH.pprDec d)))


cvtclause :: TH.Clause -> Hs.Match RdrName
cvtclause (Clause ps body wheres)
    = Hs.Match (map cvtp ps) Nothing (GRHSs (cvtguard body) (cvtdecs wheres) void)



cvtdd :: Range -> ArithSeqInfo RdrName
cvtdd (FromR x) 	      = (From (cvt x))
cvtdd (FromThenR x y)     = (FromThen (cvt x) (cvt y))
cvtdd (FromToR x y)	      = (FromTo (cvt x) (cvt y))
cvtdd (FromThenToR x y z) = (FromThenTo (cvt x) (cvt y) (cvt z))


cvtstmts :: [TH.Stmt] -> [Hs.Stmt RdrName]
cvtstmts []		       = [] -- this is probably an error as every [stmt] should end with ResultStmt
cvtstmts [NoBindS e]           = [ResultStmt (cvt e) loc0]      -- when its the last element use ResultStmt
cvtstmts (NoBindS e : ss)      = ExprStmt (cvt e) void loc0     : cvtstmts ss
cvtstmts (TH.BindS p e : ss) = BindStmt (cvtp p) (cvt e) loc0 : cvtstmts ss
cvtstmts (TH.LetS ds : ss)   = LetStmt (cvtdecs ds)	    : cvtstmts ss
cvtstmts (TH.ParS dss : ss)  = ParStmt [(cvtstmts ds, undefined) | ds <- dss] : cvtstmts ss

cvtm :: TH.Match -> Hs.Match RdrName
cvtm (TH.Match p body wheres)
    = Hs.Match [cvtp p] Nothing (GRHSs (cvtguard body) (cvtdecs wheres) void)
                             
cvtguard :: TH.Body -> [GRHS RdrName]
cvtguard (GuardedB pairs) = map cvtpair pairs
cvtguard (NormalB e) 	 = [GRHS [  ResultStmt (cvt e) loc0 ] loc0]

cvtpair :: (TH.Exp,TH.Exp) -> GRHS RdrName
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

cvtp :: TH.Pat -> Hs.Pat RdrName
cvtp (TH.LitP l)
  | overloadedLit l = NPatIn (cvtOverLit l) Nothing	-- Not right for negative
							-- patterns; need to think
							-- about that!
  | otherwise	    = Hs.LitPat (cvtLit l)
cvtp (TH.VarP s)     = Hs.VarPat(vName s)
cvtp (TupP [p])   = cvtp p
cvtp (TupP ps)    = TuplePat (map cvtp ps) Boxed
cvtp (ConP s ps)  = ConPatIn (cName s) (PrefixCon (map cvtp ps))
cvtp (TildeP p)   = LazyPat (cvtp p)
cvtp (TH.AsP s p) = AsPat (vName s) (cvtp p)
cvtp TH.WildP   = WildPat void
cvtp (RecP c fs)  = ConPatIn (cName c) $ Hs.RecCon (map (\(s,p) -> (vName s,cvtp p)) fs)
cvtp (ListP ps)   = ListPat (map cvtp ps) void

-----------------------------------------------------------
--	Types and type variables

cvt_tvs :: [TH.Name] -> [HsTyVarBndr RdrName]
cvt_tvs tvs = map (UserTyVar . tName) tvs

cvt_context :: Cxt -> HsContext RdrName 
cvt_context tys = map cvt_pred tys

cvt_pred :: TH.Type -> HsPred RdrName
cvt_pred ty = case split_ty_app ty of
	   	(ConT tc, tys) -> HsClassP (tconName tc) (map cvtType tys)
	   	(VarT tv, tys) -> HsClassP (tName tv) (map cvtType tys)
		other -> cvtPanic "Malformed predicate" (text (show (TH.pprType ty)))

cvtType :: TH.Type -> HsType RdrName
cvtType ty = trans (root ty [])
  where root (AppT a b) zs = root a (cvtType b : zs)
        root t zs 	   = (t,zs)

        trans (TupleT n,args)
            | length args == n = HsTupleTy Boxed args
            | n == 0    = foldl HsAppTy (HsTyVar (getRdrName unitTyCon))	    args
            | otherwise = foldl HsAppTy (HsTyVar (getRdrName (tupleTyCon Boxed n))) args
        trans (ArrowT,   [x,y]) = HsFunTy x y
        trans (ListT,    [x])   = HsListTy x

	trans (VarT nm, args)	    = foldl HsAppTy (HsTyVar (tName nm)) args
        trans (ConT tc, args)       = foldl HsAppTy (HsTyVar (tconName tc)) args

	trans (ForallT tvs cxt ty, []) = mkExplicitHsForAllTy 
						(cvt_tvs tvs) (cvt_context cxt) (cvtType ty)

split_ty_app :: TH.Type -> (TH.Type, [TH.Type])
split_ty_app ty = go ty []
  where
    go (AppT f a) as = go f (a:as)
    go f as 	     = (f,as)

-----------------------------------------------------------
sigP :: Dec -> Bool
sigP (TH.SigD _ _) = True
sigP other	 = False


-----------------------------------------------------------
cvtPanic :: String -> SDoc -> b
cvtPanic herald thing
  = pprPanic herald (thing $$ ptext SLIT("When splicing generated code into the program"))

-----------------------------------------------------------
-- some useful things

truePat  = ConPatIn (getRdrName trueDataCon)  (PrefixCon [])
falsePat = ConPatIn (getRdrName falseDataCon) (PrefixCon [])

overloadedLit :: Lit -> Bool
-- True for literals that Haskell treats as overloaded
overloadedLit (IntegerL  l) = True
overloadedLit (RationalL l) = True
overloadedLit l	            = False

void :: Type.Type
void = placeHolderType

loc0 :: SrcLoc
loc0 = generatedSrcLoc

--------------------------------------------------------------------
--	Turning Name back into RdrName
--------------------------------------------------------------------

-- variable names
vName :: TH.Name -> RdrName
vName = mk_name OccName.varName

-- Constructor function names; this is Haskell source, hence srcDataName
cName :: TH.Name -> RdrName
cName = mk_name OccName.srcDataName

-- Type variable names
tName :: TH.Name -> RdrName
tName = mk_name OccName.tvName

-- Type Constructor names
tconName = mk_name OccName.tcName

mk_name :: OccName.NameSpace -> TH.Name -> RdrName

-- This turns a Name into a RdrName
-- The last case is slightly interesting.  It constructs a
-- unique name from the unique in the TH thingy, so that the renamer
-- won't mess about.  I hope.  (Another possiblity would be to generate 
-- "x_77" etc, but that could conceivably clash.)

mk_name ns (TH.Name occ (TH.NameG ns' mod))  = mkOrig (mk_mod mod) (mk_occ ns occ)
mk_name ns (TH.Name occ TH.NameS)            = mkRdrUnqual (mk_occ ns occ)
mk_name ns (TH.Name occ (TH.NameU uniq))     = nameRdrName (mkInternalName (mk_uniq uniq) (mk_occ ns occ) noSrcLoc)

mk_uniq :: Int# -> Unique
mk_uniq u = mkUniqueGrimily (I# u)

-- The packing and unpacking is rather turgid :-(
mk_occ :: OccName.NameSpace -> TH.OccName -> OccName.OccName
mk_occ ns occ = OccName.mkOccFS ns (mkFastString (TH.occString occ))

mk_mod :: TH.ModName -> ModuleName
mk_mod mod = mkModuleName (TH.modString mod)
\end{code}

