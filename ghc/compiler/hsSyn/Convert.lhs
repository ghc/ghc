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
import Name	( mkInternalName )
import Module   ( Module, mkModule )
import RdrHsSyn	( mkClassDecl, mkTyData )
import qualified OccName
import SrcLoc	( generatedSrcLoc, noLoc, unLoc, Located(..),
		  SrcSpan, srcLocSpan )
import Type	( Type )
import TysWiredIn ( unitTyCon, tupleTyCon, trueDataCon )
import BasicTypes( Boxity(..), RecFlag(Recursive) )
import ForeignCall ( Safety(..), CCallConv(..), CCallTarget(..),
                     CExportSpec(..)) 
import Char 	( isAscii, isAlphaNum, isAlpha )
import List	( partition )
import Unique	( Unique, mkUniqueGrimily )
import ErrUtils (Message)
import GLAEXTS	( Int(..), Int# )
import SrcLoc	( noSrcLoc )
import Bag	( emptyBag, consBag )
import FastString
import Outputable


-------------------------------------------------------------------
convertToHsDecls :: [TH.Dec] -> [Either (LHsDecl RdrName) Message]
convertToHsDecls ds = map cvt_ltop ds

mk_con con = L loc0 $ mk_nlcon con
  where
    mk_nlcon con = case con of
	NormalC c strtys
	 -> ConDecl (noLoc (cName c)) noExistentials noContext
		  (PrefixCon (map mk_arg strtys))
	RecC c varstrtys
	 -> ConDecl (noLoc (cName c)) noExistentials noContext
		  (RecCon (map mk_id_arg varstrtys))
	InfixC st1 c st2
	 -> ConDecl (noLoc (cName c)) noExistentials noContext
		  (InfixCon (mk_arg st1) (mk_arg st2))
	ForallC tvs ctxt (ForallC tvs' ctxt' con')
	 -> mk_nlcon (ForallC (tvs ++ tvs') (ctxt ++ ctxt') con')
	ForallC tvs ctxt con' -> case mk_nlcon con' of
				ConDecl l [] (L _ []) x ->
				    ConDecl l (cvt_tvs tvs) (cvt_context ctxt) x
				c -> panic "ForallC: Can't happen"
    mk_arg (IsStrict, ty)  = noLoc $ HsBangTy HsStrict (cvtType ty)
    mk_arg (NotStrict, ty) = cvtType ty

    mk_id_arg (i, IsStrict, ty)
        = (noLoc (vName i), noLoc $ HsBangTy HsStrict (cvtType ty))
    mk_id_arg (i, NotStrict, ty)
        = (noLoc (vName i), cvtType ty)

mk_derivs [] = Nothing
mk_derivs cs = Just [noLoc $ HsPredTy $ HsClassP (tconName c) [] | c <- cs]

cvt_ltop  :: TH.Dec -> Either (LHsDecl RdrName) Message
cvt_ltop d = case cvt_top d of
		Left d -> Left (L loc0 d)
		Right m -> Right m

cvt_top :: TH.Dec -> Either (HsDecl RdrName) Message
cvt_top d@(TH.ValD _ _ _) = Left $ Hs.ValD (unLoc (cvtd d))
cvt_top d@(TH.FunD _ _)   = Left $ Hs.ValD (unLoc (cvtd d))
 
cvt_top (TySynD tc tvs rhs)
  = Left $ TyClD (TySynonym (noLoc (tconName tc)) (cvt_tvs tvs) (cvtType rhs))

cvt_top (DataD ctxt tc tvs constrs derivs)
  = Left $ TyClD (mkTyData DataType 
                           (noLoc (cvt_context ctxt, noLoc (tconName tc), cvt_tvs tvs))
                           Nothing (map mk_con constrs)
                           (mk_derivs derivs))

cvt_top (NewtypeD ctxt tc tvs constr derivs)
  = Left $ TyClD (mkTyData NewType 
                           (noLoc (cvt_context ctxt, noLoc (tconName tc), cvt_tvs tvs))
                           Nothing [mk_con constr]
                           (mk_derivs derivs))

cvt_top (ClassD ctxt cl tvs fds decs)
  = Left $ TyClD $ mkClassDecl (cvt_context ctxt,
                                noLoc (tconName cl),
                                cvt_tvs tvs)
                               (map (noLoc . cvt_fundep) fds)
                               sigs
                               binds
  where
    (binds,sigs) = cvtBindsAndSigs decs

cvt_top (InstanceD tys ty decs)
  = Left $ InstD (InstDecl (noLoc inst_ty) binds sigs)
  where
    (binds, sigs) = cvtBindsAndSigs decs
    inst_ty = mkImplicitHsForAllTy (cvt_context tys) (noLoc (HsPredTy (cvt_pred ty)))

cvt_top (TH.SigD nm typ) = Left $ Hs.SigD (Sig (noLoc (vName nm)) (cvtType typ))

cvt_top (ForeignD (ImportF callconv safety from nm typ))
 = case parsed of
       Just (c_header, cis) ->
           let i = CImport callconv' safety' c_header nilFS cis
           in Left $ ForD (ForeignImport (noLoc (vName nm)) (cvtType typ) i False)
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
   in Left $ ForD (ForeignExport (noLoc (vName nm)) (cvtType typ) e False)
    where callconv' = case callconv of
                          CCall -> CCallConv
                          StdCall -> StdCallConv

cvt_fundep :: FunDep -> Class.FunDep RdrName
cvt_fundep (FunDep xs ys) = (map tName xs, map tName ys)

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

noContext      = noLoc []
noExistentials = []

-------------------------------------------------------------------
convertToHsExpr :: TH.Exp -> LHsExpr RdrName
convertToHsExpr = cvtl

cvtl e = noLoc (cvt e)

cvt (VarE s) 	  = HsVar (vName s)
cvt (ConE s) 	  = HsVar (cName s)
cvt (LitE l) 
  | overloadedLit l = HsOverLit (cvtOverLit l)
  | otherwise	    = HsLit (cvtLit l)

cvt (AppE x y)     = HsApp (cvtl x) (cvtl y)
cvt (LamE ps e)    = HsLam (mkMatchGroup [mkSimpleMatch (map cvtlp ps) (cvtl e)])
cvt (TupE [e])	   = cvt e
cvt (TupE es)	   = ExplicitTuple(map cvtl es) Boxed
cvt (CondE x y z)  = HsIf (cvtl x) (cvtl y) (cvtl z)
cvt (LetE ds e)	   = HsLet (cvtdecs ds) (cvtl e)
cvt (CaseE e ms)   = HsCase (cvtl e) (mkMatchGroup (map cvtm ms))
cvt (DoE ss)	   = cvtHsDo DoExpr   ss
cvt (CompE ss)     = cvtHsDo ListComp ss
cvt (ArithSeqE dd) = ArithSeq noPostTcExpr (cvtdd dd)
cvt (ListE xs)     = ExplicitList void (map cvtl xs)
cvt (InfixE (Just x) s (Just y))
    = HsPar (noLoc $ OpApp (cvtl x) (cvtl s) undefined (cvtl y))
cvt (InfixE Nothing  s (Just y)) = SectionR (cvtl s) (cvtl y)
cvt (InfixE (Just x) s Nothing ) = SectionL (cvtl x) (cvtl s)
cvt (InfixE Nothing  s Nothing ) = cvt s	-- Can I indicate this is an infix thing?
cvt (SigE e t)		= ExprWithTySig (cvtl e) (cvtType t)
cvt (RecConE c flds) = RecordCon (noLoc (cName c)) noPostTcExpr
				 (map (\(x,y) -> (noLoc (vName x), cvtl y)) flds)
cvt (RecUpdE e flds) = RecordUpd (cvtl e) (map (\(x,y) -> (noLoc (vName x), cvtl y)) flds)
				 placeHolderType placeHolderType

cvtHsDo do_or_lc stmts
  = HsDo do_or_ld (init stmts') body void
  where
    stmts' = cvtstmts ss
    body = case last stmts' of
		L _ (ExprStmt body _) -> body

cvtdecs :: [TH.Dec] -> [HsBindGroup RdrName]
cvtdecs [] = []
cvtdecs ds = [HsBindGroup binds sigs Recursive]
	   where
	     (binds, sigs) = cvtBindsAndSigs ds

cvtBindsAndSigs ds 
  = (cvtds non_sigs, map cvtSig sigs)
  where 
    (sigs, non_sigs) = partition sigP ds

cvtSig (TH.SigD nm typ) = noLoc (Hs.Sig (noLoc (vName nm)) (cvtType typ))

cvtds :: [TH.Dec] -> LHsBinds RdrName
cvtds []     = emptyBag
cvtds (d:ds) = cvtd d `consBag` cvtds ds

cvtd :: TH.Dec -> LHsBind RdrName
-- Used only for declarations in a 'let/where' clause,
-- not for top level decls
cvtd (TH.ValD (TH.VarP s) body ds) 
  = noLoc $ FunBind (noLoc (vName s)) False (mkMatchGroup [cvtclause (Clause [] body ds)])
cvtd (FunD nm cls)
  = noLoc $ FunBind (noLoc (vName nm)) False (mkMatchGroup (map cvtclause cls))
cvtd (TH.ValD p body ds)
  = noLoc $ PatBind (cvtlp p) (GRHSs (cvtguard body) (cvtdecs ds)) void

cvtd d = cvtPanic "Illegal kind of declaration in where clause" 
		  (text (TH.pprint d))


cvtclause :: TH.Clause -> Hs.LMatch RdrName
cvtclause (Clause ps body wheres)
    = noLoc $ Hs.Match (map cvtlp ps) Nothing (GRHSs (cvtguard body) (cvtdecs wheres))



cvtdd :: Range -> ArithSeqInfo RdrName
cvtdd (FromR x) 	      = (From (cvtl x))
cvtdd (FromThenR x y)     = (FromThen (cvtl x) (cvtl y))
cvtdd (FromToR x y)	      = (FromTo (cvtl x) (cvtl y))
cvtdd (FromThenToR x y z) = (FromThenTo (cvtl x) (cvtl y) (cvtl z))


cvtstmts :: [TH.Stmt] -> [Hs.LStmt RdrName]
cvtstmts []		     = []
cvtstmts (NoBindS e : ss)    = noLoc (mkExprStmt (cvtl e))	     : cvtstmts ss
cvtstmts (TH.BindS p e : ss) = noLoc (mkBindStmt (cvtlp p) (cvtl e)) : cvtstmts ss
cvtstmts (TH.LetS ds : ss)   = noLoc (LetStmt (cvtdecs ds))	     : cvtstmts ss
cvtstmts (TH.ParS dss : ss)  = noLoc (ParStmt [(cvtstmts ds, undefined) | ds <- dss]) : cvtstmts ss

cvtm :: TH.Match -> Hs.LMatch RdrName
cvtm (TH.Match p body wheres)
    = noLoc (Hs.Match [cvtlp p] Nothing (GRHSs (cvtguard body) (cvtdecs wheres)))

cvtguard :: TH.Body -> [LGRHS RdrName]
cvtguard (GuardedB pairs) = map cvtpair pairs
cvtguard (NormalB e) 	 = [noLoc (GRHS [] (cvtl e))]

cvtpair :: (TH.Guard,TH.Exp) -> LGRHS RdrName
cvtpair (NormalG x,y) = noLoc (GRHS [nlBindStmt truePat (cvtl x)]
				    (cvtl y))
cvtpair (PatG x,y) = noLoc (GRHS (cvtstmts x) (cvtl y))

cvtOverLit :: Lit -> HsOverLit RdrName
cvtOverLit (IntegerL i)  = mkHsIntegral i
cvtOverLit (RationalL r) = mkHsFractional r
-- An Integer is like an an (overloaded) '3' in a Haskell source program
-- Similarly 3.5 for fractionals

cvtLit :: Lit -> HsLit
cvtLit (IntPrimL i)    = HsIntPrim i
cvtLit (FloatPrimL f)  = HsFloatPrim f
cvtLit (DoublePrimL f) = HsDoublePrim f
cvtLit (CharL c)       = HsChar c
cvtLit (StringL s)     = HsString (mkFastString s)

cvtlp :: TH.Pat -> Hs.LPat RdrName
cvtlp pat = noLoc (cvtp pat)

cvtp :: TH.Pat -> Hs.Pat RdrName
cvtp (TH.LitP l)
  | overloadedLit l = mkNPat (cvtOverLit l) Nothing	-- Not right for negative
							-- patterns; need to think
							-- about that!
  | otherwise	    = Hs.LitPat (cvtLit l)
cvtp (TH.VarP s)     = Hs.VarPat(vName s)
cvtp (TupP [p])   = cvtp p
cvtp (TupP ps)    = TuplePat (map cvtlp ps) Boxed
cvtp (ConP s ps)  = ConPatIn (noLoc (cName s)) (PrefixCon (map cvtlp ps))
cvtp (InfixP p1 s p2)
                  = ConPatIn (noLoc (cName s)) (InfixCon (cvtlp p1) (cvtlp p2))
cvtp (TildeP p)   = LazyPat (cvtlp p)
cvtp (TH.AsP s p) = AsPat (noLoc (vName s)) (cvtlp p)
cvtp TH.WildP   = WildPat void
cvtp (RecP c fs)  = ConPatIn (noLoc (cName c)) $ Hs.RecCon (map (\(s,p) -> (noLoc (vName s),cvtlp p)) fs)
cvtp (ListP ps)   = ListPat (map cvtlp ps) void
cvtp (SigP p t)   = SigPatIn (cvtlp p) (cvtType t)

-----------------------------------------------------------
--	Types and type variables

cvt_tvs :: [TH.Name] -> [LHsTyVarBndr RdrName]
cvt_tvs tvs = map (noLoc . UserTyVar . tName) tvs

cvt_context :: Cxt -> LHsContext RdrName 
cvt_context tys = noLoc (map (noLoc . cvt_pred) tys)

cvt_pred :: TH.Type -> HsPred RdrName
cvt_pred ty = case split_ty_app ty of
	   	(ConT tc, tys) -> HsClassP (tconName tc) (map cvtType tys)
	   	(VarT tv, tys) -> HsClassP (tName tv) (map cvtType tys)
		other -> cvtPanic "Malformed predicate" (text (TH.pprint ty))

convertToHsType = cvtType

cvtType :: TH.Type -> LHsType RdrName
cvtType ty = trans (root ty [])
  where root (AppT a b) zs = root a (cvtType b : zs)
        root t zs 	   = (t,zs)

        trans (TupleT n,args)
            | length args == n = noLoc (HsTupleTy Boxed args)
            | n == 0    = foldl nlHsAppTy (nlHsTyVar (getRdrName unitTyCon))	    args
            | otherwise = foldl nlHsAppTy (nlHsTyVar (getRdrName (tupleTyCon Boxed n))) args
        trans (ArrowT,   [x,y]) = nlHsFunTy x y
        trans (ListT,    [x])   = noLoc (HsListTy x)

	trans (VarT nm, args)	    = foldl nlHsAppTy (nlHsTyVar (tName nm))    args
        trans (ConT tc, args)       = foldl nlHsAppTy (nlHsTyVar (tconName tc)) args

	trans (ForallT tvs cxt ty, []) = noLoc $ mkExplicitHsForAllTy 
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

truePat  = nlConPat (getRdrName trueDataCon)  []

overloadedLit :: Lit -> Bool
-- True for literals that Haskell treats as overloaded
overloadedLit (IntegerL  l) = True
overloadedLit (RationalL l) = True
overloadedLit l	            = False

void :: Type.Type
void = placeHolderType

loc0 :: SrcSpan
loc0 = srcLocSpan generatedSrcLoc

--------------------------------------------------------------------
--	Turning Name back into RdrName
--------------------------------------------------------------------

-- variable names
vName :: TH.Name -> RdrName
vName = thRdrName OccName.varName

-- Constructor function names; this is Haskell source, hence srcDataName
cName :: TH.Name -> RdrName
cName = thRdrName OccName.srcDataName

-- Type variable names
tName :: TH.Name -> RdrName
tName = thRdrName OccName.tvName

-- Type Constructor names
tconName = thRdrName OccName.tcName

thRdrName :: OccName.NameSpace -> TH.Name -> RdrName
-- This turns a Name into a RdrName
-- The passed-in name space tells what the context is expecting;
--	use it unless the TH name knows what name-space it comes
-- 	from, in which case use the latter
thRdrName ctxt_ns (TH.Name occ (TH.NameG th_ns mod)) = mkOrig      (mk_mod mod) (mk_occ (mk_ghc_ns th_ns) occ)
thRdrName ctxt_ns (TH.Name occ (TH.NameL uniq))      = nameRdrName (mkInternalName (mk_uniq uniq) (mk_occ ctxt_ns occ) noSrcLoc)
thRdrName ctxt_ns (TH.Name occ (TH.NameQ mod))       = mkRdrQual   (mk_mod mod) (mk_occ ctxt_ns occ)
thRdrName ctxt_ns (TH.Name occ TH.NameS)             = mkRdrUnqual (mk_occ ctxt_ns occ)
thRdrName ctxt_ns (TH.Name occ (TH.NameU uniq))      = mkRdrUnqual (mk_uniq_occ ctxt_ns occ uniq)

mk_uniq_occ :: OccName.NameSpace -> TH.OccName -> Int# -> OccName.OccName
mk_uniq_occ ns occ uniq 
  = OccName.mkOccName ns (TH.occString occ ++ '[' : shows (mk_uniq uniq) "]")
	-- The idea here is to make a name that 
	-- a) the user could not possibly write, and
	-- b) cannot clash with another NameU
	-- Previously I generated an Exact RdrName with mkInternalName.
	-- This works fine for local binders, but does not work at all for
	-- top-level binders, which must have External Names, since they are
	-- rapidly baked into data constructors and the like.  Baling out
	-- and generating an unqualified RdrName here is the simple solution

mk_ghc_ns :: TH.NameSpace -> OccName.NameSpace
mk_ghc_ns DataName     = OccName.dataName
mk_ghc_ns TH.TcClsName = OccName.tcClsName
mk_ghc_ns TH.VarName   = OccName.varName

-- The packing and unpacking is rather turgid :-(
mk_occ :: OccName.NameSpace -> TH.OccName -> OccName.OccName
mk_occ ns occ = OccName.mkOccFS ns (mkFastString (TH.occString occ))

mk_mod :: TH.ModName -> Module
mk_mod mod = mkModule (TH.modString mod)

mk_uniq :: Int# -> Unique
mk_uniq u = mkUniqueGrimily (I# u)
\end{code}

