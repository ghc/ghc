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
import SrcLoc	( unLoc, Located(..), SrcSpan )
import Type	( Type )
import TysWiredIn ( unitTyCon, tupleTyCon, trueDataCon )
import BasicTypes( Boxity(..) ) 
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
convertToHsDecls :: SrcSpan -> [TH.Dec] -> [Either (LHsDecl RdrName) Message]
-- Use the loc everywhere, for lack of anything better
-- In particular, we want it on binding locations, so that variables bound in
-- the spliced-in declarations get a location that at least relates to the splice point
convertToHsDecls loc ds = map (cvt_top loc) ds

cvt_top :: SrcSpan -> TH.Dec -> Either (LHsDecl RdrName) Message
cvt_top loc d@(TH.ValD _ _ _) = Left $ L loc $ Hs.ValD (unLoc (cvtd loc d))
cvt_top loc d@(TH.FunD _ _)   = Left $ L loc $ Hs.ValD (unLoc (cvtd loc d))
cvt_top loc (TH.SigD nm typ) = Left $ L loc $ Hs.SigD (TypeSig (L loc (vName nm)) (cvtType loc typ))
 
cvt_top loc (TySynD tc tvs rhs)
  = Left $ L loc $ TyClD (TySynonym (L loc (tconName tc)) (cvt_tvs loc tvs) (cvtType loc rhs))

cvt_top loc (DataD ctxt tc tvs constrs derivs)
  = Left $ L loc $ TyClD (mkTyData DataType 
                           (L loc (cvt_context loc ctxt, L loc (tconName tc), cvt_tvs loc tvs))
                           Nothing (map (mk_con loc) constrs)
                           (mk_derivs loc derivs))

cvt_top loc (NewtypeD ctxt tc tvs constr derivs)
  = Left $ L loc $ TyClD (mkTyData NewType 
                           (L loc (cvt_context loc ctxt, L loc (tconName tc), cvt_tvs loc tvs))
                           Nothing [mk_con loc constr]
                           (mk_derivs loc derivs))

cvt_top loc (ClassD ctxt cl tvs fds decs)
  = Left $ L loc $ TyClD $ mkClassDecl (cvt_context loc ctxt,
                                L loc (tconName cl),
                                cvt_tvs loc tvs)
                               (map (L loc . cvt_fundep) fds)
                               sigs
                               binds
  where
    (binds,sigs) = cvtBindsAndSigs loc decs

cvt_top loc (InstanceD tys ty decs)
  = Left $ L loc $ InstD (InstDecl (L loc inst_ty) binds sigs)
  where
    (binds, sigs) = cvtBindsAndSigs loc decs
    inst_ty = mkImplicitHsForAllTy (cvt_context loc tys) (L loc (HsPredTy (cvt_pred loc ty)))

cvt_top loc (ForeignD (ImportF callconv safety from nm typ))
 = case parsed of
       Just (c_header, cis) ->
           let i = CImport callconv' safety' c_header nilFS cis
           in Left $ L loc $ ForD (ForeignImport (L loc (vName nm)) (cvtType loc typ) i False)
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

cvt_top loc (ForeignD (ExportF callconv as nm typ))
 = let e = CExport (CExportStatic (mkFastString as) callconv')
   in Left $ L loc $ ForD (ForeignExport (L loc (vName nm)) (cvtType loc typ) e False)
    where callconv' = case callconv of
                          CCall -> CCallConv
                          StdCall -> StdCallConv

mk_con loc con = L loc $ mk_nlcon con
  where
	-- Can't handle GADTs yet
    mk_nlcon con = case con of
	NormalC c strtys
	 -> ConDecl (L loc (cName c)) Explicit noExistentials (noContext loc)
		    (PrefixCon (map mk_arg strtys)) ResTyH98
	RecC c varstrtys
	 -> ConDecl (L loc (cName c)) Explicit noExistentials (noContext loc)
		  (RecCon (map mk_id_arg varstrtys)) ResTyH98
	InfixC st1 c st2
	 -> ConDecl (L loc (cName c)) Explicit noExistentials (noContext loc)
		  (InfixCon (mk_arg st1) (mk_arg st2)) ResTyH98
	ForallC tvs ctxt (ForallC tvs' ctxt' con')
	 -> mk_nlcon (ForallC (tvs ++ tvs') (ctxt ++ ctxt') con')
	ForallC tvs ctxt con' -> case mk_nlcon con' of
				ConDecl l _ [] (L _ []) x ResTyH98 ->
				    ConDecl l Explicit (cvt_tvs loc tvs) (cvt_context loc ctxt) x ResTyH98
				c -> panic "ForallC: Can't happen"
    mk_arg (IsStrict, ty)  = L loc $ HsBangTy HsStrict (cvtType loc ty)
    mk_arg (NotStrict, ty) = cvtType loc ty

    mk_id_arg (i, IsStrict, ty)
        = (L loc (vName i), L loc $ HsBangTy HsStrict (cvtType loc ty))
    mk_id_arg (i, NotStrict, ty)
        = (L loc (vName i), cvtType loc ty)

mk_derivs loc [] = Nothing
mk_derivs loc cs = Just [L loc $ HsPredTy $ HsClassP (tconName c) [] | c <- cs]

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

noContext loc  = L loc []
noExistentials = []

-------------------------------------------------------------------
convertToHsExpr :: SrcSpan -> TH.Exp -> LHsExpr RdrName
convertToHsExpr loc e = cvtl loc e

cvtl loc e = cvt_l e
  where
    cvt_l e = L loc (cvt e)

    cvt (VarE s) 	= HsVar (vName s)
    cvt (ConE s) 	= HsVar (cName s)
    cvt (LitE l) 
      | overloadedLit l = HsOverLit (cvtOverLit l)
      | otherwise	= HsLit (cvtLit l)

    cvt (AppE x y)     = HsApp (cvt_l x) (cvt_l y)
    cvt (LamE ps e)    = HsLam (mkMatchGroup [mkSimpleMatch (map (cvtlp loc) ps) (cvtl loc e)])
    cvt (TupE [e])     = cvt e
    cvt (TupE es)      = ExplicitTuple(map cvt_l es) Boxed
    cvt (CondE x y z)  = HsIf (cvt_l x) (cvt_l y) (cvt_l z)
    cvt (LetE ds e)	   = HsLet (cvtdecs loc ds) (cvt_l e)
    cvt (CaseE e ms)   = HsCase (cvt_l e) (mkMatchGroup (map (cvtm loc) ms))
    cvt (DoE ss)       = cvtHsDo loc DoExpr ss
    cvt (CompE ss)     = cvtHsDo loc ListComp ss
    cvt (ArithSeqE dd) = ArithSeq noPostTcExpr (cvtdd loc dd)
    cvt (ListE xs)     = ExplicitList void (map cvt_l xs)
    cvt (InfixE (Just x) s (Just y))
        = HsPar (L loc $ OpApp (cvt_l x) (cvt_l s) undefined (cvt_l y))
    cvt (InfixE Nothing  s (Just y)) = SectionR (cvt_l s) (cvt_l y)
    cvt (InfixE (Just x) s Nothing ) = SectionL (cvt_l x) (cvt_l s)
    cvt (InfixE Nothing  s Nothing ) = cvt s	-- Can I indicate this is an infix thing?
    cvt (SigE e t)		= ExprWithTySig (cvt_l e) (cvtType loc t)
    cvt (RecConE c flds) = RecordCon (L loc (cName c)) noPostTcExpr
    				 (map (\(x,y) -> (L loc (vName x), cvt_l y)) flds)
    cvt (RecUpdE e flds) = RecordUpd (cvt_l e) (map (\(x,y) -> (L loc (vName x), cvt_l y)) flds)
				 placeHolderType placeHolderType

cvtHsDo loc do_or_lc stmts
  = HsDo do_or_lc (init stmts') body void
  where
    stmts' = cvtstmts loc stmts
    body = case last stmts' of
		L _ (ExprStmt body _ _) -> body

cvtdecs :: SrcSpan -> [TH.Dec] -> HsLocalBinds RdrName
cvtdecs loc [] = EmptyLocalBinds
cvtdecs loc ds = HsValBinds (ValBindsIn binds sigs)
	   where
	     (binds, sigs) = cvtBindsAndSigs loc ds

cvtBindsAndSigs loc ds 
  = (cvtds loc non_sigs, map (cvtSig loc) sigs)
  where 
    (sigs, non_sigs) = partition sigP ds

cvtSig loc (TH.SigD nm typ) = L loc (Hs.TypeSig (L loc (vName nm)) (cvtType loc typ))

cvtds :: SrcSpan -> [TH.Dec] -> LHsBinds RdrName
cvtds loc []     = emptyBag
cvtds loc (d:ds) = cvtd loc d `consBag` cvtds loc ds

cvtd :: SrcSpan -> TH.Dec -> LHsBind RdrName
-- Used only for declarations in a 'let/where' clause,
-- not for top level decls
cvtd loc (TH.ValD (TH.VarP s) body ds) 
  = L loc $ FunBind (L loc (vName s)) False 
		    (mkMatchGroup [cvtclause loc (Clause [] body ds)])
		    placeHolderNames
cvtd loc (FunD nm cls)
  = L loc $ FunBind (L loc (vName nm)) False 
		    (mkMatchGroup (map (cvtclause loc) cls))
		    placeHolderNames
cvtd loc (TH.ValD p body ds)
  = L loc $ PatBind (cvtlp loc p) (GRHSs (cvtguard loc body) (cvtdecs loc ds)) 
		    void placeHolderNames

cvtd loc d = cvtPanic "Illegal kind of declaration in where clause" 
		  (text (TH.pprint d))


cvtclause :: SrcSpan -> TH.Clause -> Hs.LMatch RdrName
cvtclause loc (Clause ps body wheres)
    = L loc $ Hs.Match (map (cvtlp loc) ps) Nothing (GRHSs (cvtguard loc body) (cvtdecs loc wheres))



cvtdd :: SrcSpan -> Range -> ArithSeqInfo RdrName
cvtdd loc (FromR x) 	      = (From (cvtl loc x))
cvtdd loc (FromThenR x y)     = (FromThen (cvtl loc x) (cvtl loc y))
cvtdd loc (FromToR x y)	      = (FromTo (cvtl loc x) (cvtl loc y))
cvtdd loc (FromThenToR x y z) = (FromThenTo (cvtl loc x) (cvtl loc y) (cvtl loc z))


cvtstmts :: SrcSpan -> [TH.Stmt] -> [Hs.LStmt RdrName]
cvtstmts loc []		     = []
cvtstmts loc (NoBindS e : ss)    = L loc (mkExprStmt (cvtl loc e))	     : cvtstmts loc ss
cvtstmts loc (TH.BindS p e : ss) = L loc (mkBindStmt (cvtlp loc p) (cvtl loc e)) : cvtstmts loc ss
cvtstmts loc (TH.LetS ds : ss)   = L loc (LetStmt (cvtdecs loc ds))	     : cvtstmts loc ss
cvtstmts loc (TH.ParS dss : ss)  = L loc (ParStmt [(cvtstmts loc ds, undefined) | ds <- dss]) : cvtstmts loc ss

cvtm :: SrcSpan -> TH.Match -> Hs.LMatch RdrName
cvtm loc (TH.Match p body wheres)
    = L loc (Hs.Match [cvtlp loc p] Nothing (GRHSs (cvtguard loc body) (cvtdecs loc wheres)))

cvtguard :: SrcSpan -> TH.Body -> [LGRHS RdrName]
cvtguard loc (GuardedB pairs) = map (cvtpair loc) pairs
cvtguard loc (NormalB e)      = [L loc (GRHS [] (cvtl loc e))]

cvtpair :: SrcSpan -> (TH.Guard,TH.Exp) -> LGRHS RdrName
cvtpair loc (NormalG x,y) = L loc (GRHS [L loc $ mkBindStmt truePat (cvtl loc x)]
				    (cvtl loc y))
cvtpair loc (PatG x,y) = L loc (GRHS (cvtstmts loc x) (cvtl loc y))

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

cvtlp :: SrcSpan -> TH.Pat -> Hs.LPat RdrName
cvtlp loc pat = L loc (cvtp loc pat)

cvtp :: SrcSpan -> TH.Pat -> Hs.Pat RdrName
cvtp loc (TH.LitP l)
  | overloadedLit l = mkNPat (cvtOverLit l) Nothing	-- Not right for negative
							-- patterns; need to think
							-- about that!
  | otherwise	    = Hs.LitPat (cvtLit l)
cvtp loc (TH.VarP s)  = Hs.VarPat(vName s)
cvtp loc (TupP [p])   = cvtp loc p
cvtp loc (TupP ps)    = TuplePat (map (cvtlp loc) ps) Boxed
cvtp loc (ConP s ps)  = ConPatIn (L loc (cName s)) (PrefixCon (map (cvtlp loc) ps))
cvtp loc (InfixP p1 s p2)
                  = ConPatIn (L loc (cName s)) (InfixCon (cvtlp loc p1) (cvtlp loc p2))
cvtp loc (TildeP p)   = LazyPat (cvtlp loc p)
cvtp loc (TH.AsP s p) = AsPat (L loc (vName s)) (cvtlp loc p)
cvtp loc TH.WildP   = WildPat void
cvtp loc (RecP c fs)  = ConPatIn (L loc (cName c)) $ Hs.RecCon (map (\(s,p) -> (L loc (vName s),cvtlp loc p)) fs)
cvtp loc (ListP ps)   = ListPat (map (cvtlp loc) ps) void
cvtp loc (SigP p t)   = SigPatIn (cvtlp loc p) (cvtType loc t)

-----------------------------------------------------------
--	Types and type variables

cvt_tvs :: SrcSpan -> [TH.Name] -> [LHsTyVarBndr RdrName]
cvt_tvs loc tvs = map (L loc . UserTyVar . tName) tvs

cvt_context :: SrcSpan -> Cxt -> LHsContext RdrName 
cvt_context loc tys = L loc (map (L loc . cvt_pred loc) tys)

cvt_pred :: SrcSpan -> TH.Type -> HsPred RdrName
cvt_pred loc ty 
  = case split_ty_app ty of
	(ConT tc, tys) -> HsClassP (tconName tc) (map (cvtType loc) tys)
	(VarT tv, tys) -> HsClassP (tName tv) (map (cvtType loc) tys)
	other -> cvtPanic "Malformed predicate" (text (TH.pprint ty))

convertToHsType = cvtType

cvtType :: SrcSpan -> TH.Type -> LHsType RdrName
cvtType loc ty = trans (root ty [])
  where root (AppT a b) zs = root a (cvtType loc b : zs)
        root t zs 	   = (t,zs)

        trans (TupleT n,args)
            | length args == n = L loc (HsTupleTy Boxed args)
            | n == 0    = foldl nlHsAppTy (nlHsTyVar (getRdrName unitTyCon))	    args
            | otherwise = foldl nlHsAppTy (nlHsTyVar (getRdrName (tupleTyCon Boxed n))) args
        trans (ArrowT,   [x,y]) = nlHsFunTy x y
        trans (ListT,    [x])   = L loc (HsListTy x)

	trans (VarT nm, args)	    = foldl nlHsAppTy (nlHsTyVar (tName nm))    args
        trans (ConT tc, args)       = foldl nlHsAppTy (nlHsTyVar (tconName tc)) args

	trans (ForallT tvs cxt ty, []) = L loc $ mkExplicitHsForAllTy 
						(cvt_tvs loc tvs) (cvt_context loc cxt) (cvtType loc ty)

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

