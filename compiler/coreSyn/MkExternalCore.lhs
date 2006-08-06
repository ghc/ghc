
% (c) The University of Glasgow 2001
%
\begin{code}

module MkExternalCore (
	emitExternalCore
) where

#include "HsVersions.h"

import qualified ExternalCore as C
import Char
import Module
import CoreSyn
import HscTypes	
import TyCon
import TypeRep
import Type
import PprExternalCore	-- Instances
import DataCon	( DataCon, dataConExTyVars, dataConRepArgTys, 
		  dataConName, dataConTyCon )
import CoreSyn
import Var
import IdInfo
import Literal
import Name
import NameSet ( NameSet, emptyNameSet )
import UniqSet ( elementOfUniqSet )
import Outputable
import ForeignCall
import DynFlags	( DynFlags(..) )
import StaticFlags	( opt_EmitExternalCore )
import IO
import FastString

emitExternalCore :: DynFlags -> NameSet -> CgGuts -> IO ()
emitExternalCore dflags exports cg_guts
 | opt_EmitExternalCore 
 = (do handle <- openFile corename WriteMode
       hPutStrLn handle (show (mkExternalCore exports cg_guts))      
       hClose handle)
   `catch` (\err -> pprPanic "Failed to open or write external core output file" 
	                     (text corename))
   where corename = extCoreName dflags
emitExternalCore _ _ _
 | otherwise
 = return ()


mkExternalCore :: NameSet -> CgGuts -> C.Module
-- The ModGuts has been tidied, but the implicit bindings have
-- not been injected, so we have to add them manually here
-- We don't include the strange data-con *workers* because they are
-- implicit in the data type declaration itself
mkExternalCore exports (CgGuts {cg_module=this_mod, cg_tycons = tycons, cg_binds = binds})
  = C.Module mname tdefs (map (make_vdef exports) binds)
  where
    mname  = make_mid this_mod
    tdefs  = foldr collect_tdefs [] tycons

collect_tdefs :: TyCon -> [C.Tdef] -> [C.Tdef]
collect_tdefs tcon tdefs 
  | isAlgTyCon tcon = tdef: tdefs
  where
    tdef | isNewTyCon tcon = 
                C.Newtype (make_con_qid (tyConName tcon)) (map make_tbind tyvars) repclause 
-- 20060420 GHC handles empty data types just fine. ExtCore should too! jds
--         | null (tyConDataCons tcon) = error "MkExternalCore died: can't handle datatype declarations with no data constructors"
         | otherwise = 
                C.Data (make_con_qid (tyConName tcon)) (map make_tbind tyvars) (map make_cdef (tyConDataCons tcon)) 
         where repclause | isRecursiveTyCon tcon = Nothing
		         | otherwise = Just (make_ty rep)
                                           where (_, rep) = newTyConRep tcon
    tyvars = tyConTyVars tcon

collect_tdefs _ tdefs = tdefs


make_cdef :: DataCon -> C.Cdef
make_cdef dcon =  C.Constr dcon_name existentials tys
  where 
    dcon_name    = make_var_id (dataConName dcon)
    existentials = map make_tbind ex_tyvars
    ex_tyvars    = dataConExTyVars dcon
    tys 	 = map make_ty (dataConRepArgTys dcon)

make_tbind :: TyVar -> C.Tbind
make_tbind tv = (make_var_id (tyVarName tv), make_kind (tyVarKind tv))
    
make_vbind :: Var -> C.Vbind
make_vbind v = (make_var_id  (Var.varName v), make_ty (idType v))

make_vdef :: NameSet -> CoreBind -> C.Vdefg
make_vdef exports b = 
  case b of
    NonRec v e -> C.Nonrec (f (v,e))
    Rec ves -> C.Rec (map f ves)
  where
  f (v,e) = (local, make_var_id (Var.varName v), make_ty (idType v),make_exp e)
  	where local = not $ elementOfUniqSet (Var.varName v) exports
	-- Top level bindings are unqualified now

make_exp :: CoreExpr -> C.Exp
make_exp (Var v) =  
  case globalIdDetails v of
     -- a DataConId represents the Id of a worker, which is a varName. -- sof 4/02
--    DataConId _ -> C.Dcon (make_con_qid (Var.varName v))
    FCallId (CCall (CCallSpec (StaticTarget nm) _ _)) -> C.External (unpackFS nm) (make_ty (idType v))
    FCallId _ -> error "MkExternalCore died: can't handle non-static-C foreign call"
    _ -> C.Var (make_var_qid (Var.varName v))
make_exp (Lit (l@(MachLabel s _))) = error "MkExternalCore died: can't handle \"foreign label\" declarations"
make_exp (Lit l) = C.Lit (make_lit l)
make_exp (App e (Type t)) = C.Appt (make_exp e) (make_ty t)
make_exp (App e1 e2) = C.App (make_exp e1) (make_exp e2)
make_exp (Lam v e) | isTyVar v = C.Lam (C.Tb (make_tbind v)) (make_exp e)
make_exp (Lam v e) | otherwise = C.Lam (C.Vb (make_vbind v)) (make_exp e)
make_exp (Cast e co) = C.Cast (make_exp e) (make_ty co)
make_exp (Let b e) = C.Let (make_vdef emptyNameSet b) (make_exp e)
-- gaw 2004
make_exp (Case e v ty alts) = C.Case (make_exp e) (make_vbind v) (make_ty ty) (map make_alt alts)
make_exp (Note (SCC cc) e) = C.Note "SCC"  (make_exp e) -- temporary
make_exp (Note (CoreNote s) e) = C.Note s (make_exp e)  -- hdaume: core annotations
make_exp (Note InlineMe e) = C.Note "InlineMe" (make_exp e)
make_exp _ = error "MkExternalCore died: make_exp"

make_alt :: CoreAlt -> C.Alt
make_alt (DataAlt dcon, vs, e) = 
    C.Acon (make_con_qid (dataConName dcon))
           (map make_tbind tbs)
           (map make_vbind vbs)
	   (make_exp e)    
	where (tbs,vbs) = span isTyVar vs
make_alt (LitAlt l,_,e) = C.Alit (make_lit l) (make_exp e)
make_alt (DEFAULT,[],e) = C.Adefault (make_exp e)

make_lit :: Literal -> C.Lit
make_lit l = 
  case l of
    MachChar i -> C.Lchar i t
    MachStr s -> C.Lstring (unpackFS s) t
    MachNullAddr -> C.Lint 0 t
    MachInt i -> C.Lint i t
    MachInt64 i -> C.Lint i t
    MachWord i -> C.Lint i t
    MachWord64 i -> C.Lint i t
    MachFloat r -> C.Lrational r t
    MachDouble r -> C.Lrational r t
    _ -> error "MkExternalCore died: make_lit"
  where 
    t = make_ty (literalType l)

make_ty :: Type -> C.Ty
make_ty (TyVarTy tv)    	 = C.Tvar (make_var_id (tyVarName tv))
make_ty (AppTy t1 t2) 		 = C.Tapp (make_ty t1) (make_ty t2)
make_ty (FunTy t1 t2) 		 = make_ty (TyConApp funTyCon [t1,t2])
make_ty (ForAllTy tv t) 	 = C.Tforall (make_tbind tv) (make_ty t)
make_ty (TyConApp tc ts) 	 = foldl C.Tapp (C.Tcon (make_con_qid (tyConName tc))) 
					 (map make_ty ts)
-- Newtypes are treated just like any other type constructor; not expanded
-- Reason: predTypeRep does substitution and, while substitution deals
-- 	   correctly with name capture, it's only correct if you see the uniques!
--	   If you just see occurrence names, name capture may occur.
-- Example: newtype A a = A (forall b. b -> a)
--	    test :: forall q b. q -> A b
--	    test _ = undefined
-- 	Here the 'a' gets substituted by 'b', which is captured.
-- Another solution would be to expand newtypes before tidying; but that would
-- expose the representation in interface files, which definitely isn't right.
-- Maybe CoreTidy should know whether to expand newtypes or not?

make_ty (PredTy p)	= make_ty (predTypeRep p)
make_ty (NoteTy _ t) 	= make_ty t



make_kind :: Kind -> C.Kind
make_kind (FunTy k1 k2)  = C.Karrow (make_kind k1) (make_kind k2)
make_kind k
  | isLiftedTypeKind k   = C.Klifted
  | isUnliftedTypeKind k = C.Kunlifted
--   | isUnboxedTypeKind k  = C.Kunboxed	Fix me
  | isOpenTypeKind k     = C.Kopen
make_kind _ = error "MkExternalCore died: make_kind"

{- Id generation. -}

{- Use encoded strings.
   Also, adjust casing to work around some badly-chosen internal names. -}
make_id :: Bool -> Name -> C.Id
make_id is_var nm = (occNameString . nameOccName) nm

{-	SIMON thinks this stuff isn't necessary
make_id is_var nm = 
  case n of
    'Z':cs | is_var -> 'z':cs 
    'z':cs | not is_var -> 'Z':cs 
    c:cs | isUpper c && is_var -> 'z':'d':n
    c:cs | isLower c && (not is_var) -> 'Z':'d':n
    _ -> n
  where n = (occNameString . nameOccName) nm
-}

make_var_id :: Name -> C.Id
make_var_id = make_id True

make_mid :: Module -> C.Id
make_mid = showSDoc . pprModule

make_qid :: Bool -> Name -> C.Qual C.Id
make_qid is_var n = (mname,make_id is_var n)
    where mname = 
           case nameModule_maybe n of
            Just m -> make_mid m
            Nothing -> "" 

make_var_qid :: Name -> C.Qual C.Id
make_var_qid = make_qid True

make_con_qid :: Name -> C.Qual C.Id
make_con_qid = make_qid False

\end{code}




