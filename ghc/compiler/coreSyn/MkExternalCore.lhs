%
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
import DataCon
import CoreSyn
import Var
import IdInfo
import Literal
import Name
import CostCentre
import Outputable
import ForeignCall
import PprExternalCore	
import CmdLineOpts
import IO

emitExternalCore :: DynFlags -> ModIface -> ModDetails -> IO ()
emitExternalCore dflags iface details 
 | opt_EmitExternalCore 
 = (do handle <- openFile corename WriteMode
       hPutStr handle (show (mkExternalCore iface details))      
       hClose handle)
   `catch` (\err -> pprPanic "Failed to open or write external core output file" 
	                     (text corename))
   where corename = extCoreName dflags
emitExternalCore _ _ _ 
 | otherwise
 = return ()


mkExternalCore :: ModIface -> ModDetails -> C.Module
mkExternalCore (ModIface {mi_module=mi_module,mi_exports=mi_exports}) 
	       (ModDetails {md_types=md_types,md_binds=md_binds}) =
    C.Module mname {- exports -} tdefs vdefs
  where
    mname = make_mid mi_module
{-  exports = foldr (collect_exports md_types) ([],[],[]) all_avails 
    all_avails = concat (map snd (filter ((== moduleName mi_module) . fst) mi_exports))
-}
    tdefs = foldr collect_tdefs [] (typeEnvTyCons md_types)
    vdefs = map make_vdef md_binds

{-
collect_exports :: TypeEnv -> AvailInfo -> ([C.Tcon],[C.Dcon],[C.Var]) -> ([C.Tcon],[C.Dcon],[C.Var])
collect_exports tyenv (Avail n) (tcons,dcons,vars) = (tcons,dcons,make_var_id n:vars) 	    
collect_exports tyenv (AvailTC n ns) (tcons,dcons,vars) = 
  case lookupNameEnv_NF tyenv n of
     ATyCon tc | isAlgTyCon tc -> 
         (tcon ++ tcons,workers ++ dcons,wrappers ++ vars)
         where 
           tcon = if elem n ns then [make_con_id n] else []
	   workers = if isNewTyCon tc then []
	             else map  (make_con_id . idName . dataConId) exported_dcs
	   exported_dcs = filter (\dc -> elem ((idName . dataConWrapId) dc) ns') dcs
	   dcs = tyConDataConsIfAvailable tc
	   wrappers = map make_var_id ns'
           ns' = filter (\n' -> n' /= n && not (elem n' recordSels)) ns
           recordSels = map idName (tyConSelIds tc)
     AClass cl ->  {- maybe a little too free about exports -}
        (tcon : tcons,workers ++ dcons,wrappers ++ vars)
        where 
          tcon = make_con_id (tyConName tc)
	  workers = if isNewTyCon tc then []
                    else map (make_con_id . idName . dataConId) dcs 
	  wrappers = map (make_var_id . idName . dataConWrapId) dcs
          dcs = tyConDataConsIfAvailable tc
          tc = classTyCon cl
     _ -> (tcons,dcons,vars)
-}


collect_tdefs :: TyCon -> [C.Tdef] -> [C.Tdef]
collect_tdefs tcon tdefs 
  | isAlgTyCon tcon = tdef : tdefs
  where
    tdef | isNewTyCon tcon
	 = C.Newtype (make_con_id (tyConName tcon)) (map make_tbind tyvars) (make_ty rep)
	 | otherwise
	 = C.Data (make_con_id (tyConName tcon)) (map make_tbind tyvars) (map make_cdef (tyConDataCons tcon)) 
    (_, rep) = newTyConRep tcon
    tyvars   = tyConTyVars tcon

collect_tdefs _ tdefs = tdefs


make_cdef :: DataCon -> C.Cdef
make_cdef dcon =  C.Constr dcon_name existentials tys
  where 
    dcon_name = make_con_id (idName (dataConId dcon))
    existentials = map make_tbind ex_tyvars
          where (_,_,ex_tyvars,_,_,_) = dataConSig dcon
    tys = map make_ty (dataConRepArgTys dcon)

make_tbind :: TyVar -> C.Tbind
make_tbind tv = (make_var_id (tyVarName tv), make_kind (tyVarKind tv))
    
make_vbind :: Var -> C.Vbind
make_vbind v = (make_var_id  (Var.varName v), make_ty (varType v))

make_vdef :: CoreBind -> (Bool, C.Vdefg)
make_vdef b = 
  case b of
    NonRec v e -> (isGlobalId v,C.Nonrec (f (v,e)))
    Rec ves -> (or (map g ves),C.Rec (map f ves))
  where f (v,e) = (n,t,make_exp e)
                  where (n,t) = make_vbind v
        g (v,e) = isGlobalId v

make_exp :: CoreExpr -> C.Exp
make_exp (Var v) =  
  case globalIdDetails v of
    DataConId _ -> C.Dcon (make_con_qid (Var.varName v))
    FCallId (CCall (CCallSpec (StaticTarget nm) _ _)) -> C.Ccall (_UNPK_ nm) (make_ty (varType v))
    _ -> C.Var (make_var_qid (Var.varName v))
make_exp (Lit l) = C.Lit (make_lit l)
make_exp (App e (Type t)) = C.Appt (make_exp e) (make_ty t)
make_exp (App e1 e2) = C.App (make_exp e1) (make_exp e2)
make_exp (Lam v e) | isTyVar v = C.Lam (C.Tb (make_tbind v)) (make_exp e)
make_exp (Lam v e) | otherwise = C.Lam (C.Vb (make_vbind v)) (make_exp e)
make_exp (Let b e) = C.Let (snd (make_vdef b)) (make_exp e)
make_exp (Case e v alts) = C.Case (make_exp e) (make_vbind v) (map make_alt alts)
make_exp (Note (SCC cc) e) = C.Note "SCC"  (make_exp e) -- temporary
make_exp (Note (Coerce t_to t_from) e) = C.Coerce (make_ty t_to) (make_exp e)
make_exp (Note InlineCall e) = C.Note "InlineCall" (make_exp e)
make_exp (Note InlineMe e) = C.Note "InlineMe" (make_exp e)
make_exp _ = error "MkExternalCore died: make_exp"

make_alt :: CoreAlt -> C.Alt
make_alt (DataAlt dcon, vs, e) = 
    C.Acon (make_con_qid (idName (dataConId dcon))) (map make_tbind tbs) (map make_vbind vbs) (make_exp e)
	where (tbs,vbs) = span isTyVar vs
make_alt (LitAlt l,_,e) = C.Alit (make_lit l) (make_exp e)
make_alt (DEFAULT,[],e) = C.Adefault (make_exp e)

make_lit :: Literal -> C.Lit
make_lit l = 
  case l of
    MachChar i -> C.Lchar (chr i) t
    MachStr s -> C.Lstring (_UNPK_ s) t
    MachAddr i -> C.Lint i t  
    MachInt i -> C.Lint i t
    MachInt64 i -> C.Lint i t
    MachWord i -> C.Lint i t
    MachWord64 i -> C.Lint i t
    MachFloat r -> C.Lrational r t
    MachDouble r -> C.Lrational r t
    MachLabel s -> C.Lstring (_UNPK_ s) t
    _ -> error "MkExternalCore died: make_lit"
  where 
    t = make_ty (literalType l)

make_ty :: Type -> C.Ty
make_ty (TyVarTy tv) = C.Tvar (make_var_id (tyVarName tv))
make_ty (AppTy t1 t2) = C.Tapp (make_ty t1) (make_ty t2)
make_ty (TyConApp tc ts) = foldl C.Tapp (C.Tcon (make_con_qid (tyConName tc))) (map make_ty ts)
make_ty (FunTy t1 t2) = make_ty (TyConApp funTyCon [t1,t2])
make_ty (ForAllTy tv t) = C.Tforall (make_tbind tv) (make_ty t)
make_ty (SourceTy p) = make_ty (sourceTypeRep p)
make_ty (UsageTy _ t) = make_ty t
make_ty (NoteTy _ t) = make_ty t


make_kind :: Kind -> C.Kind
make_kind (FunTy k1 k2) = C.Karrow (make_kind k1) (make_kind k2)
make_kind k | k `eqKind` liftedTypeKind = C.Klifted
make_kind k | k `eqKind` unliftedTypeKind = C.Kunlifted
make_kind k | k `eqKind` openTypeKind = C.Kopen
make_kind _ = error "MkExternalCore died: make_kind"

{- Id generation. -}

{- Use encoded strings, except restore non-leading '#'s.
   Also, adjust casing to work around some badly-chosen internal names. -}
make_id :: Bool -> Name -> C.Id
make_id is_var nm = 
  case n of
    c:cs -> if isUpper c && is_var then (toLower c):(decode cs) else (decode n)
  where n = (occNameString . nameOccName) nm
        decode ('z':'h':cs) = '#':(decode cs)
        decode (c:cs) = c:(decode cs)
        decode [] = []

make_var_id :: Name -> C.Id
make_var_id = make_id True

make_con_id :: Name -> C.Id
make_con_id = make_id False

make_mid :: Module -> C.Id
make_mid = moduleNameString . moduleName

make_qid :: Bool -> Name -> C.Qual C.Id
make_qid is_var n = (mname,make_id is_var n)
    where mname = 
           case nameModule_maybe n of
            Just m -> make_mid m
            Nothing -> ""   -- for now!

make_var_qid :: Name -> C.Qual C.Id
make_var_qid = make_qid True

make_con_qid :: Name -> C.Qual C.Id
make_con_qid = make_qid False

\end{code}




