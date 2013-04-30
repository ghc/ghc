
% (c) The University of Glasgow 2001-2006
%
\begin{code}
module MkExternalCore (
        emitExternalCore
) where

#include "HsVersions.h"

import qualified ExternalCore as C
import Module
import CoreSyn
import HscTypes
import TyCon
import CoAxiom
-- import Class
import TypeRep
import Type
import Kind
import PprExternalCore () -- Instances
import DataCon
import Coercion
import Var
import IdInfo
import Literal
import Name
import Outputable
import Encoding
import ForeignCall
import DynFlags
import FastString
import Exception

import Control.Monad
import qualified Data.ByteString as BS
import Data.Char
import System.IO

emitExternalCore :: DynFlags -> FilePath -> CgGuts -> IO ()
emitExternalCore dflags extCore_filename cg_guts
 | gopt Opt_EmitExternalCore dflags
 = (do handle <- openFile extCore_filename WriteMode
       hPutStrLn handle (show (mkExternalCore dflags cg_guts))
       hClose handle)
   `catchIO` (\_ -> pprPanic "Failed to open or write external core output file"
                             (text extCore_filename))
emitExternalCore _ _ _
 | otherwise
 = return ()

-- Reinventing the Reader monad; whee.
newtype CoreM a = CoreM (CoreState -> (CoreState, a))
data CoreState = CoreState {
                     cs_dflags :: DynFlags,
                     cs_module :: Module
                 }
instance Monad CoreM where
  (CoreM m) >>= f = CoreM (\ s -> case m s of
                                    (s',r) -> case f r of
                                                CoreM f' -> f' s')
  return x = CoreM (\ s -> (s, x))
runCoreM :: CoreM a -> CoreState -> a
runCoreM (CoreM f) s = snd $ f s
ask :: CoreM CoreState
ask = CoreM (\ s -> (s,s))

instance HasDynFlags CoreM where
    getDynFlags = liftM cs_dflags ask

mkExternalCore :: DynFlags -> CgGuts -> C.Module
-- The ModGuts has been tidied, but the implicit bindings have
-- not been injected, so we have to add them manually here
-- We don't include the strange data-con *workers* because they are
-- implicit in the data type declaration itself
mkExternalCore dflags (CgGuts {cg_module=this_mod, cg_tycons = tycons,
                               cg_binds = binds})
{- Note that modules can be mutually recursive, but even so, we
   print out dependency information within each module. -}
  = C.Module (mname dflags) tdefs (runCoreM (mapM (make_vdef True) binds) initialState)
  where
    initialState = CoreState {
                       cs_dflags = dflags,
                       cs_module = this_mod
                   }
    mname dflags = make_mid dflags this_mod
    tdefs  = foldr (collect_tdefs dflags) [] tycons

collect_tdefs :: DynFlags -> TyCon -> [C.Tdef] -> [C.Tdef]
collect_tdefs dflags tcon tdefs
  | isAlgTyCon tcon = tdef: tdefs
  where
    tdef | isNewTyCon tcon =
                C.Newtype (qtc dflags tcon)
                  (qcc dflags (newTyConCo tcon))
                  (map make_tbind tyvars)
                  (make_ty dflags (snd (newTyConRhs tcon)))
         | otherwise =
                C.Data (qtc dflags tcon) (map make_tbind tyvars)
                   (map (make_cdef dflags) (tyConDataCons tcon))
    tyvars = tyConTyVars tcon

collect_tdefs _ _ tdefs = tdefs

qtc :: DynFlags -> TyCon -> C.Qual C.Tcon
qtc dflags = make_con_qid dflags . tyConName

qcc :: DynFlags -> CoAxiom br -> C.Qual C.Tcon
qcc dflags = make_con_qid dflags . co_ax_name

make_cdef :: DynFlags -> DataCon -> C.Cdef
make_cdef dflags dcon =  C.Constr dcon_name existentials tys
  where
    dcon_name    = make_qid dflags False False (dataConName dcon)
    existentials = map make_tbind ex_tyvars
    ex_tyvars    = dataConExTyVars dcon
    tys          = map (make_ty dflags) (dataConRepArgTys dcon)

make_tbind :: TyVar -> C.Tbind
make_tbind tv = (make_var_id (tyVarName tv), make_kind (tyVarKind tv))

make_vbind :: DynFlags -> Var -> C.Vbind
make_vbind dflags v = (make_var_id  (Var.varName v), make_ty dflags (varType v))

make_vdef :: Bool -> CoreBind -> CoreM C.Vdefg
make_vdef topLevel b =
  case b of
    NonRec v e -> f (v,e)     >>= (return . C.Nonrec)
    Rec ves    -> mapM f ves  >>= (return . C.Rec)
  where
  f :: (CoreBndr,CoreExpr) -> CoreM C.Vdef
  f (v,e) = do
          localN <- isALocal vName
          let local = not topLevel || localN
          rhs <- make_exp e
          -- use local flag to determine where to add the module name
          dflags <- getDynFlags
          return (local, make_qid dflags local True vName, make_ty dflags (varType v),rhs)
        where vName = Var.varName v

make_exp :: CoreExpr -> CoreM C.Exp
make_exp (Var v) = do
  let vName = Var.varName v
  isLocal <- isALocal vName
  dflags <- getDynFlags
  return $
     case idDetails v of
       FCallId (CCall (CCallSpec (StaticTarget nm _ True) callconv _))
           -> C.External (unpackFS nm) (showPpr dflags callconv) (make_ty dflags (varType v))
       FCallId (CCall (CCallSpec (StaticTarget _ _ False) _ _)) ->
           panic "make_exp: FFI values not supported"
       FCallId (CCall (CCallSpec DynamicTarget     callconv _))
           -> C.DynExternal            (showPpr dflags callconv) (make_ty dflags (varType v))
       -- Constructors are always exported, so make sure to declare them
       -- with qualified names
       DataConWorkId _ -> C.Var (make_var_qid dflags False vName)
       DataConWrapId _ -> C.Var (make_var_qid dflags False vName)
       _ -> C.Var (make_var_qid dflags isLocal vName)
make_exp (Lit (MachLabel s _ _)) = return $ C.Label (unpackFS s)
make_exp (Lit l) = do dflags <- getDynFlags
                      return $ C.Lit (make_lit dflags l)
make_exp (App e (Type t)) = do b <- make_exp e
                               dflags <- getDynFlags
                               return $ C.Appt b (make_ty dflags t)
make_exp (App _e (Coercion _co)) = error "make_exp (App _ (Coercion _))"    -- TODO
make_exp (App e1 e2) = do
   rator <- make_exp e1
   rand <- make_exp e2
   return $ C.App rator rand
make_exp (Lam v e) | isTyVar v = make_exp e >>= (\ b ->
                                    return $ C.Lam (C.Tb (make_tbind v)) b)
make_exp (Lam v e) | otherwise = do b <- make_exp e
                                    dflags <- getDynFlags
                                    return $ C.Lam (C.Vb (make_vbind dflags v)) b
make_exp (Cast e co) = do b <- make_exp e
                          dflags <- getDynFlags
                          return $ C.Cast b (make_co dflags co)
make_exp (Let b e) = do
  vd   <- make_vdef False b
  body <- make_exp e
  return $ C.Let vd body
make_exp (Case e v ty alts) = do
  scrut <- make_exp e
  newAlts  <- mapM make_alt alts
  dflags <- getDynFlags
  return $ C.Case scrut (make_vbind dflags v) (make_ty dflags ty) newAlts
make_exp (Tick _ e) = make_exp e >>= (return . C.Tick "SCC") -- temporary
make_exp _ = error "MkExternalCore died: make_exp"

make_alt :: CoreAlt -> CoreM C.Alt
make_alt (DataAlt dcon, vs, e) = do
    newE <- make_exp e
    dflags <- getDynFlags
    return $ C.Acon (make_con_qid dflags (dataConName dcon))
           (map make_tbind tbs)
           (map (make_vbind dflags) vbs)
           newE
        where (tbs,vbs) = span isTyVar vs
make_alt (LitAlt l,_,e)   = do x <- make_exp e
                               dflags <- getDynFlags
                               return $ C.Alit (make_lit dflags l) x
make_alt (DEFAULT,[],e)   = make_exp e >>= (return . C.Adefault)
-- This should never happen, as the DEFAULT alternative binds no variables,
-- but we might as well check for it:
make_alt a@(DEFAULT,_ ,_) = pprPanic ("MkExternalCore: make_alt: DEFAULT "
             ++ "alternative had a non-empty var list") (ppr a)


make_lit :: DynFlags -> Literal -> C.Lit
make_lit dflags l =
  case l of
    -- Note that we need to check whether the character is "big".
    -- External Core only allows character literals up to '\xff'.
    MachChar i | i <= chr 0xff -> C.Lchar i t
    -- For a character bigger than 0xff, we represent it in ext-core
    -- as an int lit with a char type.
    MachChar i             -> C.Lint (fromIntegral $ ord i) t
    MachStr s -> C.Lstring (BS.unpack s) t
    MachNullAddr -> C.Lint 0 t
    MachInt i -> C.Lint i t
    MachInt64 i -> C.Lint i t
    MachWord i -> C.Lint i t
    MachWord64 i -> C.Lint i t
    MachFloat r -> C.Lrational r t
    MachDouble r -> C.Lrational r t
    LitInteger i _ -> C.Lint i t
    _ -> pprPanic "MkExternalCore died: make_lit" (ppr l)
  where
    t = make_ty dflags (literalType l)

-- Expand type synonyms, then convert.
make_ty :: DynFlags -> Type -> C.Ty     -- Be sure to expand types recursively!
                                        -- example: FilePath ~> String ~> [Char]
make_ty dflags t | Just expanded <- tcView t = make_ty dflags expanded
make_ty dflags t = make_ty' dflags t

-- note calls to make_ty so as to expand types recursively
make_ty' :: DynFlags -> Type -> C.Ty
make_ty' _      (TyVarTy tv)     = C.Tvar (make_var_id (tyVarName tv))
make_ty' dflags (AppTy t1 t2)     = C.Tapp (make_ty dflags t1) (make_ty dflags t2)
make_ty' dflags (FunTy t1 t2)     = make_ty dflags (TyConApp funTyCon [t1,t2])
make_ty' dflags (ForAllTy tv t)  = C.Tforall (make_tbind tv) (make_ty dflags t)
make_ty' dflags (TyConApp tc ts) = make_tyConApp dflags tc ts
make_ty' _      (LitTy {})       = panic "MkExernalCore can't do literal types yet"

-- Newtypes are treated just like any other type constructor; not expanded
-- Reason: predTypeRep does substitution and, while substitution deals
--         correctly with name capture, it's only correct if you see the uniques!
--         If you just see occurrence names, name capture may occur.
-- Example: newtype A a = A (forall b. b -> a)
--          test :: forall q b. q -> A b
--          test _ = undefined
--      Here the 'a' gets substituted by 'b', which is captured.
-- Another solution would be to expand newtypes before tidying; but that would
-- expose the representation in interface files, which definitely isn't right.
-- Maybe CoreTidy should know whether to expand newtypes or not?

make_tyConApp :: DynFlags -> TyCon -> [Type] -> C.Ty
make_tyConApp dflags tc ts =
  foldl C.Tapp (C.Tcon (qtc dflags tc))
            (map (make_ty dflags) ts)

make_kind :: Kind -> C.Kind
make_kind (FunTy k1 k2)  = C.Karrow (make_kind k1) (make_kind k2)
make_kind k
  | isLiftedTypeKind k   = C.Klifted
  | isUnliftedTypeKind k = C.Kunlifted
  | isOpenTypeKind k     = C.Kopen
make_kind _ = error "MkExternalCore died: make_kind"

{- Id generation. -}

make_id :: Bool -> Name -> C.Id
-- include uniques for internal names in order to avoid name shadowing
make_id _is_var nm = ((occNameString . nameOccName) nm)
  ++ (if isInternalName nm then (show . nameUnique) nm else "")

make_var_id :: Name -> C.Id
make_var_id = make_id True

-- It's important to encode the module name here, because in External Core,
-- base:GHC.Base => base:GHCziBase
-- We don't do this in pprExternalCore because we
-- *do* want to keep the package name (we don't want baseZCGHCziBase,
-- because that would just be ugly.)
-- SIGH.
-- We encode the package name as well.
make_mid :: DynFlags -> Module -> C.Id
-- Super ugly code, but I can't find anything else that does quite what I
-- want (encodes the hierarchical module name without encoding the colon
-- that separates the package name from it.)
make_mid dflags m
            = showSDoc dflags $
              (text $ zEncodeString $ packageIdString $ modulePackageId m)
              <> text ":"
              <> (pprEncoded $ pprModuleName $ moduleName m)
     where pprEncoded = pprCode CStyle

make_qid :: DynFlags -> Bool -> Bool -> Name -> C.Qual C.Id
make_qid dflags force_unqual is_var n = (mname,make_id is_var n)
    where mname =
           case nameModule_maybe n of
            Just m | not force_unqual -> make_mid dflags m
            _ -> ""

make_var_qid :: DynFlags -> Bool -> Name -> C.Qual C.Id
make_var_qid dflags force_unqual = make_qid dflags force_unqual True

make_con_qid :: DynFlags -> Name -> C.Qual C.Id
make_con_qid dflags = make_qid dflags False False

make_co :: DynFlags -> Coercion -> C.Ty
make_co dflags (Refl ty)             = make_ty dflags ty
make_co dflags (TyConAppCo tc cos)   = make_conAppCo dflags (qtc dflags tc) cos
make_co dflags (AppCo c1 c2)         = C.Tapp (make_co dflags c1) (make_co dflags c2)
make_co dflags (ForAllCo tv co)      = C.Tforall (make_tbind tv) (make_co dflags co)
make_co _      (CoVarCo cv)          = C.Tvar (make_var_id (coVarName cv))
make_co dflags (AxiomInstCo cc ind cos) = C.AxiomCoercion (qcc dflags cc) ind (map (make_co dflags) cos)
make_co dflags (UnsafeCo t1 t2)      = C.UnsafeCoercion (make_ty dflags t1) (make_ty dflags t2)
make_co dflags (SymCo co)            = C.SymCoercion (make_co dflags co)
make_co dflags (TransCo c1 c2)       = C.TransCoercion (make_co dflags c1) (make_co dflags c2)
make_co dflags (NthCo d co)          = C.NthCoercion d (make_co dflags co)
make_co dflags (LRCo lr co)          = C.LRCoercion (make_lr lr) (make_co dflags co)
make_co dflags (InstCo co ty)        = C.InstCoercion (make_co dflags co) (make_ty dflags ty)

make_lr :: LeftOrRight -> C.LeftOrRight
make_lr CLeft  = C.CLeft
make_lr CRight = C.CRight

-- Used for both tycon app coercions and axiom instantiations.
make_conAppCo :: DynFlags -> C.Qual C.Tcon -> [Coercion] -> C.Ty
make_conAppCo dflags con cos =
  foldl C.Tapp (C.Tcon con)
            (map (make_co dflags) cos)

-------
isALocal :: Name -> CoreM Bool
isALocal vName = do
  modName <- liftM cs_module ask
  return $ case nameModule_maybe vName of
             -- Not sure whether isInternalName corresponds to "local"ness
             -- in the External Core sense; need to re-read the spec.
             Just m | m == modName -> isInternalName vName
             _                     -> False
\end{code}




