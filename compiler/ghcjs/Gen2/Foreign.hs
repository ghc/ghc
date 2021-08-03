{-# LANGUAGE CPP, FlexibleContexts, TypeFamilies #-}
{-
  This module takes over desugaring and typechecking foreign declarations and calls
  from GHC. foreign import javascript should be desugared differently
  from other foreign imports since we don't want Bool to de be marshalled through
  0/1 for example.

  Contains code adapted from DsForeign, DsCCall and TcForeign
 -}

module Gen2.Foreign where

import Prelude hiding ((<>))

import Control.Monad

import Data.Maybe
import Data.List (unzip4, stripPrefix)
import qualified Data.Text as T

import Hooks
import DynFlags

import GHC.Hs.Extension
import Id
import OrdList
import Name
import Bag
import CoreSyn
import ErrUtils
import HscTypes
import GHC.Hs.Binds
import GHC.Hs.Decls
import Gen2.GHC.DsForeign (dsForeigns', dsPrimCall)
import DsMonad
import Encoding
import GHC.Hs.Utils
import TcEnv
import TcExpr
import TcRnTypes
import TcForeign
import MonadUtils
import RdrName
import FastString
import ForeignCall
import SrcLoc
import Unique
import PrelNames
import PrimOp
import MkId
import TyCon
import DataCon
import Outputable
import Coercion
import Type
import RepType
import TysWiredIn
import TysPrim
import CoreUtils
import MkCore
import DsCCall
import BasicTypes
import CoreUnfold
import Pair
import Literal
import Module
import TcType
import TcRnMonad
import TcHsType
import GHC.Platform
import CmmType
import DsUtils
import CmmUtils hiding (mkIntExpr)

import Compiler.Compat
import Compiler.JMacro.Base (encodeJson)

import GHC.LanguageExtensions

type Binding = (Id, CoreExpr)

installForeignHooks :: Bool -> DynFlags -> DynFlags
installForeignHooks generatingJs dflags =
  dflags { hooks = f generatingJs $ hooks dflags }
    where
      f True h  = h { dsForeignsHook       = Just ghcjsDsForeigns
                    , tcForeignImportsHook = Just ghcjsTcForeignImports
                    , tcForeignExportsHook = Just ghcjsTcForeignExports
                    }
      f False h = h { dsForeignsHook       = Just ghcjsNativeDsForeigns
                    , tcForeignImportsHook = Just ghcjsNativeTcForeignImports
                    , tcForeignExportsHook = Just ghcjsNativeTcForeignExports
                    }
{-
   desugar foreign declarations for JavaScript
-}

ghcjsDsForeigns :: [LForeignDecl GhcTc]
                -> DsM (ForeignStubs, OrdList Binding)
ghcjsDsForeigns []
  = return (NoStubs, nilOL)
ghcjsDsForeigns fos = do
    fives <- mapM do_ldecl fos
    let
        (hs, cs, idss, bindss) = unzip4 fives
        fe_ids = concat idss
    return (ForeignStubs
             (vcat hs)
             (vcat cs),
            foldr (appOL . toOL) nilOL bindss)
  where
   do_ldecl (L loc decl) = putSrcSpanDs loc (do_decl decl)

   do_decl ForeignImport { fd_name = id, fd_i_ext = co, fd_fi = spec } = do
      traceIf (text "fi start" <+> ppr id)
      let id' = unLoc id
      (bs, h, c) <- ghcjsDsFImport id' co spec
      traceIf (text "fi end" <+> ppr id)
      return (h, c, [], bs)

   do_decl ForeignExport { fd_name = L _ id, fd_e_ext = co
                         , fd_fe = CExport (L _ (CExportStatic _ ext_nm cconv)) _ } = do
      (h, c, _, _) <- ghcjsDsFExport id co ext_nm cconv False
      return (h, c, [id], [])
   do_decl (XForeignDecl _) = panic "dsForeigns'"

ghcjsDsFExport :: Id                 -- Either the exported Id,
                                -- or the foreign-export-dynamic constructor
          -> Coercion           -- Coercion between the Haskell type callable
                                -- from C, and its representation type
          -> CLabelString       -- The name to export to C land
          -> CCallConv
          -> Bool               -- True => foreign export dynamic
                                --         so invoke IO action that's hanging off
                                --         the first argument's stable pointer
          -> DsM ( SDoc         -- contents of Module_stub.h
                 , SDoc         -- contents of Module_stub.c
                 , String       -- string describing type to pass to createAdj.
                 , Int          -- size of args to stub function
                 )

ghcjsDsFExport fn_id co ext_name cconv isDyn = do
    let
       ty                              = pSnd $ coercionKind co
       (_tvs,sans_foralls)             = tcSplitForAllTys ty
       (fe_arg_tys', orig_res_ty)      = tcSplitFunTys sans_foralls
       -- We must use tcSplits here, because we want to see
       -- the (IO t) in the corner of the type!
       fe_arg_tys | isDyn     = tail fe_arg_tys'
                  | otherwise = fe_arg_tys'

       -- Look at the result type of the exported function, orig_res_ty
       -- If it's IO t, return         (t, True)
       -- If it's plain t, return      (t, False)
       (res_ty, is_IO_res_ty) = case tcSplitIOType_maybe orig_res_ty of
                                -- The function already returns IO t
                                Just (_ioTyCon, res_ty) -> (res_ty, True)
                                -- The function returns t
                                Nothing                 -> (orig_res_ty, False)
    dflags <- getDynFlags
    return $
      mkFExportJSBits dflags ext_name
                     (if isDyn then Nothing else Just fn_id)
                     fe_arg_tys res_ty is_IO_res_ty cconv

mkFExportJSBits :: DynFlags
               -> FastString
               -> Maybe Id      -- Just==static, Nothing==dynamic
               -> [Type]
               -> Type
               -> Bool          -- True <=> returns an IO type
               -> CCallConv
               -> (SDoc,
                   SDoc,
                   String,      -- the argument reps
                   Int          -- total size of arguments
                  )
mkFExportJSBits dflags c_nm maybe_target arg_htys res_hty is_IO_res_ty cc
 = (header_bits, js_bits, type_string,
    sum [ widthInBytes (typeWidth rep) | (_,_,_,rep) <- arg_info] -- all the args
         -- NB. the calculation here isn't strictly speaking correct.
         -- We have a primitive Haskell type (eg. Int#, Double#), and
         -- we want to know the size, when passed on the C stack, of
         -- the associated C type (eg. HsInt, HsDouble).  We don't have
         -- this information to hand, but we know what GHC's conventions
         -- are for passing around the primitive Haskell types, so we
         -- use that instead.  I hope the two coincide --SDM
    )
 where
  -- list the arguments to the JS function
  arg_info :: [(SDoc,           -- arg name
                SDoc,           -- C type
                Type,           -- Haskell type
                CmmType)]       -- the CmmType
  arg_info  = [ let stg_type = showStgType ty in
                (arg_cname n stg_type,
                 stg_type,
                 ty,
                 typeCmmType dflags (getPrimTyOf ty))
              | (ty,n) <- zip arg_htys [1::Int ..] ]

  arg_cname n stg_ty = text ('a':show n)

  type_string = primTyDescChar dflags res_hty : arg_type_string

  arg_type_string = [primTyDescChar dflags ty | (_,_,ty,_) <- arg_info]

  -- stuff to do with the return type of the JS function
  res_hty_is_unit = res_hty `eqType` unitTy     -- Look through any newtypes

  unboxResType | res_hty_is_unit = text "h$rts_getUnit"
               | otherwise       = unpackHObj res_hty

  header_bits = maybe empty idTag maybe_target
  idTag i = let (tag, u) = unpkUnique (getUnique i)
            in  char tag <> int u

  fun_args
    | null arg_info = empty -- text "void"
    | otherwise         = hsep $ punctuate comma
                               $ map (\(nm,_ty,_,_) -> nm) arg_info

  fun_proto
      = text "async" <+>
        text "function" <+>
        (if isNothing maybe_target
         then text "h$" <> ftext c_nm
         else ftext c_nm) <>
        parens fun_args

  fun_export
     = case maybe_target of
          Just hs_fn | Just m <- nameModule_maybe (getName hs_fn) ->
            text "h$foreignExport" <>
                        parens (
                          ftext c_nm <> comma <>
                          strlit (installedUnitIdString (toInstalledUnitId $ moduleUnitId m)) <> comma <>
                          strlit (moduleNameString (moduleName m)) <> comma <>
                          strlit (unpackFS c_nm) <> comma <>
                          strlit type_string
                        ) <> semi
          _ -> empty

  strlit xs = text "\"" <>
              text (T.unpack . encodeJson . T.pack $ xs) <>
              text "\""

  -- the target which will form the root of what we ask rts_evalIO to run
  the_cfun
     = case maybe_target of
          Nothing    -> text "h$deRefStablePtr(the_stableptr)"
          Just hs_fn -> idClosureText dflags hs_fn

  -- the expression we give to rts_eval
  expr_to_run :: SDoc
  expr_to_run
     = foldl appArg the_cfun arg_info
       where
          appArg acc (arg_cname, _, arg_hty, _)
             = text "h$rts_apply"
               <> parens (acc <> comma <> mkHObj arg_hty <> parens arg_cname)

  -- finally, the whole darn thing
  js_bits =
    space $$
    fun_proto  $$
    vcat
     [ lbrace
     ,   text "return" <+> text "await" <+>
         ptext (sLit "h$rts_eval") <> parens (
                (if is_IO_res_ty
                       then expr_to_run
                       else ptext (sLit "h$rts_toIO") <> parens expr_to_run)
                 <> comma <+> unboxResType
             ) <> semi
     , rbrace
     ] $$
     fun_export

idClosureText :: DynFlags -> Id -> SDoc
idClosureText df i
  | isExportedId i, Just m <- nameModule_maybe (getName i)
     = let modName :: String
           modName = moduleNameString (moduleName m)
           name :: String
           name    = showPpr df . localiseName . getName $ i
           pkg :: String
           pkg     = installedUnitIdString (toInstalledUnitId $ moduleUnitId m)
       in text "h$" <> text (zEncodeString $ pkg ++ ":" ++ modName ++ "." ++ name)
  | otherwise = panic "idClosureText: unknown module"

ghcjsDsFImport :: Id
               -> Coercion
               -> ForeignImport
               -> DsM ([Binding], SDoc, SDoc)
ghcjsDsFImport id co (CImport cconv safety mHeader spec _) = do
    (ids, h, c) <- dsJsImport id co spec (unLoc cconv) (unLoc safety) mHeader
    return (ids, h, c)


dsJsImport :: Id
           -> Coercion
           -> CImportSpec
           -> CCallConv
           -> Safety
           -> Maybe Header
           -> DsM ([Binding], SDoc, SDoc)
dsJsImport id co (CLabel cid) cconv _ _ = do
   dflags <- getDynFlags
   let ty = pFst $ coercionKind co
       fod = case tyConAppTyCon_maybe (dropForAlls ty) of
             Just tycon
              | tyConUnique tycon == funPtrTyConKey ->
                 IsFunction
             _ -> IsData
   (_resTy, foRhs) <- jsResultWrapper ty
--   ASSERT(fromJust resTy `eqType` addrPrimTy)    -- typechecker ensures this
   let rhs = foRhs (Lit (LitLabel cid stdcall_info fod))
       rhs' = Cast rhs co
       stdcall_info = fun_type_arg_stdcall_info dflags cconv ty
                      -- in
   return ([(id, rhs')], empty, empty)

dsJsImport id co (CFunction target) cconv@PrimCallConv safety _
  = dsPrimCall id co (CCall (mkCCallSpec target cconv safety
                                         (panic "Missing Return PrimRep")
                                         (panic "Missing Argument PrimReps")))
dsJsImport id co (CFunction target) cconv safety mHeader
  = dsJsCall id co (CCall (mkCCallSpec target cconv safety
                                         (panic "Missing Return PrimRep")
                                         (panic "Missing Argument PrimReps"))) mHeader
dsJsImport id co CWrapper cconv _ _
  = dsJsFExportDynamic id co cconv

-- fixme work in progress
dsJsFExportDynamic :: Id
                 -> Coercion
                 -> CCallConv
                 -> DsM ([Binding], SDoc, SDoc)
dsJsFExportDynamic id co0 cconv = do
    mod <- getModule
    dflags <- getDynFlags
    let fe_nm = mkFastString $ zEncodeString
            ("h$" ++ moduleStableString mod ++ "$" ++ toCName dflags id)
        -- Construct the label based on the passed id, don't use names
        -- depending on Unique. See #13807 and Note [Unique Determinism].
    cback <- newSysLocalDs arg_ty
    newStablePtrId <- dsLookupGlobalId newStablePtrName
    stable_ptr_tycon <- dsLookupTyCon stablePtrTyConName
    let
        stable_ptr_ty = mkTyConApp stable_ptr_tycon [arg_ty]
        export_ty     = mkVisFunTy stable_ptr_ty arg_ty
    bindIOId <- dsLookupGlobalId bindIOName
    stbl_value <- newSysLocalDs stable_ptr_ty
    (h_code, c_code, typestring, args_size) <- ghcjsDsFExport id (mkRepReflCo export_ty) fe_nm cconv True
    let
         {-
          The arguments to the external function which will
          create a little bit of (template) code on the fly
          for allowing the (stable pointed) Haskell closure
          to be entered using an external calling convention
          (stdcall, ccall).
         -}
        adj_args      = [ mkIntLit dflags (toInteger (ccallConvToInt cconv))
                        , Var stbl_value
                        , Lit (LitLabel fe_nm mb_sz_args IsFunction)
                        , Lit (mkLitString typestring)
                        ]
          -- name of external entry point providing these services.
          -- (probably in the RTS.)
        adjustor   = fsLit "createAdjustor"

          -- Determine the number of bytes of arguments to the stub function,
          -- so that we can attach the '@N' suffix to its label if it is a
          -- stdcall on Windows.
        mb_sz_args = case cconv of
                        StdCallConv -> Just args_size
                        _           -> Nothing

    ccall_adj <- dsCCall adjustor adj_args PlayRisky (mkTyConApp io_tc [res_ty])
        -- PlayRisky: the adjustor doesn't allocate in the Haskell heap or do a callback

    let io_app = mkLams tvs                  $
                 Lam cback                   $
                 mkApps (Var bindIOId)
                        [ Type stable_ptr_ty
                        , Type res_ty
                        , mkApps (Var newStablePtrId) [ Type arg_ty, Var cback ]
                        , Lam stbl_value ccall_adj
                        ]

        fed = (id `setInlineActivation` NeverActive, Cast io_app co0)
               -- Never inline the f.e.d. function, because the litlit
               -- might not be in scope in other modules.

    return ([fed], h_code, c_code)

 where
  ty                       = pFst (coercionKind co0)
  (tvs,sans_foralls)       = tcSplitForAllTys ty
  ([arg_ty], fn_res_ty)    = tcSplitFunTys sans_foralls
  Just (io_tc, res_ty)     = tcSplitIOType_maybe fn_res_ty
        -- Must have an IO type; hence Just

toCName :: DynFlags -> Id -> String
toCName dflags i = showSDoc dflags (pprCode CStyle (ppr (idName i)))

dsJsCall :: Id -> Coercion -> ForeignCall -> Maybe Header
        -> DsM ([(Id, Expr TyVar)], SDoc, SDoc)
dsJsCall fn_id co (CCall (CCallSpec target cconv safety _ _)) _mDeclHeader = do
    let
        ty                   = pFst $ coercionKind co
        (tv_bndrs, rho)      = tcSplitForAllVarBndrs ty
        (arg_tys, io_res_ty) = tcSplitFunTys rho

    args <- newSysLocalsDs arg_tys  -- no FFI levity-polymorphism
    (val_args, arg_wrappers) <- mapAndUnzipM unboxJsArg (map Var args)

    let
        work_arg_ids  = [v | Var v <- val_args] -- All guaranteed to be vars

    (ccall_result_ty, res_wrapper) <- boxJsResult io_res_ty

    ccall_uniq <- newUnique
    work_uniq  <- newUnique

    dflags <- getDynFlags

    let
      fcall = CCall (mkCCallSpec target cconv safety io_res_ty arg_tys)

    let
        -- Build the worker
        worker_ty     = mkForAllTys tv_bndrs (mkVisFunTys (map idType work_arg_ids) ccall_result_ty)
        tvs           = map binderVar tv_bndrs
        the_ccall_app = mkFCall dflags ccall_uniq fcall val_args ccall_result_ty
        work_rhs      = mkLams tvs (mkLams work_arg_ids the_ccall_app)
        work_id       = mkSysLocal (fsLit "$wccall") work_uniq worker_ty

        -- Build the wrapper
        work_app     = mkApps (mkVarApps (Var work_id) tvs) val_args
        wrapper_body = foldr ($) (res_wrapper work_app) arg_wrappers
        wrap_rhs     = mkLams (tvs ++ args) wrapper_body
        wrap_rhs'    = Cast wrap_rhs co
        fn_id_w_inl  = fn_id `setIdUnfolding` mkInlineUnfoldingWithArity
                                                (length args) wrap_rhs'

    return ([(work_id, work_rhs), (fn_id_w_inl, wrap_rhs')], empty, empty)


mkHObj :: Type -> SDoc
mkHObj t = text "h$rts_mk" <> text (showFFIType t)

unpackHObj :: Type -> SDoc
unpackHObj t = text "h$rts_get" <> text (showFFIType t)

showStgType :: Type -> SDoc
showStgType t = text "Hs" <> text (showFFIType t)

showFFIType :: Type -> String
showFFIType t = getOccString (getName (typeTyCon t))

typeTyCon :: Type -> TyCon
typeTyCon ty
  -- | UnaryRep rep_ty <- repType ty
  | Just (tc, _) <- tcSplitTyConApp_maybe (unwrapType ty) -- rep_ty
  = tc
  | otherwise
  = pprPanic "Gen2.Foreign.typeTyCon" (ppr ty)


{-
  We unbox arguments for JS calls a bit different from native code:
    - Bool is marshalled to true/false, not 0/1
    - All int types are narrowed, since JS floats have a greater range than Int32
 -}

unboxJsArg :: CoreExpr                  -- The supplied argument
           -> DsM (CoreExpr,              -- To pass as the actual argument
                   CoreExpr -> CoreExpr   -- Wrapper to unbox the arg
                  )
unboxJsArg arg
  -- Primtive types: nothing to unbox
  | isPrimitiveType arg_ty
  = return (arg, \body -> body)

  -- Recursive newtypes
  | Just (co, _rep_ty) <- topNormaliseNewType_maybe arg_ty
  = unboxJsArg (mkCast arg co)

  -- Booleans, do not convert to 0/1, only force them
  | Just tc <- tyConAppTyCon_maybe arg_ty,
    tc `hasKey` boolTyConKey
  = return (arg,
              \ body -> mkWildCase arg boolTy (exprType body) [(DEFAULT,[],body)])

  | Just tc <- tyConAppTyCon_maybe arg_ty,
    tc `hasKey` anyTyConKey
  = return (arg,
              \ body -> mkWildCase arg arg_ty (exprType body) [(DEFAULT,[],body)])
  -- Data types with a single constructor, which has a single, primitive-typed arg
  -- This deals with Int, Float etc; also Ptr, ForeignPtr
  | is_product_type && data_con_arity == 1
    = do case_bndr <- newSysLocalDs arg_ty
         prim_arg <- newSysLocalDs data_con_arg_ty1
         return (Var prim_arg,
               \ body -> Case arg case_bndr (exprType body) [(DataAlt data_con,[prim_arg],body)]
              )

  -- Byte-arrays, both mutable and otherwise; hack warning
  -- We're looking for values of type ByteArray, MutableByteArray
  --    data ByteArray          ix = ByteArray        ix ix ByteArray#
  --    data MutableByteArray s ix = MutableByteArray ix ix (MutableByteArray# s)
  | is_product_type &&
    data_con_arity == 3 &&
    isJust maybe_arg3_tycon &&
    (arg3_tycon ==  byteArrayPrimTyCon ||
     arg3_tycon ==  mutableByteArrayPrimTyCon)
  = do case_bndr <- newSysLocalDs arg_ty
       vars@[_l_var, _r_var, arr_cts_var] <- newSysLocalsDs data_con_arg_tys
       return (Var arr_cts_var,
               \ body -> Case arg case_bndr (exprType body) [(DataAlt data_con,vars,body)]
              )

  | otherwise
  = do l <- getSrcSpanDs
       pprPanic "unboxJsArg: " (ppr l <+> ppr arg_ty)
  where
    arg_ty                                      = exprType arg
    maybe_product_type                          = splitDataProductType_maybe arg_ty
    is_product_type                             = isJust maybe_product_type
    Just (_, _, data_con, data_con_arg_tys)     = maybe_product_type
    data_con_arity                              = dataConSourceArity data_con
    (data_con_arg_ty1 : _)                      = data_con_arg_tys

    (_ : _ : data_con_arg_ty3 : _) = data_con_arg_tys
    maybe_arg3_tycon               = tyConAppTyCon_maybe data_con_arg_ty3
    Just arg3_tycon                = maybe_arg3_tycon


boxJsResult :: Type
            -> DsM (Type, CoreExpr -> CoreExpr)
boxJsResult result_ty
  | isRuntimeRepKindedTy result_ty = panic "boxJsResult: runtime rep ty" -- fixme
-- Takes the result of the user-level ccall:
--      either (IO t),
--      or maybe just t for an side-effect-free call
-- Returns a wrapper for the primitive ccall itself, along with the
-- type of the result of the primitive ccall.  This result type
-- will be of the form
--      State# RealWorld -> (# State# RealWorld, t' #)
-- where t' is the unwrapped form of t.  If t is simply (), then
-- the result type will be
--      State# RealWorld -> (# State# RealWorld #)

boxJsResult result_ty
  | Just (io_tycon, io_res_ty) <- tcSplitIOType_maybe result_ty
        -- isIOType_maybe handles the case where the type is a
        -- simple wrapping of IO.  E.g.
        --      newtype Wrap a = W (IO a)
        -- No coercion necessary because its a non-recursive newtype
        -- (If we wanted to handle a *recursive* newtype too, we'd need
        -- another case, and a coercion.)
        -- The result is IO t, so wrap the result in an IO constructor
  = do  { res <- jsResultWrapper io_res_ty
        ; let return_result state ans
                = mkCoreUbxTup
                    [realWorldStatePrimTy, io_res_ty]
                    [state, ans]
{-                = mkConApp (tupleDataCon Unboxed 2)
                           (map Type [realWorldStatePrimTy, io_res_ty]
                              ++ [state, ans]) -}

        ; (ccall_res_ty, the_alt) <- mk_alt return_result res

        ; state_id <- newSysLocalDs realWorldStatePrimTy
        ; let io_data_con = head (tyConDataCons io_tycon)
              toIOCon     = dataConWrapId io_data_con

              wrap the_call =
                              mkApps (Var toIOCon)
                                     [ Type io_res_ty,
                                       Lam state_id $
                                       mkWildCase (App the_call (Var state_id))
                                             ccall_res_ty
                                             (coreAltType the_alt)
                                             [the_alt]
                                     ]

        ; return (realWorldStatePrimTy `mkVisFunTy` ccall_res_ty, wrap) }

boxJsResult result_ty
  = do -- It isn't IO, so do unsafePerformIO
       -- It's not conveniently available, so we inline it
       res <- jsResultWrapper result_ty
       (ccall_res_ty, the_alt) <- mk_alt return_result res
       let
           wrap = \ the_call -> mkWildCase (App the_call (Var realWorldPrimId))
                                           ccall_res_ty
                                           (coreAltType the_alt)
                                           [the_alt]
       return (realWorldStatePrimTy `mkVisFunTy` ccall_res_ty, wrap)
  where
    return_result _ ans = ans

mk_alt :: (Expr Var -> Expr Var -> Expr Var)
       -> (Maybe Type, Expr Var -> Expr Var)
       -> DsM (Type, (AltCon, [Id], Expr Var))
mk_alt return_result (Nothing, wrap_result)
  = do -- The ccall returns ()
       state_id <- newSysLocalDs realWorldStatePrimTy
       let
             the_rhs = return_result (Var state_id)
                                     (wrap_result $ panic "jsBoxResult")
             ccall_res_ty = mkTupleTy Unboxed [realWorldStatePrimTy]
             the_alt      = (DataAlt (tupleDataCon Unboxed 1), [state_id], the_rhs)
       return (ccall_res_ty, the_alt)

mk_alt return_result (Just prim_res_ty, wrap_result)
                -- The ccall returns a non-() value
  | isUnboxedTupleType prim_res_ty = do
    let
        Just ls = fmap dropRuntimeRepArgs (tyConAppArgs_maybe prim_res_ty)
        arity = 1 + length ls
    args_ids {-@(result_id:as)-} <- mapM newSysLocalDs ls
    state_id <- newSysLocalDs realWorldStatePrimTy
    let
        result_tup = -- mkCoreConApps (tupleDataCon Unboxed (length ls)) (map Type ls ++ map Var args_ids)
          mkCoreUbxTup ls (map Var args_ids)
        the_rhs = return_result (Var state_id)
                                (wrap_result result_tup)
        ccall_res_ty = mkTupleTy Unboxed (realWorldStatePrimTy : ls)
        the_alt      = ( DataAlt (tupleDataCon Unboxed arity)
                       , (state_id : args_ids)
                       , the_rhs
                       )
    return (ccall_res_ty, the_alt)

  | otherwise = do
    result_id <- newSysLocalDs prim_res_ty
    state_id <- newSysLocalDs realWorldStatePrimTy
    let
        the_rhs = return_result (Var state_id)
                                (wrap_result (Var result_id))
        ccall_res_ty = mkTupleTy Unboxed [realWorldStatePrimTy, prim_res_ty]
        the_alt      = (DataAlt (tupleDataCon Unboxed 2), [state_id, result_id], the_rhs)
    return (ccall_res_ty, the_alt)

fun_type_arg_stdcall_info :: DynFlags -> CCallConv -> Type -> Maybe Int
fun_type_arg_stdcall_info _ _other_conv _ = Nothing


jsResultWrapper :: Type
              -> DsM (Maybe Type,               -- Type of the expected result, if any
                      CoreExpr -> CoreExpr)     -- Wrapper for the result
-- resultWrapper deals with the result *value*
-- E.g. foreign import foo :: Int -> IO T
-- Then resultWrapper deals with marshalling the 'T' part
jsResultWrapper result_ty
  | isRuntimeRepKindedTy result_ty = return (Nothing, id) -- fixme this seems like a hack
  -- Base case 1a: unboxed tuples
  | Just (tc, args) <- splitTyConApp_maybe result_ty
  , isUnboxedTupleTyCon tc {- && False -} = do
    let args' = dropRuntimeRepArgs args
    (tys, wrappers) <- unzip <$> mapM jsResultWrapper args'
    matched <- mapM (mapM newSysLocalDs) tys
    let tys'    = catMaybes tys
        -- arity   = length args'
        -- resCon  = tupleDataCon Unboxed (length args)
        err     = panic "jsResultWrapper: used Id with result type Nothing"
        resWrap :: CoreExpr
        resWrap = mkCoreUbxTup args' (zipWith (\w -> w . Var . fromMaybe err) wrappers matched)
          -- mkCoreConApps resCon (map Type args ++ zipWith (\w -> w . Var . fromMaybe err) wrappers matched)
    return $
      if null tys'
        then (Nothing, \_ -> resWrap)
        else let innerArity = length tys'
                 innerTy    = mkTupleTy Unboxed tys'
                 innerCon   = tupleDataCon Unboxed innerArity
                 inner :: CoreExpr -> CoreExpr
                 inner e    = mkWildCase e innerTy result_ty
                                         [( DataAlt innerCon
                                          , catMaybes matched
                                          , resWrap
                                          )]
             in (Just innerTy, inner)

  -- Base case 1b: primitive types
  | isPrimitiveType result_ty
  = return (Just result_ty, \e -> e)
  -- Base case 1c: boxed tuples
  -- fixme: levity args?
  | Just (tc, args) <- splitTyConApp_maybe result_ty
  , isBoxedTupleTyCon tc = do
      let args'   = dropRuntimeRepArgs args
          innerTy = mkTupleTy Unboxed args'
      (inner_res, w) <- jsResultWrapper innerTy
      matched <- mapM newSysLocalDs args'
      let inner e = mkWildCase (w e) innerTy result_ty
                               [( DataAlt (tupleDataCon Unboxed (length args'))
                                , matched
                                , mkCoreTup (map Var matched)
                                -- mkCoreConApps (tupleDataCon Boxed (length args)) (map Type args ++ map Var matched)
                                )]
      return (inner_res, inner)

  -- Base case 2: the unit type ()
  | Just (tc,_) <- maybe_tc_app, tc `hasKey` unitTyConKey
  = return (Nothing, \_ -> Var unitDataConId)

  -- Base case 3: the boolean type
  | Just (tc,_) <- maybe_tc_app, tc `hasKey` boolTyConKey = do
    dflags <- getDynFlags
--    result_id <- newSysLocalDs boolTy
    ccall_uniq <- newUnique
    let forceBool e = mkJsCall dflags ccall_uniq "$r = !(!$1)" [e] boolTy
    return
     (Just intPrimTy, \e -> forceBool e)

  -- Base case 4: the any type
  |  Just (tc,_) <- maybe_tc_app, tc `hasKey` anyTyConKey
  = return (Just result_ty, \e -> e)

  -- Newtypes
  | Just (co, rep_ty) <- topNormaliseNewType_maybe result_ty
  = do (maybe_ty, wrapper) <- jsResultWrapper rep_ty
       return (maybe_ty, \e -> mkCastDs (wrapper e) (mkSymCo co))

  -- The type might contain foralls (eg. for dummy type arguments,
  -- referring to 'Ptr a' is legal).
  | Just (tyvar, rest) <- splitForAllTy_maybe result_ty
  = do (maybe_ty, wrapper) <- jsResultWrapper rest
       return (maybe_ty, \e -> Lam tyvar (wrapper e))

  -- Data types with a single constructor, which has a single arg
  -- This includes types like Ptr and ForeignPtr
  | Just (tycon, tycon_arg_tys, data_con, data_con_arg_tys) <- splitDataProductType_maybe result_ty,
    dataConSourceArity data_con == 1
  = do dflags <- getDynFlags
       let
           (unwrapped_res_ty : _) = data_con_arg_tys
           narrow_wrapper         = maybeJsNarrow dflags tycon
       (maybe_ty, wrapper) <- jsResultWrapper unwrapped_res_ty
       return
         (maybe_ty, \e -> mkApps (Var (dataConWrapId data_con))
                                 (map Type tycon_arg_tys ++ [wrapper (narrow_wrapper e)]))

  | otherwise
  = pprPanic "jsResultWrapper" (ppr result_ty)
  where
    maybe_tc_app = splitTyConApp_maybe result_ty

-- low-level primitive JavaScript call:
mkJsCall :: DynFlags -> Unique -> String -> [CoreExpr] -> Type -> CoreExpr
mkJsCall dflags u tgt args t =
  mkFCall dflags u (CCall (mkCCallSpec (StaticTarget NoSourceText (mkFastString tgt)
                                                       (Just primPackageKey)
                                                       True)
                                      JavaScriptCallConv PlayRisky
                                      t
                                      (map exprType args)
                                      )) args t

-- represent a primitive type as a Char, for building a string that
-- described the foreign function type.  The types are size-dependent,
-- e.g. 'W' is a signed 32-bit integer.
primTyDescChar :: DynFlags -> Type -> Char
primTyDescChar dflags ty
 | ty `eqType` unitTy = 'v'
 | ty `eqType` boolTy = 'b'
 | otherwise
 = case typePrimRep1 (getPrimTyOf ty) of
     IntRep      -> signed_word
     WordRep     -> unsigned_word
     Int64Rep    -> 'L'
     Word64Rep   -> 'l'
     AddrRep     -> 'p'
     FloatRep    -> 'f'
     DoubleRep   -> 'd'
     _           -> pprPanic "primTyDescChar" (ppr ty)
  where
    (signed_word, unsigned_word)
       | wORD_SIZE dflags == 4  = ('W','w')
       | wORD_SIZE dflags == 8  = ('L','l')
       | otherwise              = panic "primTyDescChar"

cLibFFI :: Bool
cLibFFI = False

-- This function returns the primitive type associated with the boxed
-- type argument to a foreign export (eg. Int ==> Int#).
getPrimTyOf :: Type -> UnaryType
getPrimTyOf ty
  | isBoolTy rep_ty = intPrimTy
  -- Except for Bool, the types we are interested in have a single constructor
  -- with a single primitive-typed argument (see TcType.legalFEArgTyCon).
  | otherwise =
  case splitDataProductType_maybe rep_ty of
     Just (_, _, _data_con, [prim_ty]) ->
--        ASSERT(dataConSourceArity data_con == 1)
--        ASSERT2(isUnliftedType prim_ty, ppr prim_ty)
        prim_ty
     _other -> pprPanic "DsForeign.getPrimTyOf" (ppr ty)
  where
    rep_ty = unwrapType ty

-- When the result of a foreign call is smaller than the word size, we
-- need to sign- or zero-extend the result up to the word size.  The C
-- standard appears to say that this is the responsibility of the
-- caller, not the callee.

-- narrow int32 and word32 since JS numbers can contain more
maybeJsNarrow :: DynFlags -> TyCon -> (CoreExpr -> CoreExpr)
maybeJsNarrow _dflags tycon
  | tycon `hasKey` intTyConKey    = \e -> App (Var (mkPrimOpId Narrow32IntOp)) e
  | tycon `hasKey` int8TyConKey   = \e -> App (Var (mkPrimOpId Narrow8IntOp)) e
  | tycon `hasKey` int16TyConKey  = \e -> App (Var (mkPrimOpId Narrow16IntOp)) e
  | tycon `hasKey` int32TyConKey  = \e -> App (Var (mkPrimOpId Narrow32IntOp)) e
  | tycon `hasKey` wordTyConKey   = \e -> App (Var (mkPrimOpId Narrow32WordOp)) e
  | tycon `hasKey` word8TyConKey  = \e -> App (Var (mkPrimOpId Narrow8WordOp)) e
  | tycon `hasKey` word16TyConKey = \e -> App (Var (mkPrimOpId Narrow16WordOp)) e
  | tycon `hasKey` word32TyConKey = \e -> App (Var (mkPrimOpId Narrow32WordOp)) e
  | otherwise                     = id

{-
  desugar foreign declarations for native code: replace
  all foreign import JavaScript by a CCall
  to the JavaScript handler.

  The JavaScript handler can be installed by calling
  `setJavaScriptHandler` (ghcjs.h) from C. The
  default handler prints an error message and terminates the
  program.
-}
ghcjsNativeDsForeigns :: [LForeignDecl GhcTc]
                      -> DsM (ForeignStubs, OrdList (Id, CoreExpr))
ghcjsNativeDsForeigns fos = do
  dflags <- getDynFlags
  (stubs, ret) <- dsForeigns' (map (convertForeignDecl dflags) fos)
  case mapMaybe (importStub dflags) fos of
    [] -> return (stubs, ret)
    xs -> return (stubs `appendStubC'` vcat xs, ret)
    where
      appendStubC' NoStubs s = ForeignStubs empty (inclGhcjs $$ s)
      appendStubC' (ForeignStubs h c) s =
        ForeignStubs h (inclGhcjs $$ c $$ s)
      inclGhcjs = text "#include \"ghcjs.h\""

      convertForeignDecl :: DynFlags -> LForeignDecl GhcTc -> LForeignDecl GhcTc
      convertForeignDecl dflags (L l (ForeignImport c n t (CImport (L _lc JavaScriptCallConv) _safety mheader _spec txt))) =
        (L l (ForeignImport c n t (CImport (noLoc CCallConv) (noLoc PlaySafe) mheader (convertSpec dflags n) txt)))
      convertForeignDecl _dflags (L l (ForeignExport c n t (CExport (L _ (CExportStatic srcTxt lbl JavaScriptCallConv)) txt))) =
        (L l (ForeignExport c n t (CExport (noLoc (CExportStatic srcTxt lbl CCallConv)) txt)))
      convertForeignDecl _ x = x

      -- convertSpec :: DynFlags -> Located GhcTc -> CImportSpec
      convertSpec dflags i = CFunction (StaticTarget NoSourceText (stubName dflags (unLoc i)) Nothing True)

      -- stubName :: DynFlags -> Id -> FastString
      stubName dflags i = mkFastString $
        "__ghcjs_stub_" ++ zEncodeString (showSDocOneLine dflags (ppr $ idName i))

      importStub :: DynFlags -> LForeignDecl GhcTc -> Maybe SDoc
      importStub dflags (L _l (ForeignImport c n _t (CImport (L _ JavaScriptCallConv) (L _ safety) _mheader spec _txt))) =
        Just (mkImportStub dflags (unLoc n) c safety spec)
      importStub _ _ = Nothing

      mkImportStub :: DynFlags -> Id -> Coercion -> Safety -> CImportSpec -> SDoc
      mkImportStub dflags i _c s spec =
        text resTy <+> ftext (stubName dflags i) <> stubArgs <+> braces body
          where
           js :: SDoc
           js = case spec of
                  CLabel cls                         -> escapeQuoted (unpackFS cls)
                  CFunction (StaticTarget _ cls _ _) -> escapeQuoted (unpackFS cls) -- fixme source text may be better?
                  _ -> error "ghcjsNativeDsForeigns: unexpected import spec"
           safety PlayRisky         = int 0
           safety PlaySafe          = int 1
           safety PlayInterruptible = int 2
           escapeQuoted xs = doubleQuotes $ text (concatMap escapeChar xs)
             where
               -- fixme proper escaping and handling of non-ascii characters
               escapeChar '\\' = "\\\\"
               escapeChar '\n' = "\\n"
               escapeChar '\t' = "\\t"
               escapeChar '"'  = "\\\""
               escapeChar x    = x:[]

           t = idType i
           (args, res) = tcSplitFunTys . snd . tcSplitForAllTys $ t
           argNames    = map ((text "arg" <>) . int) [1..]
           (argTys, argsSig) = unzip $ map (jsTySigLit dflags False) args
           (resTy,  resSig) = jsTySigLit dflags True res
           body | resSig == 'v' = vcat [text "int res;", call]
                | otherwise     = vcat [text resTy <+> text "res;", call, text "return res;"]
           call = text "getJavaScriptHandler()" <> parens (pprWithCommas id handlerArgs) <> semi
           handlerArgs = [js, safety s, escapeQuoted (resSig : argsSig), text "(void*)&res"]
                           ++ take (length args) argNames
           stubArgs = parens $ pprWithCommas id (zipWith (\ty n -> text ty <+> n) argTys argNames)

jsTySigLit :: DynFlags -> Bool -> Type -> (String, Char)
jsTySigLit dflags isResult t | isResult, Just (_ ,result) <- tcSplitIOType_maybe t =
                                 jsTySigLit dflags isResult result
                             | Just (_, t') <- splitForAllTy_maybe t = jsTySigLit dflags isResult t'
                             | Just (tc, _) <- splitTyConApp_maybe t = tcSig isResult tc
                             | otherwise = error $ "jsTySigLit: unexpected type: "
                                                       ++ showSDoc dflags (ppr t)
  where
           tcSig :: Bool -> TyCon -> (String, Char)
           tcSig isResult tc
             | isUnliftedTyCon tc, [tt] <- tyConPrimRep tc = prim tt -- = prim (tyConPrimRep tc)
             | Just r <- lookup (getUnique tc) boxed               = r
             | isResult && getUnique tc == unitTyConKey            = ("void", 'v')
             | isJSValTyCon dflags tc = ("StgPtr", 'r')
             | otherwise = error $ "jsTySigLit: unexpected TyCon: "
                                       ++ showSDoc dflags (ppr tc)
              where
                 -- fixme is there already a list of these somewhere else?
                 prim VoidRep  = error "jsTySigLit: VoidRep"
                 prim LiftedRep = hsPtr
                 prim UnliftedRep = hsPtr -- fixme?
                 -- prim PtrRep   = hsPtr
                 prim IntRep   = hsInt
                 prim Int8Rep  = hsInt
                 prim Int16Rep = hsInt
                 prim Int32Rep = hsInt
                 prim WordRep  = hsWord
                 prim Word8Rep = hsWord
                 prim Word16Rep = hsWord
                 prim Word32Rep = hsWord
                 prim Int64Rep  = hsInt64
                 prim Word64Rep = hsWord64
                 prim AddrRep   = hsPtr
                 prim FloatRep  = hsFloat
                 prim DoubleRep = hsDouble
                 prim (VecRep{}) = error "jsTySigLit: VecRep"
                 boxed = [ (intTyConKey,        hsInt               )
                         , (int8TyConKey,       ("StgInt8",      'b'))
                         , (int16TyConKey,      ("StgInt16",     's'))
                         , (int32TyConKey,      ("StgInt32",     'l'))
                         , (int64TyConKey,      hsInt64             )
                         , (wordTyConKey,       hsWord              )
                         , (word8TyConKey,      ("StgWord8",     'B'))
                         , (word16TyConKey,     ("StgWord16",    'S'))
                         , (word32TyConKey,     ("StgWord32",    'L'))
                         , (word64TyConKey,     hsWord64            )
                         , (floatTyConKey,      hsFloat             )
                         , (doubleTyConKey,     hsDouble            )
                         , (ptrTyConKey,        hsPtr               )
                         , (funPtrTyConKey,     hsPtr               )
                         , (charTyConKey,       ("StgChar",      'c'))
                         , (stablePtrTyConKey,  hsPtr               )
                         , (boolTyConKey,       hsInt               )
                         ]
                 hsInt    = ("StgInt",    'i')
                 hsInt64  = ("StgInt64",  'm')
                 hsWord   = ("StgWord",   'I')
                 hsWord64 = ("StgWord64", 'M')
                 hsPtr    = ("StgPtr",    'p')
                 hsFloat  = ("StgFloat",  'f')
                 hsDouble = ("StgDouble", 'd')

ghcjsTcForeignImports :: [LForeignDecl GhcRn]
                      -> TcM ([Id], [LForeignDecl GhcTc], Bag GlobalRdrElt)
ghcjsTcForeignImports decls
  = do { (ids, decls, gres) <- mapAndUnzip3M ghcjsTcFImport $
                               filter isForeignImport decls
       ; return (ids, decls, unionManyBags gres) }

foreignDeclCtxt :: ForeignDecl GhcRn -> SDoc
foreignDeclCtxt fo
  = hang (ptext (sLit "When checking declaration:"))
       2 (ppr fo)

ghcjsTcFImport :: LForeignDecl GhcRn
               -> TcM (Id, LForeignDecl GhcTc, Bag GlobalRdrElt)
ghcjsTcFImport (L dloc fo@(ForeignImport { fd_name = L nloc nm, fd_sig_ty = hs_ty
                                    , fd_fi = imp_decl }))
  = setSrcSpan dloc $ addErrCtxt (foreignDeclCtxt fo)  $
    do { sig_ty <- tcHsSigType (ForSigCtxt nm) hs_ty
       ; (norm_co, norm_sig_ty, gres) <- normaliseFfiType sig_ty
       ; let
           -- Drop the foralls before inspecting the
           -- structure of the foreign type.
             (bndrs, res_ty)   = tcSplitPiTys norm_sig_ty
             arg_tys           = mapMaybe binderRelevantType_maybe bndrs
             id                = mkLocalId nm sig_ty
                 -- Use a LocalId to obey the invariant that locally-defined
                 -- things are LocalIds.  However, it does not need zonking,
                 -- (so TcHsSyn.zonkForeignExports ignores it).

       ; imp_decl' <- ghcjsTcCheckFIType arg_tys res_ty imp_decl
          -- Can't use sig_ty here because sig_ty :: Type and
          -- we need HsType Id hence the undefined
       ; let fi_decl = ForeignImport { fd_name = L nloc id
                                     , fd_sig_ty = undefined
                                     , fd_i_ext = mkSymCo norm_co
                                     , fd_fi = imp_decl' }
       ; return (id, L dloc fi_decl, gres) }
ghcjsTcFImport d = pprPanic "ghcjsTcFImport" (ppr d)


ghcjsTcCheckFIType :: [Type] -> Type -> ForeignImport -> TcM ForeignImport
ghcjsTcCheckFIType {- sig_ty -} arg_tys res_ty (CImport lcconv@(L _ cconv) lsafety@(L _ safety) mh (CFunction target) src)
  | cconv == JavaScriptCallConv = do
      dflags <- getDynFlags
      checkForeignArgs (isGhcjsFFIArgumentTy dflags safety) arg_tys
      checkForeignRes nonIOok checkSafe (isGhcjsFFIImportResultTy dflags) res_ty
      -- checkMissingAmpersand dflags arg_tys res_ty
      case target of
          StaticTarget _ _ _ False
           | not (null arg_tys) ->
              addErrTc (text "`value' imports cannot have function types")
          _ -> return ()
      return $ CImport lcconv lsafety mh (CFunction target) src
ghcjsTcCheckFIType {- _sig_ty -} arg_tys res_ty idecl = tcCheckFIType arg_tys res_ty idecl

isGhcjsFFIArgumentTy :: DynFlags -> Safety -> Type -> Validity
isGhcjsFFIArgumentTy dflags safety ty
  | isValid (isFFIArgumentTy dflags safety ty)                          = IsValid
  | haveGHCForeignImportPrim dflags && haveUnliftedFFITypes dflags &&
    isValid (isFFIPrimArgumentTy dflags ty)                             = IsValid
  | isGhcjsFFITy dflags ty                                              = IsValid
  | Just (tc, _) <- tcSplitTyConApp_maybe ty
  , getUnique tc == anyTyConKey && haveGHCForeignImportPrim dflags      = IsValid
  | otherwise = NotValid
      (text "JavaScript FFI argument type must be a valid CCall FFI argument type or JSVal")

isGhcjsFFIImportResultTy :: DynFlags -> Type -> Validity
isGhcjsFFIImportResultTy dflags ty
  | Just (tc, args) <- tcSplitTyConApp_maybe ty
  , isBoxedTupleTyCon tc || isUnboxedTupleTyCon tc =
      let args' = dropRuntimeRepArgs args
      in if all (isValid . check) args'
           then IsValid
           else NotValid
              (text $ "JavaScript FFI result type must be a valid CCall FFI result type or JSVal: " ++ showPpr dflags ty ++ " " ++ show (map (isValid . check) args') ++ " " ++ show (map (showPpr dflags) args'))
  | otherwise = isGhcjsFFIImportResultTy' dflags ty
    where
      check ty | Just (tc, _args) <- tcSplitTyConApp_maybe ty
               , getUnique tc == liftedTypeKindTyConKey
                {- || getUnique tc == unliftedTypeKindTyConKey -} = IsValid -- fixme
               | isValid (isGhcjsFFIImportResultTy' dflags ty) = IsValid
               | isGhcjsFFITy dflags ty = IsValid
               | otherwise = NotValid (text $ "")


isGhcjsFFIImportResultTy' :: DynFlags -> Type -> Validity
isGhcjsFFIImportResultTy' dflags ty
  | isValid (isFFIImportResultTy dflags ty)                   = IsValid

-- -  | xopt Opt_UnliftedFFITypes dflags && isUnboxedTupleType ty = IsValid
  | haveGHCForeignImportPrim dflags && haveUnliftedFFITypes dflags &&
    checkRepTyCon isUnliftedTyCon ty                          = IsValid
  --   isValid (isFFIPrimResultTy dflags ty)                     = IsValid
  | Just (tc, _) <- tcSplitTyConApp_maybe ty
  , haveGHCForeignImportPrim dflags && getUnique tc == anyTyConKey = IsValid
  | isGhcjsFFITy dflags ty                                    = IsValid
  | otherwise = NotValid
      (text $ "JavaScript FFI result type must be a valid CCall FFI result type or JSVal: " ++ showPpr dflags ty)

haveUnliftedFFITypes :: DynFlags -> Bool
haveUnliftedFFITypes dflags = xopt UnliftedFFITypes dflags

haveGHCForeignImportPrim :: DynFlags -> Bool
haveGHCForeignImportPrim dflags = xopt GHCForeignImportPrim dflags

-- check for JSVal, xxx cleanup, xxx check whether newtypes work
isGhcjsFFITy _ ty
  | Just (tc, _) <- splitTyConApp_maybe ty = getUnique tc == jsvalTyConKey
  | otherwise = False

isJSValTyCon :: DynFlags -> TyCon -> Bool
isJSValTyCon _ tc = tc `hasKey` jsvalTyConKey


{-
isGhcjsFFITy :: DynFlags -> Type -> Bool
isGhcjsFFITy = checkNamedTy jsFfiTys

isJSValTy :: DynFlags -> Type -> Bool
isJSValTy = checkNamedTy [jsValTy]

isJSValTyCon :: DynFlags -> TyCon -> Bool
isJSValTyCon = checkNamedTyCon [jsValTy]

checkNamedTy :: [(String, String, String)] -> DynFlags -> Type -> Bool
checkNamedTy tys dflags ty = checkRepTyCon (checkNamedTyCon tys dflags) ty

checkNamedTyCon :: [(String, String, String)] -> DynFlags -> TyCon -> Bool
checkNamedTyCon tys dflags tc
  = any (\(p,m,n) -> m == mod && n == name && validPkg p pkg) tys
      where
        validPkg p ""
          | [b] <- catMaybes (map (stripPrefix "-DBOOTING_PACKAGE=")
                                  (opt_P dflags)) = p == b
        validPkg p p' = p == p'

        -- comparing strings is probably not too fast, perhaps search
        -- for the types first and use some cache
        n = tyConName (repTc tc)
        (pkg, mod) = case nameModule_maybe n of
                       Nothing -> ("", "")
                       Just m  -> ( modulePackageName dflags m
                                  , moduleNameString (moduleName m))
        name = occNameString (nameOccName n)
-}
repTc :: TyCon -> TyCon
repTc = go
  where
    go :: TyCon -> TyCon
    go tc | Just (_tvs, t, _) <- unwrapNewTyCon_maybe tc =
              case splitTyConApp_maybe (dropForAlls t) of
                Nothing       -> error "repTc: not a tycon application"
                Just (tc', _) -> go tc'
          | otherwise = tc

{-
jsFfiTys :: [(String, String, String)]
jsFfiTys = [jsValTy]

jsValTy :: (String, String, String)
jsValTy = ("base", "GHCJS.Prim", "JSVal")
-}

-- normaliseFfiType gets run before checkRepTyCon, so we don't
-- need to worry about looking through newtypes or type functions
-- here; that's already been taken care of.
checkRepTyCon :: (TyCon -> Bool) -> Type -> Bool
checkRepTyCon check_tc ty
    | Just (tc, _) <- splitTyConApp_maybe ty
      = check_tc tc
    | otherwise
      = False

ghcjsNativeTcForeignImports :: [LForeignDecl GhcRn]
                            -> TcM ([Id], [LForeignDecl GhcTcId], Bag GlobalRdrElt)
ghcjsNativeTcForeignImports = ghcjsTcForeignImports

ghcjsNativeTcForeignExports :: [LForeignDecl GhcRn]
                            -> TcM (LHsBinds GhcTcId, [LForeignDecl GhcTcId], Bag GlobalRdrElt)
ghcjsNativeTcForeignExports = ghcjsTcForeignExports

ghcjsTcForeignExports :: [LForeignDecl GhcRn]
                      -> TcM (LHsBinds GhcTc, [LForeignDecl GhcTc], Bag GlobalRdrElt)
-- For the (Bag GlobalRdrElt) result,
-- see Note [Newtype constructor usage in foreign declarations]
ghcjsTcForeignExports decls
  = foldlM combine (emptyLHsBinds, [], emptyBag) (filter isForeignExport decls)
  where
   combine (binds, fs, gres1) (L loc fe) = do
       (b, f, gres2) <- setSrcSpan loc (ghcjsTcFExport fe)
       return (b `consBag` binds, L loc f : fs, gres1 `unionBags` gres2)

ghcjsTcFExport :: ForeignDecl GhcRn
          -> TcM (LHsBind GhcTc, ForeignDecl GhcTc, Bag GlobalRdrElt)
ghcjsTcFExport fo@(ForeignExport { fd_name = L loc nm, fd_sig_ty = hs_ty, fd_fe = spec })
  = addErrCtxt (foreignDeclCtxt fo) $ do

    sig_ty <- tcHsSigType (ForSigCtxt nm) hs_ty
    rhs <- tcPolyExpr (nlHsVar nm) sig_ty

    (norm_co, norm_sig_ty, gres) <- normaliseFfiType sig_ty

    spec' <- ghcjsTcCheckFEType norm_sig_ty spec

           -- we're exporting a function, but at a type possibly more
           -- constrained than its declared/inferred type. Hence the need
           -- to create a local binding which will call the exported function
           -- at a particular type (and, maybe, overloading).


    -- We need to give a name to the new top-level binding that
    -- is *stable* (i.e. the compiler won't change it later),
    -- because this name will be referred to by the C code stub.
    id  <- mkStableIdFromName nm sig_ty loc mkForeignExportOcc
    return ( mkVarBind id rhs
           , ForeignExport { fd_name = L loc id
                           , fd_sig_ty = undefined
                           , fd_e_ext = norm_co, fd_fe = spec' }
           , gres)
ghcjsTcFExport d = pprPanic "ghcjsTcFExport" (ppr d)


ghcjsTcCheckFEType :: Type -> ForeignExport -> TcM ForeignExport
ghcjsTcCheckFEType sig_ty (CExport (L l (CExportStatic esrc str cconv)) src) = do
--    checkCg checkCOrAsmOrLlvm
    checkTc (isCLabelString str) (badCName str)
    cconv' <- ghcjsCheckCConv cconv
    checkForeignArgs isFFIExternalTy arg_tys
    checkForeignRes nonIOok noCheckSafe isFFIExportResultTy res_ty
    return (CExport (L l (CExportStatic esrc str cconv')) src)
  where
      -- Drop the foralls before inspecting n
      -- the structure of the foreign type.
    (_, t_ty) = tcSplitForAllTys sig_ty
    (arg_tys, res_ty) = tcSplitFunTys t_ty

ghcjsCheckCConv :: CCallConv -> TcM CCallConv
ghcjsCheckCConv CCallConv          = return CCallConv
ghcjsCheckCConv CApiConv           = return CApiConv
ghcjsCheckCConv JavaScriptCallConv = return JavaScriptCallConv

ghcjsCheckCConv StdCallConv  = do dflags <- getDynFlags
                                  let platform = targetPlatform dflags
                                  if platformArch platform == ArchX86
                                    then return StdCallConv
                                    else do -- This is a warning, not an error. see #3336
                                           when (wopt Opt_WarnUnsupportedCallingConventions dflags) $
                                             addWarnTc (Reason Opt_WarnUnsupportedCallingConventions) (text "the 'stdcall' calling convention is unsupported on this platform," $$ text "treating as ccall")
                                           return CCallConv
ghcjsCheckCConv PrimCallConv = do addErrTc (text "The `prim' calling convention can only be used with `foreign import'")
                                  return PrimCallConv


checkCg :: (HscTarget -> Maybe SDoc) -> TcM ()
checkCg check = do
    dflags <- getDynFlags
    let target = hscTarget dflags
    case target of
      HscNothing -> return ()
      _ ->
        case check target of
          Nothing  -> return ()
          Just err -> addErrTc (text "Illegal foreign declaration:" <+> err)

check :: Bool -> MsgDoc -> TcM ()
check True _       = return ()
check _    the_err = addErrTc the_err

badCName :: CLabelString -> MsgDoc
badCName target
  = sep [quotes (ppr target) <+> ptext (sLit "is not a valid C identifier")]

