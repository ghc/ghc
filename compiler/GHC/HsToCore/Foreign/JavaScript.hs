{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- | Handling of JavaScript foreign imports/exports
module GHC.HsToCore.Foreign.JavaScript
  ( dsJsImport
  , dsJsFExport
  , dsJsFExportDynamic
  )
where

import GHC.Prelude

import GHC.Platform

import GHC.Hs

import GHC.HsToCore.Monad
import GHC.HsToCore.Foreign.Call
import GHC.HsToCore.Foreign.Prim
import GHC.HsToCore.Foreign.Utils
import GHC.HsToCore.Utils

import GHC.Core
import GHC.Core.Make
import GHC.Core.Utils
import GHC.Core.DataCon
import GHC.Core.Unfold.Make
import GHC.Core.Type
import GHC.Core.TyCon
import GHC.Core.Coercion
import GHC.Core.Multiplicity

import GHC.Types.Id
import GHC.Types.Id.Make
import GHC.Types.Literal
import GHC.Types.ForeignStubs
import GHC.Types.SourceText
import GHC.Types.Name
import GHC.Types.RepType
import GHC.Types.ForeignCall
import GHC.Types.Basic
import GHC.Types.Unique

import GHC.Unit.Module

import GHC.Tc.Utils.TcType

import GHC.Cmm.Expr
import GHC.Cmm.Utils

import GHC.JS.Ppr

import GHC.Driver.DynFlags
import GHC.Driver.Config

import GHC.Builtin.Types
import GHC.Builtin.Types.Prim
import GHC.Builtin.Names

import GHC.Data.FastString
import GHC.Data.Maybe

import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Encoding

dsJsFExport
  :: Id                 -- Either the exported Id,
                        -- or the foreign-export-dynamic constructor
  -> Coercion           -- Coercion between the Haskell type callable
                        -- from C, and its representation type
  -> CLabelString       -- The name to export to C land
  -> CCallConv
  -> Bool               -- True => foreign export dynamic
                        --         so invoke IO action that's hanging off
                        --         the first argument's stable pointer
  -> DsM ( CHeader      -- contents of Module_stub.h
         , CStub        -- contents of Module_stub.c
         , String       -- string describing type to pass to createAdj.
         )

dsJsFExport fn_id co ext_name cconv isDyn = do
    let
       ty                              = coercionRKind co
       (_tvs,sans_foralls)             = tcSplitForAllTyVars ty
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
    platform <- targetPlatform <$> getDynFlags
    return $
      mkFExportJSBits platform ext_name
                     (if isDyn then Nothing else Just fn_id)
                     (map scaledThing fe_arg_tys) res_ty is_IO_res_ty cconv

mkFExportJSBits
  :: Platform
  -> FastString
  -> Maybe Id      -- Just==static, Nothing==dynamic
  -> [Type]
  -> Type
  -> Bool          -- True <=> returns an IO type
  -> CCallConv
  -> (CHeader,
      CStub,
      String       -- the argument reps
     )
mkFExportJSBits platform c_nm maybe_target arg_htys res_hty is_IO_res_ty _cconv
 = (header_bits, js_bits, type_string)
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
                 typeCmmType platform (getPrimTyOf ty))
              | (ty,n) <- zip arg_htys [1::Int ..] ]

  arg_cname n _stg_ty = text ('a':show n)

  type_string = primTyDescChar platform res_hty : arg_type_string

  arg_type_string = [primTyDescChar platform ty | (_,_,ty,_) <- arg_info]

  -- stuff to do with the return type of the JS function
  res_hty_is_unit = res_hty `eqType` unitTy     -- Look through any newtypes

  unboxResType | res_hty_is_unit = text "h$rts_getUnit"
               | otherwise       = unpackHObj res_hty

  header_bits = maybe mempty idTag maybe_target
  idTag i = let (tag, u) = unpkUnique (getUnique i)
            in  CHeader (char tag <> word64 u)

  normal_args = map (\(nm,_ty,_,_) -> nm) arg_info
  all_args
    | isNothing maybe_target = text "stableptr_offset" : normal_args
    | otherwise              = normal_args

  fun_args
    | null arg_info = empty -- text "void"
    | otherwise     = hsep $ punctuate comma all_args

  fun_proto
      = text "function" <+>
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
                          strlit (unitIdFS (moduleUnitId m)) <> comma <>
                          strlit (moduleNameFS (moduleName m)) <> comma <>
                          strlit c_nm <> comma <>
                          strlit (mkFastString type_string)
                        ) <> semi
          _ -> empty

  strlit xs = pprStringLit xs

  -- the target which will form the root of what we ask rts_evalIO to run
  the_cfun
     = case maybe_target of
          Nothing    -> text "h$deRefStablePtr(stableptr_offset)"
          Just hs_fn -> idClosureText hs_fn

  -- the expression we give to rts_eval
  expr_to_run :: SDoc
  expr_to_run
     = foldl appArg the_cfun arg_info
       where
          appArg acc (arg_cname, _, arg_hty, _)
             = text "h$rts_apply"
               <> parens (acc <> comma <> mkHObj arg_hty <> parens arg_cname)

  -- finally, the whole darn thing
  js_bits = CStub { getCStub        = js_sdoc
                  , getInitializers = mempty
                  , getFinalizers   = mempty
                  }
       where js_sdoc = space
               $$ fun_proto
               $$ vcat
                 [ lbrace
                 ,   text "return"
                     <+> text "h$rts_eval_sync"
                     <> parens ((if is_IO_res_ty
                                 then expr_to_run
                                 else text "h$rts_toIO" <> parens expr_to_run)
                                <> comma <+> unboxResType)
                     <> semi
                 , rbrace
                 ]
               $$ fun_export

idClosureText :: Id -> SDoc
idClosureText i
  | isExportedId i
  , name <- getName i
  , Just m <- nameModule_maybe name
  = let str = renderWithContext defaultSDocContext (pprFullName m (localiseName name))
    in text "h$" <> text (zEncodeString str)
  | otherwise
  = panic "idClosureText: unknown module"

-- | Desugaring of JavaScript foreign imports
dsJsImport
  :: Id
  -> Coercion
  -> CImportSpec
  -> CCallConv
  -> Safety
  -> Maybe Header
  -> DsM ([Binding], CHeader, CStub)
dsJsImport id co (CLabel cid) _ _ _ = do
   let ty = coercionLKind co
       fod = case tyConAppTyCon_maybe (dropForAlls ty) of
             Just tycon
              | tyConUnique tycon == funPtrTyConKey ->
                 IsFunction
             _ -> IsData
   (_resTy, foRhs) <- jsResultWrapper ty
--   ASSERT(fromJust resTy `eqType` addrPrimTy)    -- typechecker ensures this
   let rhs = foRhs (Lit (LitLabel cid fod))
       rhs' = Cast rhs co

   return ([(id, rhs')], mempty, mempty)

dsJsImport id co (CFunction target) cconv@PrimCallConv safety _
  = dsPrimCall id co (CCall (CCallSpec target cconv safety))
dsJsImport id co (CFunction target) cconv safety mHeader
  = dsJsCall id co (CCall (CCallSpec target cconv safety)) mHeader
dsJsImport id co CWrapper cconv _ _
  = dsJsFExportDynamic id co cconv

-- fixme work in progress
-- FIXME (Sylvain 2022-03): possibility of code sharing with dsFExportDynamic?
-- Lot of duplication
dsJsFExportDynamic :: Id
                 -> Coercion
                 -> CCallConv
                 -> DsM ([Binding], CHeader, CStub)
dsJsFExportDynamic id co0 cconv = do
    let
      ty                            = coercionLKind co0
      (tvs,sans_foralls)            = tcSplitForAllTyVars ty
      ([scaled_arg_ty], fn_res_ty)  = tcSplitFunTys sans_foralls
      arg_ty                        = scaledThing scaled_arg_ty
      (io_tc, res_ty)               = expectJust "dsJsFExportDynamic: IO type expected"
                                        -- Must have an IO type; hence Just
                                        $ tcSplitIOType_maybe fn_res_ty
    mod <- getModule
    let fe_nm = mkFastString $ zEncodeString
            ("h$" ++ moduleStableString mod ++ "$" ++ toJsName id)
        -- Construct the label based on the passed id, don't use names
        -- depending on Unique. See #13807 and Note [Unique Determinism].
    cback <- newSysLocalDs scaled_arg_ty
    newStablePtrId <- dsLookupGlobalId newStablePtrName
    stable_ptr_tycon <- dsLookupTyCon stablePtrTyConName
    let
        stable_ptr_ty = mkTyConApp stable_ptr_tycon [arg_ty]
        export_ty     = mkVisFunTyMany stable_ptr_ty arg_ty
    bindIOId <- dsLookupGlobalId bindIOName
    stbl_value <- newSysLocalMDs stable_ptr_ty
    (h_code, c_code, typestring) <- dsJsFExport id (mkRepReflCo export_ty) fe_nm cconv True
    let
         {-
          The arguments to the external function which will
          create a little bit of (template) code on the fly
          for allowing the (stable pointed) Haskell closure
          to be entered using an external calling convention
          (ccall).
         -}
        adj_args      = [ Var stbl_value
                        , Lit (LitLabel fe_nm IsFunction)
                        , Lit (mkLitString typestring)
                        ]
          -- name of external entry point providing these services.
          -- (probably in the RTS.)
        adjustor   = fsLit "createAdjustor"

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

        fed = (id `setInlineActivation` NeverActive noExtField, Cast io_app co0)
               -- Never inline the f.e.d. function, because the litlit
               -- might not be in scope in other modules.

    return ([fed], h_code, c_code)

toJsName :: Id -> String
toJsName i = renderWithContext defaultSDocContext (pprCode (ppr (idName i)))

dsJsCall :: Id -> Coercion -> ForeignCall -> Maybe Header
        -> DsM ([(Id, Expr TyVar)], CHeader, CStub)
dsJsCall fn_id co (CCall (CCallSpec target cconv safety)) _mDeclHeader = do
    let
        ty                   = coercionLKind co
        (tv_bndrs, rho)      = tcSplitForAllTyVarBinders ty
        (arg_tys, io_res_ty) = tcSplitFunTys rho

    args <- newSysLocalsDs arg_tys  -- no FFI levity-polymorphism
    (val_args, arg_wrappers) <- mapAndUnzipM unboxJsArg (map Var args)

    let
        work_arg_ids  = [v | Var v <- val_args] -- All guaranteed to be vars

    (ccall_result_ty, res_wrapper) <- boxJsResult io_res_ty

    ccall_uniq <- newUnique
    work_uniq  <- newUnique

    simpl_opts <- initSimpleOpts <$> getDynFlags

    let
        -- Build the worker
        fcall         = CCall (CCallSpec target cconv safety)
        worker_ty     = mkForAllTys tv_bndrs (mkVisFunTysMany (map idType work_arg_ids) ccall_result_ty)
        tvs           = map binderVar tv_bndrs
        the_ccall_app = mkFCall ccall_uniq fcall val_args ccall_result_ty
        work_rhs      = mkLams tvs (mkLams work_arg_ids the_ccall_app)
        work_id       = mkSysLocal (fsLit "$wccall") work_uniq ManyTy worker_ty

        -- Build the wrapper
        work_app     = mkApps (mkVarApps (Var work_id) tvs) val_args
        wrapper_body = foldr ($) (res_wrapper work_app) arg_wrappers
        wrap_rhs     = mkLams (tvs ++ args) wrapper_body
        wrap_rhs'    = Cast wrap_rhs co
        fn_id_w_inl  = fn_id
                       `setIdUnfolding`
                       mkInlineUnfoldingWithArity simpl_opts VanillaSrc
                                                  (length args)  wrap_rhs'

    return ([(work_id, work_rhs), (fn_id_w_inl, wrap_rhs')], mempty, mempty)


mkHObj :: Type -> SDoc
mkHObj t = text "h$rts_mk" <> showFFIType t

unpackHObj :: Type -> SDoc
unpackHObj t = text "h$rts_get" <> showFFIType t

showStgType :: Type -> SDoc
showStgType t = text "Hs" <> showFFIType t

showFFIType :: Type -> SDoc
showFFIType t = ftext (occNameFS (getOccName (typeTyCon t)))

typeTyCon :: Type -> TyCon
typeTyCon ty
  -- UnaryRep rep_ty <- repType ty
  | Just (tc, _) <- tcSplitTyConApp_maybe (unwrapType ty) -- rep_ty
  = tc
  | otherwise
  = pprPanic "typeTyCon" (ppr ty)


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
              \ body -> mkWildCase arg (unrestricted boolTy) (exprType body) [Alt DEFAULT [] body])

  | Just tc <- tyConAppTyCon_maybe arg_ty,
    tc `hasKey` anyTyConKey
  = return (arg,
              \ body -> mkWildCase arg (unrestricted arg_ty) (exprType body) [Alt DEFAULT [] body])
  -- Data types with a single constructor, which has a single, primitive-typed arg
  -- This deals with Int, Float etc; also Ptr, ForeignPtr
  | is_product_type && data_con_arity == 1
    = do case_bndr <- newSysLocalMDs arg_ty
         prim_arg <- newSysLocalMDs (scaledThing data_con_arg_ty1)
         return (Var prim_arg,
               \ body -> Case arg case_bndr (exprType body) [Alt (DataAlt data_con) [prim_arg] body]
              )

  -- Byte-arrays, both mutable and otherwise; hack warning
  -- We're looking for values of type ByteArray, MutableByteArray
  --    data ByteArray          ix = ByteArray        ix ix ByteArray#
  --    data MutableByteArray s ix = MutableByteArray ix ix (MutableByteArray# s)
  | is_product_type &&
    data_con_arity == 3,
    Just arg3_tycon <- maybe_arg3_tycon,
    (arg3_tycon ==  byteArrayPrimTyCon ||
     arg3_tycon ==  mutableByteArrayPrimTyCon)
  = do case_bndr <- newSysLocalMDs arg_ty
       vars@[_l_var, _r_var, arr_cts_var] <- newSysLocalsDs data_con_arg_tys
       return (Var arr_cts_var,
               \ body -> Case arg case_bndr (exprType body) [Alt (DataAlt data_con) vars body]
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
    maybe_arg3_tycon               = tyConAppTyCon_maybe (scaledThing data_con_arg_ty3)


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
boxJsResult :: Type -> DsM (Type, CoreExpr -> CoreExpr)
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
                = mkCoreUnboxedTuple [state, ans]

        ; (ccall_res_ty, the_alt) <- mk_alt return_result res

        ; state_id <- newSysLocalMDs realWorldStatePrimTy
        ; let io_data_con = head (tyConDataCons io_tycon)
              toIOCon     = dataConWrapId io_data_con

              wrap the_call =
                              mkApps (Var toIOCon)
                                     [ Type io_res_ty,
                                       Lam state_id $
                                       mkWildCase (App the_call (Var state_id))
                                             (unrestricted ccall_res_ty)
                                             (coreAltType the_alt)
                                             [the_alt]
                                     ]

        ; return (realWorldStatePrimTy `mkVisFunTyMany` ccall_res_ty, wrap) }

boxJsResult result_ty
  = do -- It isn't IO, so do unsafePerformIO
       -- It's not conveniently available, so we inline it
       res <- jsResultWrapper result_ty
       (ccall_res_ty, the_alt) <- mk_alt return_result res
       let
           wrap = \ the_call -> mkWildCase (App the_call (Var realWorldPrimId))
                                           (unrestricted ccall_res_ty)
                                           (coreAltType the_alt)
                                           [the_alt]
       return (realWorldStatePrimTy `mkVisFunTyMany` ccall_res_ty, wrap)
  where
    return_result _ ans = ans

mk_alt :: (Expr Var -> Expr Var -> Expr Var)
       -> (Maybe Type, Expr Var -> Expr Var)
       -> DsM (Type, CoreAlt)
mk_alt return_result (Nothing, wrap_result)
  = do -- The ccall returns ()
       state_id <- newSysLocalMDs realWorldStatePrimTy
       let
             the_rhs = return_result (Var state_id)
                                     (wrap_result $ panic "jsBoxResult")
             ccall_res_ty = mkTupleTy Unboxed [realWorldStatePrimTy]
             the_alt      = Alt (DataAlt (tupleDataCon Unboxed 1)) [state_id] the_rhs
       return (ccall_res_ty, the_alt)

mk_alt return_result (Just prim_res_ty, wrap_result)
                -- The ccall returns a non-() value
  | isUnboxedTupleType prim_res_ty = do
    let
        ls = dropRuntimeRepArgs (tyConAppArgs prim_res_ty)
        arity = 1 + length ls
    args_ids <- newSysLocalsMDs ls
    state_id <- newSysLocalMDs realWorldStatePrimTy
    let
        result_tup = mkCoreUnboxedTuple (map Var args_ids)
        the_rhs = return_result (Var state_id)
                                (wrap_result result_tup)
        ccall_res_ty = mkTupleTy Unboxed (realWorldStatePrimTy : ls)
        the_alt      = Alt (DataAlt (tupleDataCon Unboxed arity))
                           (state_id : args_ids)
                          the_rhs
    return (ccall_res_ty, the_alt)

  | otherwise = do
    result_id <- newSysLocalMDs prim_res_ty
    state_id <- newSysLocalMDs realWorldStatePrimTy
    let
        the_rhs = return_result (Var state_id)
                                (wrap_result (Var result_id))
        ccall_res_ty = mkTupleTy Unboxed [realWorldStatePrimTy, prim_res_ty]
        the_alt      = Alt (DataAlt (tupleDataCon Unboxed 2)) [state_id, result_id] the_rhs
    return (ccall_res_ty, the_alt)

jsResultWrapper
  :: Type
  -> DsM ( Maybe Type           -- Type of the expected result, if any
         , CoreExpr -> CoreExpr -- Wrapper for the result
         )
-- resultWrapper deals with the result *value*
-- E.g. foreign import foo :: Int -> IO T
-- Then resultWrapper deals with marshalling the 'T' part
jsResultWrapper result_ty
  -- Base case 1a: unboxed tuples
  | Just (tc, args) <- splitTyConApp_maybe result_ty
  , isUnboxedTupleTyCon tc {- && False -} = do
    let args' = dropRuntimeRepArgs args
    (tys, wrappers) <- unzip <$> mapM jsResultWrapper args'
    matched <- mapM (mapM newSysLocalMDs) tys
    let tys'    = catMaybes tys
        -- arity   = length args'
        -- resCon  = tupleDataCon Unboxed (length args)
        err     = panic "jsResultWrapper: used Id with result type Nothing"
        resWrap :: CoreExpr
        resWrap = mkCoreUnboxedTuple (zipWith (\w -> w . Var . fromMaybe err) wrappers matched)
    return $
      if null tys'
        then (Nothing, \_ -> resWrap)
        else let innerArity = length tys'
                 innerTy    = mkTupleTy Unboxed tys'
                 innerCon   = tupleDataCon Unboxed innerArity
                 inner :: CoreExpr -> CoreExpr
                 inner e    = mkWildCase e (unrestricted innerTy) result_ty
                                         [Alt (DataAlt innerCon)
                                              (catMaybes matched)
                                              resWrap
                                         ]
             in (Just innerTy, inner)

  -- Base case 1b: primitive types
  | isPrimitiveType result_ty
  = return (Just result_ty, \e -> e)
  -- Base case 1c: boxed tuples
  | Just (tc, args) <- maybe_tc_app
  , isBoxedTupleTyCon tc = do
      let innerTy = mkTupleTy Unboxed args
      (inner_res, w) <- jsResultWrapper innerTy
      matched <- newSysLocalsMDs args
      let inner e = mkWildCase (w e) (unrestricted innerTy) result_ty
                               [ Alt (DataAlt (tupleDataCon Unboxed (length args)))
                                     matched
                                     (mkCoreTup (map Var matched))
                                -- mkCoreConApps (tupleDataCon Boxed (length args)) (map Type args ++ map Var matched)
                               ]
      return (inner_res, inner)

  -- Base case 2: the unit type ()
  | Just (tc,_) <- maybe_tc_app, tc `hasKey` unitTyConKey
  = return (Nothing, \_ -> Var unitDataConId)

  -- Base case 3: the boolean type
  | Just (tc,_) <- maybe_tc_app, tc `hasKey` boolTyConKey = do
--    result_id <- newSysLocalDs boolTy
    ccall_uniq <- newUnique
    let forceBool e = mkJsCall ccall_uniq (fsLit "((x) => { return !(!x); })") [e] boolTy
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
  | Just (tyvar, rest) <- splitForAllTyCoVar_maybe result_ty
  = do (maybe_ty, wrapper) <- jsResultWrapper rest
       return (maybe_ty, \e -> Lam tyvar (wrapper e))

  -- Data types with a single constructor, which has a single arg
  -- This includes types like Ptr and ForeignPtr
  | Just (_tycon, tycon_arg_tys, data_con, data_con_arg_tys) <- splitDataProductType_maybe result_ty,
    dataConSourceArity data_con == 1
  = do let (unwrapped_res_ty : _) = data_con_arg_tys
       (maybe_ty, wrapper) <- jsResultWrapper (scaledThing unwrapped_res_ty)
       return
         (maybe_ty, \e -> mkApps (Var (dataConWrapId data_con))
                                 (map Type tycon_arg_tys ++ [wrapper e]))

  | otherwise
  = pprPanic "jsResultWrapper" (ppr result_ty)
  where
    maybe_tc_app = splitTyConApp_maybe result_ty

-- low-level primitive JavaScript call:
mkJsCall :: Unique -> FastString -> [CoreExpr] -> Type -> CoreExpr
mkJsCall u tgt args t = mkFCall u ccall args t
  where
    ccall = CCall $ CCallSpec
              (StaticTarget NoSourceText tgt (Just primUnit) True)
              JavaScriptCallConv
              PlayRisky
