{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module GHC.HsToCore.Foreign.Wasm
  ( dsWasmJSImport,
    dsWasmJSExport,
  )
where

import Data.List
  ( intercalate,
    stripPrefix,
  )
import Data.Maybe
import GHC.Builtin.Names
import GHC.Builtin.Types
import GHC.Builtin.Types.Prim
import GHC.Core
import GHC.Core.Coercion
import GHC.Core.DataCon
import GHC.Core.Make
import GHC.Core.Multiplicity
import GHC.Core.TyCon
import GHC.Core.Type
import GHC.Core.Utils
import GHC.Data.FastString
import GHC.Hs
import GHC.HsToCore.Foreign.Call
import GHC.HsToCore.Foreign.Utils
import GHC.HsToCore.Monad
import GHC.HsToCore.Types
import GHC.Iface.Env
import GHC.Prelude
import GHC.Tc.Utils.Env
import GHC.Tc.Utils.Monad
import GHC.Tc.Utils.TcType
import GHC.Types.ForeignCall
import GHC.Types.ForeignStubs
import GHC.Types.Id
import GHC.Types.Name
import GHC.Types.SourceText
import GHC.Types.SrcLoc
import GHC.Types.Var
import GHC.Unit
import GHC.Utils.Outputable
import GHC.Utils.Panic
import Language.Haskell.Syntax.Basic

dsWasmJSImport ::
  Id ->
  Coercion ->
  CImportSpec ->
  Safety ->
  DsM ([Binding], CHeader, CStub, [Id])
dsWasmJSImport id co (CFunction (StaticTarget _ js_src mUnitId _)) safety
  | js_src == "wrapper" = dsWasmJSDynamicExport id co mUnitId
  | otherwise = do
      (bs, h, c) <- dsWasmJSStaticImport id co (unpackFS js_src) mUnitId safety
      pure (bs, h, c, [])
dsWasmJSImport _ _ _ _ = panic "dsWasmJSImport: unreachable"

{-

Note [Desugaring JSFFI dynamic export]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A JSFFI dynamic export wraps a Haskell function as a JavaScript
callback:

foreign import javascript "wrapper"
  mk_wrapper :: HsFuncType -> IO JSVal

We desugar it to three bindings under the hood:

1. The worker function

mk_wrapper_worker :: StablePtr HsFuncType -> HsFuncType
mk_wrapper_worker sp = unsafeDupablePerformIO (deRefStablePtr sp)

No need to bother with eta-expansion here. Also, the worker function
is marked as a JSFFI static export.

2. The adjustor function

foreign import javascript unsafe "(...args) => __exports.mk_wrapper_worker($1, ...args)"
  mk_wrapper_adjustor :: StablePtr HsFuncType -> IO JSVal

It generates a JavaScript callback that captures the stable pointer.
When the callback is invoked later, it calls our worker function and
passes the stable pointer as well as the rest of the arguments.

3. The wrapper function

mk_wrapper :: HsFuncType -> IO JSVal
mk_wrapper = mkJSCallback mk_wrapper_adjustor

This is the user-facing mk_wrapper binding. It allocates a stable
pointer for the Haskell function closure, then calls the adjustor
function to fetch the JSVal that represents the JavaScript callback.
The JSVal as returned by the adjustor is not returned directly; it has
a StablePtr# field which is NULL by default, but for JSFFI dynamic
exports, it's set to the Haskell function's stable pointer. This way,
when we call freeJSVal, the Haskell function can be freed as well.

-}

dsWasmJSDynamicExport ::
  Id -> Coercion -> Maybe Unit -> DsM ([Binding], CHeader, CStub, [Id])
dsWasmJSDynamicExport fn_id co mUnitId = do
  sp_tycon <- dsLookupTyCon stablePtrTyConName
  let ty = coercionLKind co
      (tv_bndrs, fun_ty) = tcSplitForAllTyVarBinders ty
      ([Scaled ManyTy arg_ty], io_jsval_ty) = tcSplitFunTys fun_ty
      sp_ty = mkTyConApp sp_tycon [arg_ty]
      (real_arg_tys, _) = tcSplitFunTys arg_ty
  sp_id <- newSysLocalDs ManyTy sp_ty
  work_uniq <- newUnique
  work_export_name <- uniqueCFunName
  deRefStablePtr_id <- lookupGhcInternalVarId "GHC.Internal.Stable" "deRefStablePtr"
  unsafeDupablePerformIO_id <-
    lookupGhcInternalVarId
      "GHC.Internal.IO.Unsafe"
      "unsafeDupablePerformIO"
  let work_id =
        mkExportedVanillaId
          ( mkExternalName
              work_uniq
              (nameModule $ getName fn_id)
              (mkVarOcc $ "jsffi_" ++ occNameString (getOccName fn_id) ++ "_work")
              generatedSrcSpan
          )
          work_ty
      work_rhs =
        mkCoreLams ([tv | Bndr tv _ <- tv_bndrs] ++ [sp_id])
          $ mkApps
            (Var unsafeDupablePerformIO_id)
            [Type arg_ty, mkApps (Var deRefStablePtr_id) [Type arg_ty, Var sp_id]]
      work_ty = exprType work_rhs
  (work_h, work_c, _, work_ids, work_bs) <-
    dsWasmJSExport
      work_id
      (mkRepReflCo work_ty)
      work_export_name
  adjustor_uniq <- newUnique
  let adjustor_id =
        mkExportedVanillaId
          ( mkExternalName
              adjustor_uniq
              (nameModule $ getName fn_id)
              ( mkVarOcc
                  $ "jsffi_"
                  ++ occNameString (getOccName fn_id)
                  ++ "_adjustor"
              )
              generatedSrcSpan
          )
          adjustor_ty
      adjustor_ty = mkForAllTys tv_bndrs $ mkVisFunTysMany [sp_ty] io_jsval_ty
      adjustor_js_src =
        "("
          ++ intercalate "," ["a" ++ show i | i <- [1 .. length real_arg_tys]]
          ++ ") => __exports."
          ++ unpackFS work_export_name
          ++ "($1"
          ++ mconcat [",a" ++ show i | i <- [1 .. length real_arg_tys]]
          ++ ")"
  (adjustor_bs, adjustor_h, adjustor_c) <-
    dsWasmJSStaticImport
      adjustor_id
      (mkRepReflCo adjustor_ty)
      adjustor_js_src
      mUnitId
      PlayRisky
  mkJSCallback_id <- lookupGhcInternalVarId "GHC.Internal.Wasm.Prim.Exports" "mkJSCallback"
  let wrap_rhs =
        mkCoreLams [tv | Bndr tv _ <- tv_bndrs]
          $ mkApps
            (Var mkJSCallback_id)
            [ Type arg_ty,
              mkApps
                (Var adjustor_id)
                [Type $ mkTyVarTy tv | Bndr tv _ <- tv_bndrs]
            ]
  pure
    ( [(fn_id, Cast wrap_rhs co), (work_id, work_rhs)] ++ work_bs ++ adjustor_bs,
      work_h `mappend` adjustor_h,
      work_c `mappend` adjustor_c,
      work_ids
    )

{-

Note [Desugaring JSFFI import]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The simplest case is JSFFI sync import, those marked as unsafe. It is
implemented on top of C FFI unsafe import.

Unlike C FFI which generates a worker/wrapper pair that unboxes the
arguments and boxes the result in Haskell, we only desugar to a single
Haskell binding that case-binds the arguments to ensure they're
evaluated, then passes the boxed arguments directly to C and receive
the boxed result from C as well.

This is of course less efficient than how C FFI does it, and unboxed
FFI types aren't supported, but it's the easiest way to implement it,
especially since leaving all the boxing/unboxing business to C unifies
the implementation of JSFFI imports and exports.

Now, each sync import calls a generated C function with a unique
symbol. The C function uses rts_get* to unbox the arguments, call into
JavaScript, then boxes the result with rts_mk* and returns it to
Haskell. But wait, how on earth is C able to call into JavaScript
here!? The secret is using a wasm import:

__attribute__((import_module("ghc_wasm_jsffi"), import_name("my_js_func")))
HsJSVal worker_func(HsInt a1, HsJSVal a2);

Wasm imports live in the same namespace as other wasm functions, so
our C wrapper function can call into this imported worker function,
which will literally be the user written JavaScript function with
binders $1, $2, etc.

So far so good, but how does the source code snippet go from Haskell
source files to the JavaScript module which provides the
ghc_wasm_jsffi imports to be used by the wasm module at runtime? The
secret is embedding the source code snippets in a wasm custom section
named ghc_wasm_jsffi:

.section .custom_section.ghc_wasm_jsffi,"",@
.asciz my_js_func
.asciz ($1, $2)
.asciz js_code_containing($1, $2)

At link time, for all object files touched by wasm-ld, all
ghc_wasm_jsffi sections are concatenated into a single ghc_wasm_jsffi
section in the output wasm module. And then, a simple "post-linker"
program can parse the payload of that section and emit a JavaScript
module. Note that above is assembly source file, but we're only
generating a C stub, so we need to smuggle the assembly code into C
via __asm__.

JSFFI async import is implemented on top of JSFFI sync import. We
still desugar it to a single Haskell binding that calls C, with some
subtle differences:

- The C result type is always a boxed JSVal that represents the
  JavaScript Promise, instead of the actual Haskell result type.
- In the custom section payload, we emit "async ($1, $2)" instead of
  "($1, $2)". As you can see, it is the arrow function binder, and the
  post-linker will respect the async binder and allow await in the
  function body.
- The C import is also marked as safe. This is required since the
  JavaScript code may re-enter Haskell. If re-entrance only happens in
  future event loop tasks, it's fine to mark the C import as unsafe
  since the current Haskell execution context has already been freed
  at that point, but there's no such guarantee, so better safe than
  sorry here.

Now we have the Promise JSVal, we apply stg_blockPromise to it to get
a thunk with the desired return type. When the thunk is forced, it
will block the forcing thread and wait for the Promise to resolve or
reject. See Note [stg_blockPromise] for detailed explanation of how it
works.

-}

dsWasmJSStaticImport ::
  Id ->
  Coercion ->
  String ->
  Maybe Unit ->
  Safety ->
  DsM ([Binding], CHeader, CStub)
dsWasmJSStaticImport fn_id co js_src' mUnitId safety = do
  cfun_name <- uniqueCFunName
  let ty = coercionLKind co
      (tvs, fun_ty) = tcSplitForAllInvisTyVars ty
      (arg_tys, orig_res_ty) = tcSplitFunTys fun_ty
      (res_ty, is_io) = case tcSplitIOType_maybe orig_res_ty of
        Just (_, res_ty) -> (res_ty, True)
        Nothing -> (orig_res_ty, False)
      js_src
        -- Just desugar it to a JSFFI import with source text "$1($2,
        -- ...)", with the same type signature and safety annotation.
        | js_src' == "dynamic" =
            "$1("
              ++ intercalate "," ["$" ++ show i | i <- [2 .. length arg_tys]]
              ++ ")"
        | otherwise =
            js_src'
  case safety of
    PlayRisky -> do
      rhs <-
        importBindingRHS
          mUnitId
          PlayRisky
          cfun_name
          tvs
          arg_tys
          orig_res_ty
          id
      pure
        ( [(fn_id, Cast rhs co)],
          CHeader commonCDecls,
          importCStub
            PlayRisky
            cfun_name
            (map scaledThing arg_tys)
            res_ty
            js_src
        )
    _ -> do
      io_tycon <- dsLookupTyCon ioTyConName
      jsval_ty <- mkTyConTy <$> lookupGhcInternalTyCon "GHC.Internal.Wasm.Prim.Types" "JSVal"
      bindIO_id <- dsLookupGlobalId bindIOName
      returnIO_id <- dsLookupGlobalId returnIOName
      promise_id <- newSysLocalDs ManyTy jsval_ty
      blockPromise_id <- lookupGhcInternalVarId "GHC.Internal.Wasm.Prim.Imports" "stg_blockPromise"
      msgPromise_id <-
        lookupGhcInternalVarId "GHC.Internal.Wasm.Prim.Imports" $ "stg_messagePromise" ++ ffiType res_ty
      unsafeDupablePerformIO_id <-
        lookupGhcInternalVarId
          "GHC.Internal.IO.Unsafe"
          "unsafeDupablePerformIO"
      rhs <-
        importBindingRHS
          mUnitId
          PlaySafe
          cfun_name
          tvs
          arg_tys
          (mkTyConApp io_tycon [jsval_ty])
          $ ( if is_io
                then id
                else \m_res ->
                  mkApps (Var unsafeDupablePerformIO_id) [Type res_ty, m_res]
            )
          -- (m_promise :: IO JSVal) >>= (\promise -> return (stg_blockPromise promise msgPromise))
          -- stg_blockPromise returns the thunk
          . ( \m_promise ->
                mkApps
                  (Var bindIO_id)
                  [ Type jsval_ty,
                    Type res_ty,
                    m_promise,
                    Lam promise_id
                      $ mkApps
                        (Var returnIO_id)
                        [ Type res_ty,
                          mkApps
                            (Var blockPromise_id)
                            [Type res_ty, Var promise_id, Var msgPromise_id]
                        ]
                  ]
            )
      pure
        ( [(fn_id, Cast rhs co)],
          CHeader commonCDecls,
          importCStub
            PlaySafe
            cfun_name
            (map scaledThing arg_tys)
            jsval_ty
            js_src
        )

uniqueCFunName :: DsM FastString
uniqueCFunName = do
  cfun_num <- ds_next_wrapper_num <$> getGblEnv
  mkWrapperName cfun_num "ghc_wasm_jsffi" ""

importBindingRHS ::
  Maybe Unit ->
  Safety ->
  FastString ->
  [TyVar] ->
  [Scaled Type] ->
  Type ->
  (CoreExpr -> CoreExpr) ->
  DsM CoreExpr
importBindingRHS mUnitId safety cfun_name tvs arg_tys orig_res_ty res_trans =
  do
    ccall_uniq <- newUnique
    args_unevaled <- newSysLocalsDs arg_tys
    args_evaled <- newSysLocalsDs arg_tys
    -- ccall_action_ty: type of the_call, State# RealWorld -> (# State# RealWorld, a #)
    -- res_wrapper: turn the_call to (IO a) or a
    (ccall_action_ty, res_wrapper) <- case tcSplitIOType_maybe orig_res_ty of
      Just (io_tycon, res_ty) -> do
        s0_id <- newSysLocalDs ManyTy realWorldStatePrimTy
        s1_id <- newSysLocalDs ManyTy realWorldStatePrimTy
        let io_data_con = tyConSingleDataCon io_tycon
            toIOCon = dataConWorkId io_data_con
            (ccall_res_ty, wrap)
              | res_ty `eqType` unitTy =
                  ( mkTupleTy Unboxed [realWorldStatePrimTy],
                    \the_call ->
                      mkApps
                        (Var toIOCon)
                        [ Type res_ty,
                          Lam s0_id
                            $ mkWildCase
                              (App the_call (Var s0_id))
                              (unrestricted ccall_res_ty)
                              (mkTupleTy Unboxed [realWorldStatePrimTy, unitTy])
                              [ Alt
                                  (DataAlt (tupleDataCon Unboxed 1))
                                  [s1_id]
                                  (mkCoreUnboxedTuple [Var s1_id, unitExpr])
                              ]
                        ]
                  )
              | otherwise =
                  ( mkTupleTy Unboxed [realWorldStatePrimTy, res_ty],
                    \the_call -> mkApps (Var toIOCon) [Type res_ty, the_call]
                  )
        pure (realWorldStatePrimTy `mkVisFunTyMany` ccall_res_ty, wrap)
      Nothing -> do
        unsafeDupablePerformIO_id <-
          lookupGhcInternalVarId
            "GHC.Internal.IO.Unsafe"
            "unsafeDupablePerformIO"
        io_data_con <- dsLookupDataCon ioDataConName
        let ccall_res_ty =
              mkTupleTy Unboxed [realWorldStatePrimTy, orig_res_ty]
            toIOCon = dataConWorkId io_data_con
            wrap the_call =
              mkApps
                (Var unsafeDupablePerformIO_id)
                [ Type orig_res_ty,
                  mkApps (Var toIOCon) [Type orig_res_ty, the_call]
                ]
        pure (realWorldStatePrimTy `mkVisFunTyMany` ccall_res_ty, wrap)
    let cfun_fcall =
          CCall
            ( CCallSpec
                (StaticTarget NoSourceText cfun_name mUnitId True)
                CCallConv
                safety
            )
        call_app =
          mkFCall ccall_uniq cfun_fcall (map Var args_evaled) ccall_action_ty
        rhs =
          mkCoreLams (tvs ++ args_unevaled)
            $ foldr
              (\(arg_u, arg_e) acc -> mkDefaultCase (Var arg_u) arg_e acc)
              -- res_trans transforms the result. When desugaring
              -- JSFFI sync imports, the result is just (IO a) or a,
              -- and res_trans is id; for async cases, the result is
              -- always (IO JSVal), and res_trans will wrap it in a
              -- thunk that has the original return type. This way, we
              -- can reuse most of the RHS generation logic for both
              -- sync/async imports.
              (res_trans $ res_wrapper call_app)
              (zip args_unevaled args_evaled)
    pure rhs

importCStub :: Safety -> FastString -> [Type] -> Type -> String -> CStub
importCStub safety cfun_name arg_tys res_ty js_src = CStub c_doc [] []
  where
    import_name = fromJust $ stripPrefix "ghczuwasmzujsffi" (unpackFS cfun_name)
    import_asm =
      text "__asm__"
        <> parens
          ( vcat
              [ text (show l)
                | l <-
                    [ ".section .custom_section.ghc_wasm_jsffi,\"\",@\n",
                      ".asciz \"" ++ import_name ++ "\"\n",
                      ".asciz \""
                        ++ ( case safety of
                               PlayRisky -> "("
                               _ -> "async ("
                           )
                        ++ intercalate "," ["$" ++ show i | i <- [1 .. length arg_tys]]
                        ++ ")\"\n",
                      ".asciz " ++ show js_src ++ "\n"
                    ]
              ]
          )
        <> semi
    import_attr =
      text "__attribute__"
        <> parens
          ( parens
              ( hsep
                  ( punctuate
                      comma
                      [ text k <> parens (doubleQuotes (text v))
                        | (k, v) <-
                            [("import_module", "ghc_wasm_jsffi"), ("import_name", import_name)]
                      ]
                  )
              )
          )
    import_proto =
      import_res_ty <+> text import_name <> parens import_args <> semi
    import_res_ty
      | res_ty `eqType` unitTy = text "void"
      | otherwise = text ("Hs" ++ ffiType res_ty)
    import_arg_list =
      [ text ("Hs" ++ ffiType arg_ty) <+> char 'a' <> int i
        | (i, arg_ty) <- zip [1 ..] arg_tys
      ]
    import_args = case import_arg_list of
      [] -> text "void"
      _ -> hsep $ punctuate comma import_arg_list
    cfun_proto = cfun_res_ty <+> ppr cfun_name <> parens cfun_args
    cfun_ret
      | res_ty `eqType` unitTy = cfun_call_import <> semi
      | otherwise = text "return" <+> cfun_call_import <> semi
    cfun_make_arg arg_ty arg_val =
      text ("rts_get" ++ ffiType arg_ty) <> parens arg_val
    cfun_make_ret ret_val
      | res_ty `eqType` unitTy = ret_val
      | otherwise =
          text ("rts_mk" ++ ffiType res_ty)
            -- We can cheat a little bit here since there's only
            -- MainCapability in the single-threaded RTS anyway, so no
            -- need to call rts_unsafeGetMyCapability().
            <> parens (hsep (punctuate comma [text "&MainCapability", ret_val]))
    cfun_call_import =
      cfun_make_ret
        $ text import_name
        <> parens
          ( hsep
              ( punctuate
                  comma
                  [ cfun_make_arg arg_ty (char 'a' <> int n)
                    | (arg_ty, n) <- zip arg_tys [1 ..]
                  ]
              )
          )
    cfun_res_ty
      | res_ty `eqType` unitTy = text "void"
      | otherwise = text "HaskellObj"
    cfun_arg_list =
      [text "HaskellObj" <+> char 'a' <> int n | n <- [1 .. length arg_tys]]
    cfun_args = case cfun_arg_list of
      [] -> text "void"
      _ -> hsep $ punctuate comma cfun_arg_list
    c_doc =
      commonCDecls
        $+$ import_asm
        $+$ import_attr
        $+$ import_proto
        $+$ cfun_proto
        $+$ braces cfun_ret

{-

Note [Desugaring JSFFI static export]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A JSFFI static export wraps a top-level Haskell binding as a wasm
module export that can be called in JavaScript as an async function:

foreign export javascript "plus"
  (+) :: Int -> Int -> Int

Just like generating C stub for a JSFFI import, we need to generate C
stub for a JSFFI export as well:

__attribute__((export_name("plus")))
HsJSVal plus(HsInt a1, HsInt a2) { ... }

At link time, you need to pass -optl-Wl,--export=plus,--export=... to
specify your entrypoint function symbols as roots of wasm-ld link-time
garbage collection. As for the auto-generated exports when desugaring
the JSFFI dynamic exports, they will be transitively included as well
due to the export_name attribute.

For each JSFFI static export, we create an internal worker function
which takes the same arguments as the exported Haskell binding, but
always returns (IO JSVal). Its RHS simply applies the arguments to the
original binding, then applies a runIO/runNonIO top handler function
to the result. The top handler creates a JavaScript Promise that
stands for Haskell evaluation result, schedules Haskell computation to
happen, and the Promise will eventually be resolved with the result or
rejected with an exception. That Promise is what we return in the C
stub function. See Note [Async JSFFI scheduler] for detailed
explanation.

There's nothing else to explain about the C stub function body; just
like C FFI exports, it calls rts_mk* to box the arguments, rts_apply
to apply them to the worker function, evaluates the result, then
unboxes the resulting Promise using rts_getJSVal and returns it.

Now, in JavaScript, once the wasm instance is initialized, you can
directly call these exports and await them, as if they're real
JavaScript async functions.

-}

dsWasmJSExport ::
  Id ->
  Coercion ->
  CLabelString ->
  DsM (CHeader, CStub, String, [Id], [Binding])
dsWasmJSExport fn_id co ext_name = do
  work_uniq <- newUnique
  let ty = coercionRKind co
      (tvs, fun_ty) = tcSplitForAllInvisTyVars ty
      (arg_tys, orig_res_ty) = tcSplitFunTys fun_ty
      (res_ty, is_io) = case tcSplitIOType_maybe orig_res_ty of
        Just (_, res_ty) -> (res_ty, True)
        Nothing -> (orig_res_ty, False)
      (_, res_ty_args) = splitTyConApp res_ty
      res_ty_str = ffiType res_ty
  args <- newSysLocalsDs arg_tys
  promiseRes_id <-
    lookupGhcInternalVarId "GHC.Internal.Wasm.Prim.Exports" $ "js_promiseResolve" ++ res_ty_str
  runIO_id <- lookupGhcInternalVarId "GHC.Internal.Wasm.Prim.Exports" "runIO"
  runNonIO_id <- lookupGhcInternalVarId "GHC.Internal.Wasm.Prim.Exports" "runNonIO"
  let work_id =
        mkExportedVanillaId
          ( mkExternalName
              work_uniq
              (nameModule $ getName fn_id)
              (mkVarOcc $ "jsffi_" ++ occNameString (getOccName fn_id))
              generatedSrcSpan
          )
          (exprType work_rhs)
      work_rhs =
        mkCoreLams (tvs ++ args)
          $ mkApps
            (Var $ if is_io then runIO_id else runNonIO_id)
            [ Type res_ty,
              mkApps (Var promiseRes_id) $ map Type res_ty_args,
              mkApps (Cast (Var fn_id) co)
                $ map (Type . mkTyVarTy) tvs
                ++ map Var args
            ]
      work_closure = ppr work_id <> text "_closure"
      work_closure_decl = text "extern StgClosure" <+> work_closure <> semi
      cstub_attr =
        text "__attribute__"
          <> parens
            (parens $ text "export_name" <> parens (doubleQuotes $ ftext ext_name))
      cstub_arg_list =
        [ text ("Hs" ++ ffiType (scaledThing arg_ty)) <+> char 'a' <> int i
          | (i, arg_ty) <- zip [1 ..] arg_tys
        ]
      cstub_args = case cstub_arg_list of
        [] -> text "void"
        _ -> hsep $ punctuate comma cstub_arg_list
      cstub_proto = text "HsJSVal" <+> ftext ext_name <> parens cstub_args
      cstub_body =
        vcat
          [ lbrace,
            text "Capability *cap = rts_lock();",
            text "HaskellObj ret;",
            -- rts_evalLazyIO is fine, the top handler always returns
            -- an evaluated result
            text "rts_evalLazyIO"
              <> parens
                ( hsep
                    $ punctuate
                      comma
                      [ text "&cap",
                        foldl'
                          ( \acc (i, arg_ty) ->
                              text "rts_apply"
                                <> parens
                                  ( hsep
                                      $ punctuate
                                        comma
                                        [ text "cap",
                                          acc,
                                          text ("rts_mk" ++ ffiType (scaledThing arg_ty))
                                            <> parens
                                              (hsep $ punctuate comma [text "cap", char 'a' <> int i])
                                        ]
                                  )
                          )
                          (char '&' <> work_closure)
                          $ zip [1 ..] arg_tys,
                        text "&ret"
                      ]
                )
              <> semi,
            text "rts_checkSchedStatus"
              <> parens (doubleQuotes (ftext ext_name) <> comma <> text "cap")
              <> semi,
            text "rts_unlock(cap);",
            text "return rts_getJSVal(ret);",
            rbrace
          ]
      cstub =
        commonCDecls
          $+$ work_closure_decl
          $+$ cstub_attr
          $+$ cstub_proto
          $+$ cstub_body
  pure
    ( CHeader commonCDecls,
      CStub cstub [] [],
      "",
      [work_id],
      [(work_id, work_rhs)]
    )

lookupGhcInternalVarId :: FastString -> String -> DsM Id
lookupGhcInternalVarId m v = do
  n <- lookupOrig (mkGhcInternalModule m) (mkVarOcc v)
  dsLookupGlobalId n

lookupGhcInternalTyCon :: FastString -> String -> DsM TyCon
lookupGhcInternalTyCon m t = do
  n <- lookupOrig (mkGhcInternalModule m) (mkTcOcc t)
  dsLookupTyCon n

ffiType :: Type -> String
ffiType = occNameString . getOccName . fst . splitTyConApp

commonCDecls :: SDoc
commonCDecls =
  vcat
    [ text "typedef __externref_t HsJSVal;",
      text "HsJSVal rts_getJSVal(HaskellObj);",
      text "HaskellObj rts_mkJSVal(Capability*, HsJSVal);"
    ]
