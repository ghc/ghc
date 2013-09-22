%
% (c) The University of Glasgow 2006
% (c) The AQUA Project, Glasgow University, 1998
%

Desugaring foreign declarations (see also DsCCall).

\begin{code}
module DsForeign ( dsForeigns
                 , dsForeigns'
                 , dsFImport, dsCImport, dsFCall, dsPrimCall
                 , dsFExport, dsFExportDynamic, mkFExportCBits
                 , toCType
                 , foreignExportInitialiser
                 ) where

#include "HsVersions.h"
import TcRnMonad        -- temp

import TypeRep

import CoreSyn

import DsCCall
import DsMonad

import HsSyn
import DataCon
import CoreUnfold
import Id
import Literal
import Module
import Name
import Type
import TyCon
import Coercion
import TcEnv
import TcType

import CmmExpr
import CmmUtils
import HscTypes
import ForeignCall
import TysWiredIn
import TysPrim
import PrelNames
import BasicTypes
import SrcLoc
import Outputable
import FastString
import DynFlags
import Platform
import Config
import OrdList
import Pair
import Util
import Hooks

import Data.Maybe
import Data.List
\end{code}

Desugaring of @foreign@ declarations is naturally split up into
parts, an @import@ and an @export@  part. A @foreign import@
declaration
\begin{verbatim}
  foreign import cc nm f :: prim_args -> IO prim_res
\end{verbatim}
is the same as
\begin{verbatim}
  f :: prim_args -> IO prim_res
  f a1 ... an = _ccall_ nm cc a1 ... an
\end{verbatim}
so we reuse the desugaring code in @DsCCall@ to deal with these.

\begin{code}
type Binding = (Id, CoreExpr)   -- No rec/nonrec structure;
                                -- the occurrence analyser will sort it all out

dsForeigns :: [LForeignDecl Id]
           -> DsM (ForeignStubs, OrdList Binding)
dsForeigns fos = getHooked dsForeignsHook dsForeigns' >>= ($ fos)

dsForeigns' :: [LForeignDecl Id]
            -> DsM (ForeignStubs, OrdList Binding)
dsForeigns' []
  = return (NoStubs, nilOL)
dsForeigns' fos = do
    fives <- mapM do_ldecl fos
    let
        (hs, cs, idss, bindss) = unzip4 fives
        fe_ids = concat idss
        fe_init_code = map foreignExportInitialiser fe_ids
    --
    return (ForeignStubs
             (vcat hs)
             (vcat cs $$ vcat fe_init_code),
            foldr (appOL . toOL) nilOL bindss)
  where
   do_ldecl (L loc decl) = putSrcSpanDs loc (do_decl decl)

   do_decl (ForeignImport id _ co spec) = do
      traceIf (text "fi start" <+> ppr id)
      (bs, h, c) <- dsFImport (unLoc id) co spec
      traceIf (text "fi end" <+> ppr id)
      return (h, c, [], bs)

   do_decl (ForeignExport (L _ id) _ co (CExport (CExportStatic ext_nm cconv))) = do
      (h, c, _, _) <- dsFExport id co ext_nm cconv False
      return (h, c, [id], [])
\end{code}


%************************************************************************
%*                                                                      *
\subsection{Foreign import}
%*                                                                      *
%************************************************************************

Desugaring foreign imports is just the matter of creating a binding
that on its RHS unboxes its arguments, performs the external call
(using the @CCallOp@ primop), before boxing the result up and returning it.

However, we create a worker/wrapper pair, thus:

        foreign import f :: Int -> IO Int
==>
        f x = IO ( \s -> case x of { I# x# ->
                         case fw s x# of { (# s1, y# #) ->
                         (# s1, I# y# #)}})

        fw s x# = ccall f s x#

The strictness/CPR analyser won't do this automatically because it doesn't look
inside returned tuples; but inlining this wrapper is a Really Good Idea
because it exposes the boxing to the call site.

\begin{code}
dsFImport :: Id
          -> Coercion
          -> ForeignImport
          -> DsM ([Binding], SDoc, SDoc)
dsFImport id co (CImport cconv safety mHeader spec) = do
    (ids, h, c) <- dsCImport id co spec cconv safety mHeader
    return (ids, h, c)

dsCImport :: Id
          -> Coercion
          -> CImportSpec
          -> CCallConv
          -> Safety
          -> Maybe Header
          -> DsM ([Binding], SDoc, SDoc)
dsCImport id co (CLabel cid) cconv _ _ = do
   dflags <- getDynFlags
   let ty = pFst $ coercionKind co
       fod = case tyConAppTyCon_maybe (dropForAlls ty) of
             Just tycon
              | tyConUnique tycon == funPtrTyConKey ->
                 IsFunction
             _ -> IsData
   (resTy, foRhs) <- resultWrapper ty
   ASSERT(fromJust resTy `eqType` addrPrimTy)    -- typechecker ensures this
    let
        rhs = foRhs (Lit (MachLabel cid stdcall_info fod))
        rhs' = Cast rhs co
        stdcall_info = fun_type_arg_stdcall_info dflags cconv ty
    in
    return ([(id, rhs')], empty, empty)

dsCImport id co (CFunction target) cconv@PrimCallConv safety _
  = dsPrimCall id co (CCall (CCallSpec target cconv safety))
dsCImport id co (CFunction target) cconv safety mHeader
  = dsFCall id co (CCall (CCallSpec target cconv safety)) mHeader
dsCImport id co CWrapper cconv _ _
  = dsFExportDynamic id co cconv

-- For stdcall labels, if the type was a FunPtr or newtype thereof,
-- then we need to calculate the size of the arguments in order to add
-- the @n suffix to the label.
fun_type_arg_stdcall_info :: DynFlags -> CCallConv -> Type -> Maybe Int
fun_type_arg_stdcall_info dflags StdCallConv ty
  | Just (tc,[arg_ty]) <- splitTyConApp_maybe ty,
    tyConUnique tc == funPtrTyConKey
  = let
       (_tvs,sans_foralls)        = tcSplitForAllTys arg_ty
       (fe_arg_tys, _orig_res_ty) = tcSplitFunTys sans_foralls
    in Just $ sum (map (widthInBytes . typeWidth . typeCmmType dflags . getPrimTyOf) fe_arg_tys)
fun_type_arg_stdcall_info _ _other_conv _
  = Nothing
\end{code}


%************************************************************************
%*                                                                      *
\subsection{Foreign calls}
%*                                                                      *
%************************************************************************

\begin{code}
dsFCall :: Id -> Coercion -> ForeignCall -> Maybe Header
        -> DsM ([(Id, Expr TyVar)], SDoc, SDoc)
dsFCall fn_id co fcall mDeclHeader = do
    let
        ty                   = pFst $ coercionKind co
        (tvs, fun_ty)        = tcSplitForAllTys ty
        (arg_tys, io_res_ty) = tcSplitFunTys fun_ty
                -- Must use tcSplit* functions because we want to
                -- see that (IO t) in the corner

    args <- newSysLocalsDs arg_tys
    (val_args, arg_wrappers) <- mapAndUnzipM unboxArg (map Var args)

    let
        work_arg_ids  = [v | Var v <- val_args] -- All guaranteed to be vars

    (ccall_result_ty, res_wrapper) <- boxResult io_res_ty

    ccall_uniq <- newUnique
    work_uniq  <- newUnique

    dflags <- getDynFlags
    (fcall', cDoc) <-
              case fcall of
              CCall (CCallSpec (StaticTarget cName mPackageId isFun) CApiConv safety) ->
               do wrapperName <- mkWrapperName "ghc_wrapper" (unpackFS cName)
                  let fcall' = CCall (CCallSpec (StaticTarget wrapperName mPackageId True) CApiConv safety)
                      c = includes
                       $$ fun_proto <+> braces (cRet <> semi)
                      includes = vcat [ text "#include <" <> ftext h <> text ">"
                                      | Header h <- nub headers ]
                      fun_proto = cResType <+> pprCconv <+> ppr wrapperName <> parens argTypes
                      cRet
                       | isVoidRes =                   cCall
                       | otherwise = text "return" <+> cCall
                      cCall = if isFun
                              then ppr cName <> parens argVals
                              else if null arg_tys
                                    then ppr cName
                                    else panic "dsFCall: Unexpected arguments to FFI value import"
                      raw_res_ty = case tcSplitIOType_maybe io_res_ty of
                                   Just (_ioTyCon, res_ty) -> res_ty
                                   Nothing                 -> io_res_ty
                      isVoidRes = raw_res_ty `eqType` unitTy
                      (mHeader, cResType)
                       | isVoidRes = (Nothing, text "void")
                       | otherwise = toCType raw_res_ty
                      pprCconv = ccallConvAttribute CApiConv
                      mHeadersArgTypeList
                          = [ (header, cType <+> char 'a' <> int n)
                            | (t, n) <- zip arg_tys [1..]
                            , let (header, cType) = toCType t ]
                      (mHeaders, argTypeList) = unzip mHeadersArgTypeList
                      argTypes = if null argTypeList
                                 then text "void"
                                 else hsep $ punctuate comma argTypeList
                      mHeaders' = mDeclHeader : mHeader : mHeaders
                      headers = catMaybes mHeaders'
                      argVals = hsep $ punctuate comma
                                    [ char 'a' <> int n
                                    | (_, n) <- zip arg_tys [1..] ]
                  return (fcall', c)
              _ ->
                  return (fcall, empty)
    let
        -- Build the worker
        worker_ty     = mkForAllTys tvs (mkFunTys (map idType work_arg_ids) ccall_result_ty)
        the_ccall_app = mkFCall dflags ccall_uniq fcall' val_args ccall_result_ty
        work_rhs      = mkLams tvs (mkLams work_arg_ids the_ccall_app)
        work_id       = mkSysLocal (fsLit "$wccall") work_uniq worker_ty

        -- Build the wrapper
        work_app     = mkApps (mkVarApps (Var work_id) tvs) val_args
        wrapper_body = foldr ($) (res_wrapper work_app) arg_wrappers
        wrap_rhs     = mkLams (tvs ++ args) wrapper_body
        wrap_rhs'    = Cast wrap_rhs co
        fn_id_w_inl  = fn_id `setIdUnfolding` mkInlineUnfolding (Just (length args)) wrap_rhs'

    return ([(work_id, work_rhs), (fn_id_w_inl, wrap_rhs')], empty, cDoc)
\end{code}


%************************************************************************
%*                                                                      *
\subsection{Primitive calls}
%*                                                                      *
%************************************************************************

This is for `@foreign import prim@' declarations.

Currently, at the core level we pretend that these primitive calls are
foreign calls. It may make more sense in future to have them as a distinct
kind of Id, or perhaps to bundle them with PrimOps since semantically and
for calling convention they are really prim ops.

\begin{code}
dsPrimCall :: Id -> Coercion -> ForeignCall
           -> DsM ([(Id, Expr TyVar)], SDoc, SDoc)
dsPrimCall fn_id co fcall = do
    let
        ty                   = pFst $ coercionKind co
        (tvs, fun_ty)        = tcSplitForAllTys ty
        (arg_tys, io_res_ty) = tcSplitFunTys fun_ty
                -- Must use tcSplit* functions because we want to
                -- see that (IO t) in the corner

    args <- newSysLocalsDs arg_tys

    ccall_uniq <- newUnique
    dflags <- getDynFlags
    let
        call_app = mkFCall dflags ccall_uniq fcall (map Var args) io_res_ty
        rhs      = mkLams tvs (mkLams args call_app)
        rhs'     = Cast rhs co
    return ([(fn_id, rhs')], empty, empty)

\end{code}

%************************************************************************
%*                                                                      *
\subsection{Foreign export}
%*                                                                      *
%************************************************************************

The function that does most of the work for `@foreign export@' declarations.
(see below for the boilerplate code a `@foreign export@' declaration expands
 into.)

For each `@foreign export foo@' in a module M we generate:
\begin{itemize}
\item a C function `@foo@', which calls
\item a Haskell stub `@M.\$ffoo@', which calls
\end{itemize}
the user-written Haskell function `@M.foo@'.

\begin{code}
dsFExport :: Id                 -- Either the exported Id,
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

dsFExport fn_id co ext_name cconv isDyn = do
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
      mkFExportCBits dflags ext_name
                     (if isDyn then Nothing else Just fn_id)
                     fe_arg_tys res_ty is_IO_res_ty cconv
\end{code}

@foreign import "wrapper"@ (previously "foreign export dynamic") lets
you dress up Haskell IO actions of some fixed type behind an
externally callable interface (i.e., as a C function pointer). Useful
for callbacks and stuff.

\begin{verbatim}
type Fun = Bool -> Int -> IO Int
foreign import "wrapper" f :: Fun -> IO (FunPtr Fun)

-- Haskell-visible constructor, which is generated from the above:
-- SUP: No check for NULL from createAdjustor anymore???

f :: Fun -> IO (FunPtr Fun)
f cback =
   bindIO (newStablePtr cback)
          (\StablePtr sp# -> IO (\s1# ->
              case _ccall_ createAdjustor cconv sp# ``f_helper'' <arg info> s1# of
                 (# s2#, a# #) -> (# s2#, A# a# #)))

foreign import "&f_helper" f_helper :: FunPtr (StablePtr Fun -> Fun)

-- and the helper in C:

f_helper(StablePtr s, HsBool b, HsInt i)
{
        rts_evalIO(rts_apply(rts_apply(deRefStablePtr(s),
                                       rts_mkBool(b)), rts_mkInt(i)));
}
\end{verbatim}

\begin{code}
dsFExportDynamic :: Id
                 -> Coercion
                 -> CCallConv
                 -> DsM ([Binding], SDoc, SDoc)
dsFExportDynamic id co0 cconv = do
    fe_id <-  newSysLocalDs ty
    mod <- getModule
    dflags <- getDynFlags
    let
        -- hack: need to get at the name of the C stub we're about to generate.
        -- TODO: There's no real need to go via String with
        -- (mkFastString . zString). In fact, is there a reason to convert
        -- to FastString at all now, rather than sticking with FastZString?
        fe_nm    = mkFastString (zString (zEncodeFS (moduleNameFS (moduleName mod))) ++ "_" ++ toCName dflags fe_id)

    cback <- newSysLocalDs arg_ty
    newStablePtrId <- dsLookupGlobalId newStablePtrName
    stable_ptr_tycon <- dsLookupTyCon stablePtrTyConName
    let
        stable_ptr_ty = mkTyConApp stable_ptr_tycon [arg_ty]
        export_ty     = mkFunTy stable_ptr_ty arg_ty
    bindIOId <- dsLookupGlobalId bindIOName
    stbl_value <- newSysLocalDs stable_ptr_ty
    (h_code, c_code, typestring, args_size) <- dsFExport id (mkReflCo Representational export_ty) fe_nm cconv True
    let
         {-
          The arguments to the external function which will
          create a little bit of (template) code on the fly
          for allowing the (stable pointed) Haskell closure
          to be entered using an external calling convention
          (stdcall, ccall).
         -}
        adj_args      = [ mkIntLitInt dflags (ccallConvToInt cconv)
                        , Var stbl_value
                        , Lit (MachLabel fe_nm mb_sz_args IsFunction)
                        , Lit (mkMachString typestring)
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
\end{code}

%*
%
\subsection{Generating @foreign export@ stubs}
%
%*

For each @foreign export@ function, a C stub function is generated.
The C stub constructs the application of the exported Haskell function
using the hugs/ghc rts invocation API.

\begin{code}
mkFExportCBits :: DynFlags
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
mkFExportCBits dflags c_nm maybe_target arg_htys res_hty is_IO_res_ty cc
 = (header_bits, c_bits, type_string,
    sum [ widthInBytes (typeWidth rep) | (_,_,_,rep) <- aug_arg_info] -- all the args
         -- NB. the calculation here isn't strictly speaking correct.
         -- We have a primitive Haskell type (eg. Int#, Double#), and
         -- we want to know the size, when passed on the C stack, of
         -- the associated C type (eg. HsInt, HsDouble).  We don't have
         -- this information to hand, but we know what GHC's conventions
         -- are for passing around the primitive Haskell types, so we
         -- use that instead.  I hope the two coincide --SDM
    )
 where
  -- list the arguments to the C function
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

  arg_cname n stg_ty
        | libffi    = char '*' <> parens (stg_ty <> char '*') <>
                      ptext (sLit "args") <> brackets (int (n-1))
        | otherwise = text ('a':show n)

  -- generate a libffi-style stub if this is a "wrapper" and libffi is enabled
  libffi = cLibFFI && isNothing maybe_target

  type_string
      -- libffi needs to know the result type too:
      | libffi    = primTyDescChar dflags res_hty : arg_type_string
      | otherwise = arg_type_string

  arg_type_string = [primTyDescChar dflags ty | (_,_,ty,_) <- arg_info]
                -- just the real args

  -- add some auxiliary args; the stable ptr in the wrapper case, and
  -- a slot for the dummy return address in the wrapper + ccall case
  aug_arg_info
    | isNothing maybe_target = stable_ptr_arg : insertRetAddr dflags cc arg_info
    | otherwise              = arg_info

  stable_ptr_arg =
        (text "the_stableptr", text "StgStablePtr", undefined,
         typeCmmType dflags (mkStablePtrPrimTy alphaTy))

  -- stuff to do with the return type of the C function
  res_hty_is_unit = res_hty `eqType` unitTy     -- Look through any newtypes

  cResType | res_hty_is_unit = text "void"
           | otherwise       = showStgType res_hty

  -- when the return type is integral and word-sized or smaller, it
  -- must be assigned as type ffi_arg (#3516).  To see what type
  -- libffi is expecting here, take a look in its own testsuite, e.g.
  -- libffi/testsuite/libffi.call/cls_align_ulonglong.c
  ffi_cResType
     | is_ffi_arg_type = text "ffi_arg"
     | otherwise       = cResType
     where
       res_ty_key = getUnique (getName (typeTyCon res_hty))
       is_ffi_arg_type = res_ty_key `notElem`
              [floatTyConKey, doubleTyConKey,
               int64TyConKey, word64TyConKey]

  -- Now we can cook up the prototype for the exported function.
  pprCconv = ccallConvAttribute cc

  header_bits = ptext (sLit "extern") <+> fun_proto <> semi

  fun_args
    | null aug_arg_info = text "void"
    | otherwise         = hsep $ punctuate comma
                               $ map (\(nm,ty,_,_) -> ty <+> nm) aug_arg_info

  fun_proto
    | libffi
      = ptext (sLit "void") <+> ftext c_nm <>
          parens (ptext (sLit "void *cif STG_UNUSED, void* resp, void** args, void* the_stableptr"))
    | otherwise
      = cResType <+> pprCconv <+> ftext c_nm <> parens fun_args

  -- the target which will form the root of what we ask rts_evalIO to run
  the_cfun
     = case maybe_target of
          Nothing    -> text "(StgClosure*)deRefStablePtr(the_stableptr)"
          Just hs_fn -> char '&' <> ppr hs_fn <> text "_closure"

  cap = text "cap" <> comma

  -- the expression we give to rts_evalIO
  expr_to_run
     = foldl appArg the_cfun arg_info -- NOT aug_arg_info
       where
          appArg acc (arg_cname, _, arg_hty, _)
             = text "rts_apply"
               <> parens (cap <> acc <> comma <> mkHObj arg_hty <> parens (cap <> arg_cname))

  -- various other bits for inside the fn
  declareResult = text "HaskellObj ret;"
  declareCResult | res_hty_is_unit = empty
                 | otherwise       = cResType <+> text "cret;"

  assignCResult | res_hty_is_unit = empty
                | otherwise       =
                        text "cret=" <> unpackHObj res_hty <> parens (text "ret") <> semi

  -- an extern decl for the fn being called
  extern_decl
     = case maybe_target of
          Nothing -> empty
          Just hs_fn -> text "extern StgClosure " <> ppr hs_fn <> text "_closure" <> semi


  -- finally, the whole darn thing
  c_bits =
    space $$
    extern_decl $$
    fun_proto  $$
    vcat
     [ lbrace
     ,   ptext (sLit "Capability *cap;")
     ,   declareResult
     ,   declareCResult
     ,   text "cap = rts_lock();"
          -- create the application + perform it.
     ,   ptext (sLit "rts_evalIO") <> parens (
                char '&' <> cap <>
                ptext (sLit "rts_apply") <> parens (
                    cap <>
                    text "(HaskellObj)"
                 <> ptext (if is_IO_res_ty
                                then (sLit "runIO_closure")
                                else (sLit "runNonIO_closure"))
                 <> comma
                 <> expr_to_run
                ) <+> comma
               <> text "&ret"
             ) <> semi
     ,   ptext (sLit "rts_checkSchedStatus") <> parens (doubleQuotes (ftext c_nm)
                                                <> comma <> text "cap") <> semi
     ,   assignCResult
     ,   ptext (sLit "rts_unlock(cap);")
     ,   ppUnless res_hty_is_unit $
         if libffi
                  then char '*' <> parens (ffi_cResType <> char '*') <>
                       ptext (sLit "resp = cret;")
                  else ptext (sLit "return cret;")
     , rbrace
     ]


foreignExportInitialiser :: Id -> SDoc
foreignExportInitialiser hs_fn =
   -- Initialise foreign exports by registering a stable pointer from an
   -- __attribute__((constructor)) function.
   -- The alternative is to do this from stginit functions generated in
   -- codeGen/CodeGen.lhs; however, stginit functions have a negative impact
   -- on binary sizes and link times because the static linker will think that
   -- all modules that are imported directly or indirectly are actually used by
   -- the program.
   -- (this is bad for big umbrella modules like Graphics.Rendering.OpenGL)
   vcat
    [ text "static void stginit_export_" <> ppr hs_fn
         <> text "() __attribute__((constructor));"
    , text "static void stginit_export_" <> ppr hs_fn <> text "()"
    , braces (text "foreignExportStablePtr"
       <> parens (text "(StgPtr) &" <> ppr hs_fn <> text "_closure")
       <> semi)
    ]


mkHObj :: Type -> SDoc
mkHObj t = text "rts_mk" <> text (showFFIType t)

unpackHObj :: Type -> SDoc
unpackHObj t = text "rts_get" <> text (showFFIType t)

showStgType :: Type -> SDoc
showStgType t = text "Hs" <> text (showFFIType t)

showFFIType :: Type -> String
showFFIType t = getOccString (getName (typeTyCon t))

toCType :: Type -> (Maybe Header, SDoc)
toCType = f False
    where f voidOK t
           -- First, if we have (Ptr t) of (FunPtr t), then we need to
           -- convert t to a C type and put a * after it. If we don't
           -- know a type for t, then "void" is fine, though.
           | Just (ptr, [t']) <- splitTyConApp_maybe t
           , tyConName ptr `elem` [ptrTyConName, funPtrTyConName]
              = case f True t' of
                (mh, cType') ->
                    (mh, cType' <> char '*')
           -- Otherwise, if we have a type constructor application, then
           -- see if there is a C type associated with that constructor.
           -- Note that we aren't looking through type synonyms or
           -- anything, as it may be the synonym that is annotated.
           | TyConApp tycon _ <- t
           , Just (CType mHeader cType) <- tyConCType_maybe tycon
              = (mHeader, ftext cType)
           -- If we don't know a C type for this type, then try looking
           -- through one layer of type synonym etc.
           | Just t' <- coreView t
              = f voidOK t'
           -- Otherwise we don't know the C type. If we are allowing
           -- void then return that; otherwise something has gone wrong.
           | voidOK = (Nothing, ptext (sLit "void"))
           | otherwise
              = pprPanic "toCType" (ppr t)

typeTyCon :: Type -> TyCon
typeTyCon ty
  | UnaryRep rep_ty <- repType ty
  , Just (tc, _) <- tcSplitTyConApp_maybe rep_ty
  = tc
  | otherwise
  = pprPanic "DsForeign.typeTyCon" (ppr ty)

insertRetAddr :: DynFlags -> CCallConv
              -> [(SDoc, SDoc, Type, CmmType)]
              -> [(SDoc, SDoc, Type, CmmType)]
insertRetAddr dflags CCallConv args
    = case platformArch platform of
      ArchX86_64
       | platformOS platform == OSMinGW32 ->
          -- On other Windows x86_64 we insert the return address
          -- after the 4th argument, because this is the point
          -- at which we need to flush a register argument to the stack
          -- (See rts/Adjustor.c for details).
          let go :: Int -> [(SDoc, SDoc, Type, CmmType)]
                        -> [(SDoc, SDoc, Type, CmmType)]
              go 4 args = ret_addr_arg dflags : args
              go n (arg:args) = arg : go (n+1) args
              go _ [] = []
          in go 0 args
       | otherwise ->
          -- On other x86_64 platforms we insert the return address
          -- after the 6th integer argument, because this is the point
          -- at which we need to flush a register argument to the stack
          -- (See rts/Adjustor.c for details).
          let go :: Int -> [(SDoc, SDoc, Type, CmmType)]
                        -> [(SDoc, SDoc, Type, CmmType)]
              go 6 args = ret_addr_arg dflags : args
              go n (arg@(_,_,_,rep):args)
               | cmmEqType_ignoring_ptrhood rep b64 = arg : go (n+1) args
               | otherwise  = arg : go n     args
              go _ [] = []
          in go 0 args
      _ ->
          ret_addr_arg dflags : args
    where platform = targetPlatform dflags
insertRetAddr _ _ args = args

ret_addr_arg :: DynFlags -> (SDoc, SDoc, Type, CmmType)
ret_addr_arg dflags = (text "original_return_addr", text "void*", undefined,
                       typeCmmType dflags addrPrimTy)

-- This function returns the primitive type associated with the boxed
-- type argument to a foreign export (eg. Int ==> Int#).
getPrimTyOf :: Type -> UnaryType
getPrimTyOf ty
  | isBoolTy rep_ty = intPrimTy
  -- Except for Bool, the types we are interested in have a single constructor
  -- with a single primitive-typed argument (see TcType.legalFEArgTyCon).
  | otherwise =
  case splitDataProductType_maybe rep_ty of
     Just (_, _, data_con, [prim_ty]) ->
        ASSERT(dataConSourceArity data_con == 1)
        ASSERT2(isUnLiftedType prim_ty, ppr prim_ty)
        prim_ty
     _other -> pprPanic "DsForeign.getPrimTyOf" (ppr ty)
  where
        UnaryRep rep_ty = repType ty

-- represent a primitive type as a Char, for building a string that
-- described the foreign function type.  The types are size-dependent,
-- e.g. 'W' is a signed 32-bit integer.
primTyDescChar :: DynFlags -> Type -> Char
primTyDescChar dflags ty
 | ty `eqType` unitTy = 'v'
 | otherwise
 = case typePrimRep (getPrimTyOf ty) of
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
\end{code}
