{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- | Handling of C foreign imports/exports
module GHC.HsToCore.Foreign.C
  ( dsCImport
  , dsCFExport
  , dsCFExportDynamic
  )
where

import GHC.Prelude

import GHC.Platform

import GHC.Tc.Utils.Monad        -- temp
import GHC.Tc.Utils.Env
import GHC.Tc.Utils.TcType

import GHC.Core
import GHC.Core.Unfold.Make
import GHC.Core.Type
import GHC.Core.TyCon
import GHC.Core.Coercion
import GHC.Core.Multiplicity

import GHC.HsToCore.Foreign.Call
import GHC.HsToCore.Foreign.Prim
import GHC.HsToCore.Foreign.Utils
import GHC.HsToCore.Monad
import GHC.HsToCore.Types (ds_next_wrapper_num)

import GHC.Hs

import GHC.Types.Id
import GHC.Types.Literal
import GHC.Types.ForeignStubs
import GHC.Types.SourceText
import GHC.Types.Name
import GHC.Types.RepType
import GHC.Types.ForeignCall
import GHC.Types.Basic

import GHC.Unit.Module

import GHC.Driver.DynFlags
import GHC.Driver.Config

import GHC.Cmm.Expr
import GHC.Cmm.Utils

import GHC.Builtin.Types
import GHC.Builtin.Types.Prim
import GHC.Builtin.Names

import GHC.Data.FastString

import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Panic.Plain
import GHC.Utils.Encoding

import Data.Maybe
import Data.List (nub)

dsCFExport:: Id                 -- Either the exported Id,
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
                 , Int          -- size of args to stub function
                 )

dsCFExport fn_id co ext_name cconv isDyn = do
    let
       ty                     = coercionRKind co
       (bndrs, orig_res_ty)   = tcSplitPiTys ty
       fe_arg_tys'            = mapMaybe anonPiTyBinderType_maybe bndrs
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

dsCImport :: Id
          -> Coercion
          -> CImportSpec
          -> CCallConv
          -> Safety
          -> Maybe Header
          -> DsM ([Binding], CHeader, CStub)
dsCImport id co (CLabel cid) cconv _ _ = do
   dflags <- getDynFlags
   let ty  = coercionLKind co
       platform = targetPlatform dflags
       fod = case tyConAppTyCon_maybe (dropForAlls ty) of
             Just tycon
              | tyConUnique tycon == funPtrTyConKey ->
                 IsFunction
             _ -> IsData
   (resTy, foRhs) <- resultWrapper ty
   assert (fromJust resTy `eqType` addrPrimTy) $    -- typechecker ensures this
    let
        rhs = foRhs (Lit (LitLabel cid stdcall_info fod))
        rhs' = Cast rhs co
        stdcall_info = fun_type_arg_stdcall_info platform cconv ty
    in
    return ([(id, rhs')], mempty, mempty)

dsCImport id co (CFunction target) cconv@PrimCallConv safety _
  = dsPrimCall id co (CCall (CCallSpec target cconv safety))
dsCImport id co (CFunction target) cconv safety mHeader
  = dsFCall id co (CCall (CCallSpec target cconv safety)) mHeader
dsCImport id co CWrapper cconv _ _
  = dsCFExportDynamic id co cconv


{-
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

-- and the helper in C: (approximately; see `mkFExportCBits` below)

f_helper(StablePtr s, HsBool b, HsInt i)
{
        Capability *cap;
        cap = rts_lock();
        rts_inCall(&cap,
                   rts_apply(rts_apply(deRefStablePtr(s),
                                       rts_mkBool(b)), rts_mkInt(i)));
        rts_unlock(cap);
}
\end{verbatim}
-}
dsCFExportDynamic :: Id
                 -> Coercion
                 -> CCallConv
                 -> DsM ([Binding], CHeader, CStub)
dsCFExportDynamic id co0 cconv = do
    mod <- getModule
    dflags <- getDynFlags
    let platform = targetPlatform dflags
    let fe_nm = mkFastString $ zEncodeString
            (moduleStableString mod ++ "$" ++ toCName id)
        -- Construct the label based on the passed id, don't use names
        -- depending on Unique. See #13807 and Note [Unique Determinism].
    cback <- newSysLocalDs arg_mult arg_ty
    newStablePtrId <- dsLookupGlobalId newStablePtrName
    stable_ptr_tycon <- dsLookupTyCon stablePtrTyConName
    let
        stable_ptr_ty = mkTyConApp stable_ptr_tycon [arg_ty]
        export_ty     = mkVisFunTyMany stable_ptr_ty arg_ty
    bindIOId <- dsLookupGlobalId bindIOName
    stbl_value <- newSysLocalDs ManyTy stable_ptr_ty
    (h_code, c_code, typestring, args_size) <- dsCFExport id (mkRepReflCo export_ty) fe_nm cconv True
    let
         {-
          The arguments to the external function which will
          create a little bit of (template) code on the fly
          for allowing the (stable pointed) Haskell closure
          to be entered using an external calling convention
          (stdcall, ccall).
         -}
        adj_args      = [ mkIntLit platform (fromIntegral (ccallConvToInt cconv))
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
  ty                       = coercionLKind co0
  (tvs,sans_foralls)       = tcSplitForAllInvisTyVars ty
  ([Scaled arg_mult arg_ty], fn_res_ty)    = tcSplitFunTys sans_foralls
  Just (io_tc, res_ty)     = tcSplitIOType_maybe fn_res_ty
        -- Must have an IO type; hence Just


-- | Foreign calls
dsFCall :: Id -> Coercion -> ForeignCall -> Maybe Header
        -> DsM ([(Id, Expr TyVar)], CHeader, CStub)
dsFCall fn_id co fcall mDeclHeader = do
    let
        (ty,ty1)             = (coercionLKind co, coercionRKind co)
        (tv_bndrs, rho)      = tcSplitForAllTyVarBinders ty
        (arg_tys, io_res_ty) = tcSplitFunTys rho

    let constQual -- provide 'const' qualifier (#22043)
          | (_, res_ty1) <- tcSplitFunTys ty1
          , newty <- maybe res_ty1 snd (tcSplitIOType_maybe res_ty1)
          , Just (ptr, _) <- splitTyConApp_maybe newty
          , tyConName ptr == constPtrConName
          = text "const"
          | otherwise = empty

    args <- newSysLocalsDs arg_tys  -- no FFI representation polymorphism
    (val_args, arg_wrappers) <- mapAndUnzipM unboxArg (map Var args)

    let
        work_arg_ids  = [v | Var v <- val_args] -- All guaranteed to be vars

    (ccall_result_ty, res_wrapper) <- boxResult io_res_ty

    ccall_uniq <- newUnique
    work_uniq  <- newUnique

    (fcall', cDoc) <-
              case fcall of
              CCall (CCallSpec (StaticTarget _ cName mUnitId isFun)
                               CApiConv safety) ->
               do nextWrapperNum <- ds_next_wrapper_num <$> getGblEnv
                  wrapperName <- mkWrapperName nextWrapperNum "ghc_wrapper" (unpackFS cName)
                  let fcall' = CCall (CCallSpec
                                      (StaticTarget NoSourceText
                                                    wrapperName mUnitId
                                                    True)
                                      CApiConv safety)
                      c = includes
                       $$ fun_proto <+> braces (cRet <> semi)
                      includes = vcat [ text "#include \"" <> ftext h
                                        <> text "\""
                                      | Header _ h <- nub headers ]
                      fun_proto = constQual <+> cResType <+> pprCconv <+> ppr wrapperName <> parens argTypes
                      cRet
                       | isVoidRes =                   cCall
                       | otherwise = text "return" <+> cCall
                      cCall
                        | isFun = ppr cName <> parens argVals
                        | null arg_tys = ppr cName
                        | otherwise = panic "dsFCall: Unexpected arguments to FFI value import"
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
                            , let (header, cType) = toCType (scaledThing t) ]
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
    dflags <- getDynFlags
    let
        -- Build the worker
        worker_ty     = mkForAllTys tv_bndrs (mkVisFunTysMany (map idType work_arg_ids) ccall_result_ty)
        tvs           = map binderVar tv_bndrs
        the_ccall_app = mkFCall ccall_uniq fcall' val_args ccall_result_ty
        work_rhs      = mkLams tvs (mkLams work_arg_ids the_ccall_app)
        work_id       = mkSysLocal (fsLit "$wccall") work_uniq ManyTy worker_ty

        -- Build the wrapper
        work_app     = mkApps (mkVarApps (Var work_id) tvs) val_args
        wrapper_body = foldr ($) (res_wrapper work_app) arg_wrappers
        wrap_rhs     = mkLams (tvs ++ args) wrapper_body
        wrap_rhs'    = Cast wrap_rhs co
        simpl_opts   = initSimpleOpts dflags
        fn_id_w_inl  = fn_id `setIdUnfolding` mkInlineUnfoldingWithArity simpl_opts
                                                StableSystemSrc (length args)
                                                wrap_rhs'

    return ([(work_id, work_rhs), (fn_id_w_inl, wrap_rhs')], mempty, CStub cDoc [] [])


toCName :: Id -> String
toCName i = showSDocOneLine defaultSDocContext (pprCode (ppr (idName i)))

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
           | Just tycon <- tyConAppTyConPicky_maybe t
           , Just (CType _ mHeader (_,cType)) <- tyConCType_maybe tycon
              = (mHeader, ftext cType)
           -- If we don't know a C type for this type, then try looking
           -- through one layer of type synonym etc.
           | Just t' <- coreView t
              = f voidOK t'
          -- Handle 'UnliftedFFITypes' argument
           | Just tyCon <- tyConAppTyConPicky_maybe t
           , isPrimTyCon tyCon
           , Just cType <- ppPrimTyConStgType tyCon
           = (Nothing, text cType)

           -- Otherwise we don't know the C type. If we are allowing
           -- void then return that; otherwise something has gone wrong.
           | voidOK = (Nothing, text "void")
           | otherwise
              = pprPanic "toCType" (ppr t)

{-
*

\subsection{Generating @foreign export@ stubs}

*

For each @foreign export@ function, a C stub function is generated.
The C stub constructs the application of the exported Haskell function
using the hugs/ghc rts invocation API.
-}

mkFExportCBits :: DynFlags
               -> FastString
               -> Maybe Id      -- Just==static, Nothing==dynamic
               -> [Type]
               -> Type
               -> Bool          -- True <=> returns an IO type
               -> CCallConv
               -> (CHeader,
                   CStub,
                   String,      -- the argument reps
                   Int          -- total size of arguments
                  )
mkFExportCBits dflags c_nm maybe_target arg_htys res_hty is_IO_res_ty cc
 =
   ( header_bits
   , CStub body [] []
   , type_string,
    aug_arg_size
    )
 where
  platform = targetPlatform dflags

  -- list the arguments to the C function
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

  arg_cname n stg_ty
        | libffi    = char '*' <> parens (stg_ty <> char '*') <>
                      text "args" <> brackets (int (n-1))
        | otherwise = char 'a' <> int n

  -- generate a libffi-style stub if this is a "wrapper" and libffi is enabled
  libffi = platformMisc_libFFI (platformMisc dflags) && isNothing maybe_target

  type_string
      -- libffi needs to know the result type too:
      | libffi    = primTyDescChar platform res_hty : arg_type_string
      | otherwise = arg_type_string

  arg_type_string = [primTyDescChar platform ty | (_,_,ty,_) <- arg_info]
                -- just the real args

  -- add some auxiliary args; the stable ptr in the wrapper case, and
  -- a slot for the dummy return address in the wrapper + ccall case
  aug_arg_info
    | isNothing maybe_target = stable_ptr_arg : insertRetAddr platform cc arg_info
    | otherwise              = arg_info

  aug_arg_size = sum [ widthInBytes (typeWidth rep) | (_,_,_,rep) <- aug_arg_info]
         -- NB. the calculation here isn't strictly speaking correct.
         -- We have a primitive Haskell type (eg. Int#, Double#), and
         -- we want to know the size, when passed on the C stack, of
         -- the associated C type (eg. HsInt, HsDouble).  We don't have
         -- this information to hand, but we know what GHC's conventions
         -- are for passing around the primitive Haskell types, so we
         -- use that instead.  I hope the two coincide --SDM
         -- AK: This seems just wrong, the code here uses widthInBytes, but when
         -- we pass args on the haskell stack we always extend to multiples of 8
         -- to my knowledge. Not sure if it matters though so I won't touch this
         -- for now.

  stable_ptr_arg =
        (text "the_stableptr", text "StgStablePtr", undefined,
         typeCmmType platform (mkStablePtrPrimTy alphaTy))

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

  header_bits = CHeader (text "extern" <+> fun_proto <> semi)

  fun_args
    | null aug_arg_info = text "void"
    | otherwise         = hsep $ punctuate comma
                               $ map (\(nm,ty,_,_) -> ty <+> nm) aug_arg_info

  fun_proto
    | libffi
      = text "void" <+> ftext c_nm <>
          parens (text "void *cif STG_UNUSED, void* resp, void** args, void* the_stableptr")
    | otherwise
      = cResType <+> pprCconv <+> ftext c_nm <> parens fun_args

  -- the target which will form the root of what we ask rts_inCall to run
  the_cfun
     = case maybe_target of
          Nothing    -> text "(StgClosure*)deRefStablePtr(the_stableptr)"
          Just hs_fn -> char '&' <> ppr hs_fn <> text "_closure"

  cap = text "cap" <> comma

  -- the expression we give to rts_inCall
  expr_to_run
     = foldl' appArg the_cfun arg_info -- NOT aug_arg_info
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
  body =
    space $$
    extern_decl $$
    fun_proto  $$
    vcat
     [ lbrace
     ,   text "Capability *cap;"
     ,   declareResult
     ,   declareCResult
     ,   text "cap = rts_lock();"
          -- create the application + perform it.
     ,   text "rts_inCall" <> parens (
                char '&' <> cap <>
                text "rts_apply" <> parens (
                    cap <>
                    text "(HaskellObj)"
                 <> (if is_IO_res_ty
                      then text "runIO_closure"
                      else text "runNonIO_closure")
                 <> comma
                 <> expr_to_run
                ) <+> comma
               <> text "&ret"
             ) <> semi
     ,   text "rts_checkSchedStatus" <> parens (doubleQuotes (ftext c_nm)
                                                <> comma <> text "cap") <> semi
     ,   assignCResult
     ,   text "rts_unlock(cap);"
     ,   ppUnless res_hty_is_unit $
         if libffi
                  then char '*' <> parens (ffi_cResType <> char '*') <>
                       text "resp = " <> parens ffi_cResType <> text "cret;"
                  else text "return cret;"
     , rbrace
     ]

mkHObj :: Type -> SDoc
mkHObj t = text "rts_mk" <> showFFIType t

unpackHObj :: Type -> SDoc
unpackHObj t = text "rts_get" <> showFFIType t

showStgType :: Type -> SDoc
showStgType t = text "Hs" <> showFFIType t

showFFIType :: Type -> SDoc
showFFIType t = ftext (occNameFS (getOccName (typeTyCon t)))

typeTyCon :: Type -> TyCon
typeTyCon ty
  | Just (tc, _) <- tcSplitTyConApp_maybe (unwrapType ty)
  = tc
  | otherwise
  = pprPanic "GHC.HsToCore.Foreign.C.typeTyCon" (ppr ty)


insertRetAddr :: Platform -> CCallConv
              -> [(SDoc, SDoc, Type, CmmType)]
              -> [(SDoc, SDoc, Type, CmmType)]
insertRetAddr platform CCallConv args
    = case platformArch platform of
      ArchX86_64
       | platformOS platform == OSMinGW32 ->
          -- On other Windows x86_64 we insert the return address
          -- after the 4th argument, because this is the point
          -- at which we need to flush a register argument to the stack
          -- (See rts/Adjustor.c for details).
          let go :: Int -> [(SDoc, SDoc, Type, CmmType)]
                        -> [(SDoc, SDoc, Type, CmmType)]
              go 4 args = ret_addr_arg platform : args
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
              go 6 args = ret_addr_arg platform : args
              go n (arg@(_,_,_,rep):args)
                -- Int type fitting into int register
                | (isBitsType rep && typeWidth rep <= W64 || isGcPtrType rep)
                = arg : go (n+1) args
                | otherwise
                = arg : go n args
              go _ [] = []
          in go 0 args
      _ ->
          ret_addr_arg platform : args
insertRetAddr _ _ args = args

ret_addr_arg :: Platform -> (SDoc, SDoc, Type, CmmType)
ret_addr_arg platform = (text "original_return_addr", text "void*", undefined,
                         typeCmmType platform addrPrimTy)

-- For stdcall labels, if the type was a FunPtr or newtype thereof,
-- then we need to calculate the size of the arguments in order to add
-- the @n suffix to the label.
fun_type_arg_stdcall_info :: Platform -> CCallConv -> Type -> Maybe Int
fun_type_arg_stdcall_info platform StdCallConv ty
  | Just (tc,[arg_ty]) <- splitTyConApp_maybe ty,
    tyConUnique tc == funPtrTyConKey
  = let
       (bndrs, _) = tcSplitPiTys arg_ty
       fe_arg_tys = mapMaybe anonPiTyBinderType_maybe bndrs
    in Just $ sum (map (widthInBytes . typeWidth . typeCmmType platform . getPrimTyOf) fe_arg_tys)
fun_type_arg_stdcall_info _ _other_conv _
  = Nothing
