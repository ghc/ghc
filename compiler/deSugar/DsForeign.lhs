%
% (c) The University of Glasgow 2006
% (c) The AQUA Project, Glasgow University, 1998
%

Desugaring foreign declarations (see also DsCCall).

\begin{code}
module DsForeign ( dsForeigns ) where

#include "HsVersions.h"
import TcRnMonad	-- temp

import CoreSyn

import DsCCall
import DsMonad

import HsSyn
import DataCon
import CoreUtils
import CoreUnfold
import Id
import Literal
import Module
import Name
import Type
import TyCon
import Coercion
import TcType
import Var

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
import Config
import Constants

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
type Binding = (Id, CoreExpr)	-- No rec/nonrec structure;
				-- the occurrence analyser will sort it all out

dsForeigns :: [LForeignDecl Id] 
	   -> DsM (ForeignStubs, [Binding])
dsForeigns [] 
  = return (NoStubs, [])
dsForeigns fos = do
    fives <- mapM do_ldecl fos
    let
        (hs, cs, idss, bindss) = unzip4 fives
        fe_ids = concat idss
        fe_init_code = map foreignExportInitialiser fe_ids
    --
    return (ForeignStubs 
             (vcat hs)
             (vcat cs $$ vcat fe_init_code),
           (concat bindss))
  where
   do_ldecl (L loc decl) = putSrcSpanDs loc (do_decl decl)
            
   do_decl (ForeignImport id _ spec) = do
      traceIf (text "fi start" <+> ppr id)
      (bs, h, c) <- dsFImport (unLoc id) spec
      traceIf (text "fi end" <+> ppr id)
      return (h, c, [], bs)

   do_decl (ForeignExport (L _ id) _ (CExport (CExportStatic ext_nm cconv))) = do
      (h, c, _, _) <- dsFExport id (idType id) ext_nm cconv False
      return (h, c, [id], [])
\end{code}


%************************************************************************
%*									*
\subsection{Foreign import}
%*									*
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
	  -> ForeignImport
	  -> DsM ([Binding], SDoc, SDoc)
dsFImport id (CImport cconv safety _ spec) = do
    (ids, h, c) <- dsCImport id spec cconv safety
    return (ids, h, c)

dsCImport :: Id
	  -> CImportSpec
	  -> CCallConv
	  -> Safety
	  -> DsM ([Binding], SDoc, SDoc)
dsCImport id (CLabel cid) cconv _ = do
   let ty = idType id
       fod = case splitTyConApp_maybe (repType ty) of
             Just (tycon, _)
              | tyConUnique tycon == funPtrTyConKey ->
                 IsFunction
             _ -> IsData
   (resTy, foRhs) <- resultWrapper ty
   ASSERT(fromJust resTy `coreEqType` addrPrimTy)    -- typechecker ensures this
    let
        rhs = foRhs (Lit (MachLabel cid stdcall_info fod))
        stdcall_info = fun_type_arg_stdcall_info cconv ty
    in
    return ([(id, rhs)], empty, empty)

dsCImport id (CFunction target) cconv@PrimCallConv safety
  = dsPrimCall id (CCall (CCallSpec target cconv safety))
dsCImport id (CFunction target) cconv safety
  = dsFCall id (CCall (CCallSpec target cconv safety))
dsCImport id CWrapper cconv _
  = dsFExportDynamic id cconv

-- For stdcall labels, if the type was a FunPtr or newtype thereof,
-- then we need to calculate the size of the arguments in order to add
-- the @n suffix to the label.
fun_type_arg_stdcall_info :: CCallConv -> Type -> Maybe Int
fun_type_arg_stdcall_info StdCallConv ty
  | Just (tc,[arg_ty]) <- splitTyConApp_maybe (repType ty),
    tyConUnique tc == funPtrTyConKey
  = let
       (_tvs,sans_foralls)        = tcSplitForAllTys arg_ty
       (fe_arg_tys, _orig_res_ty) = tcSplitFunTys sans_foralls
    in Just $ sum (map (widthInBytes . typeWidth . typeCmmType . getPrimTyOf) fe_arg_tys)
fun_type_arg_stdcall_info _other_conv _
  = Nothing
\end{code}


%************************************************************************
%*									*
\subsection{Foreign calls}
%*									*
%************************************************************************

\begin{code}
dsFCall :: Id -> ForeignCall -> DsM ([(Id, Expr TyVar)], SDoc, SDoc)
dsFCall fn_id fcall = do
    let
        ty                   = idType fn_id
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
    let
        -- Build the worker
        worker_ty     = mkForAllTys tvs (mkFunTys (map idType work_arg_ids) ccall_result_ty)
        the_ccall_app = mkFCall ccall_uniq fcall val_args ccall_result_ty
        work_rhs      = mkLams tvs (mkLams work_arg_ids the_ccall_app)
        work_id       = mkSysLocal (fsLit "$wccall") work_uniq worker_ty

        -- Build the wrapper
        work_app     = mkApps (mkVarApps (Var work_id) tvs) val_args
        wrapper_body = foldr ($) (res_wrapper work_app) arg_wrappers
        wrap_rhs     = mkLams (tvs ++ args) wrapper_body
        fn_id_w_inl  = fn_id `setIdUnfolding` mkInlineUnfolding (Just (length args)) wrap_rhs
    
    return ([(work_id, work_rhs), (fn_id_w_inl, wrap_rhs)], empty, empty)
\end{code}


%************************************************************************
%*									*
\subsection{Primitive calls}
%*									*
%************************************************************************

This is for `@foreign import prim@' declarations.

Currently, at the core level we pretend that these primitive calls are
foreign calls. It may make more sense in future to have them as a distinct
kind of Id, or perhaps to bundle them with PrimOps since semantically and
for calling convention they are really prim ops.

\begin{code}
dsPrimCall :: Id -> ForeignCall -> DsM ([(Id, Expr TyVar)], SDoc, SDoc)
dsPrimCall fn_id fcall = do
    let
        ty                   = idType fn_id
        (tvs, fun_ty)        = tcSplitForAllTys ty
        (arg_tys, io_res_ty) = tcSplitFunTys fun_ty
                -- Must use tcSplit* functions because we want to
                -- see that (IO t) in the corner

    args <- newSysLocalsDs arg_tys

    ccall_uniq <- newUnique
    let
        call_app = mkFCall ccall_uniq fcall (map Var args) io_res_ty
        rhs      = mkLams tvs (mkLams args call_app)
    return ([(fn_id, rhs)], empty, empty)

\end{code}

%************************************************************************
%*									*
\subsection{Foreign export}
%*									*
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
dsFExport :: Id			-- Either the exported Id, 
				-- or the foreign-export-dynamic constructor
	  -> Type		-- The type of the thing callable from C
	  -> CLabelString	-- The name to export to C land
	  -> CCallConv
	  -> Bool		-- True => foreign export dynamic
				-- 	   so invoke IO action that's hanging off 
				-- 	   the first argument's stable pointer
	  -> DsM ( SDoc		-- contents of Module_stub.h
		 , SDoc		-- contents of Module_stub.c
		 , String       -- string describing type to pass to createAdj.
		 , Int		-- size of args to stub function
		 )

dsFExport fn_id ty ext_name cconv isDyn=  do
    let
       (_tvs,sans_foralls)             = tcSplitForAllTys ty
       (fe_arg_tys', orig_res_ty)      = tcSplitFunTys sans_foralls
       -- We must use tcSplits here, because we want to see 
       -- the (IO t) in the corner of the type!
       fe_arg_tys | isDyn     = tail fe_arg_tys'
                  | otherwise = fe_arg_tys'
    
       -- Look at the result type of the exported function, orig_res_ty
       -- If it's IO t, return         (t, True)
       -- If it's plain t, return      (t, False)
    (res_ty,             -- t
     is_IO_res_ty) <-    -- Bool
        case tcSplitIOType_maybe orig_res_ty of
           Just (_ioTyCon, res_ty, _co) -> return (res_ty, True)
                   -- The function already returns IO t
                   -- ToDo: what about the coercion?
           Nothing                    -> return (orig_res_ty, False) 
                   -- The function returns t
    
    return $
      mkFExportCBits ext_name 
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
		 -> CCallConv
		 -> DsM ([Binding], SDoc, SDoc)
dsFExportDynamic id cconv = do
    fe_id <-  newSysLocalDs ty
    mod <- getModuleDs
    let
        -- hack: need to get at the name of the C stub we're about to generate.
        fe_nm    = mkFastString (unpackFS (zEncodeFS (moduleNameFS (moduleName mod))) ++ "_" ++ toCName fe_id)

    cback <- newSysLocalDs arg_ty
    newStablePtrId <- dsLookupGlobalId newStablePtrName
    stable_ptr_tycon <- dsLookupTyCon stablePtrTyConName
    let
        stable_ptr_ty = mkTyConApp stable_ptr_tycon [arg_ty]
        export_ty     = mkFunTy stable_ptr_ty arg_ty
    bindIOId <- dsLookupGlobalId bindIOName
    stbl_value <- newSysLocalDs stable_ptr_ty
    (h_code, c_code, typestring, args_size) <- dsFExport id export_ty fe_nm cconv True
    let
         {-
          The arguments to the external function which will
          create a little bit of (template) code on the fly
          for allowing the (stable pointed) Haskell closure
          to be entered using an external calling convention
          (stdcall, ccall).
         -}
        adj_args      = [ mkIntLitInt (ccallConvToInt cconv)
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

    let io_app = mkLams tvs                $
                 Lam cback                 $
                 mkCoerceI (mkSymCoI co)   $
                 mkApps (Var bindIOId)
                        [ Type stable_ptr_ty
                        , Type res_ty       
                        , mkApps (Var newStablePtrId) [ Type arg_ty, Var cback ]
                        , Lam stbl_value ccall_adj
                        ]

        fed = (id `setInlineActivation` NeverActive, io_app)
               -- Never inline the f.e.d. function, because the litlit
               -- might not be in scope in other modules.

    return ([fed], h_code, c_code)

 where
  ty                       = idType id
  (tvs,sans_foralls)       = tcSplitForAllTys ty
  ([arg_ty], fn_res_ty)    = tcSplitFunTys sans_foralls
  Just (io_tc, res_ty, co) = tcSplitIOType_maybe fn_res_ty
	-- Must have an IO type; hence Just
	-- co : fn_res_ty ~ IO res_ty

toCName :: Id -> String
toCName i = showSDoc (pprCode CStyle (ppr (idName i)))
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
mkFExportCBits :: FastString
	       -> Maybe Id 	-- Just==static, Nothing==dynamic
	       -> [Type] 
	       -> Type 
               -> Bool		-- True <=> returns an IO type
	       -> CCallConv 
	       -> (SDoc, 
		   SDoc,
		   String, 	-- the argument reps
		   Int		-- total size of arguments
		  )
mkFExportCBits c_nm maybe_target arg_htys res_hty is_IO_res_ty cc 
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
  arg_info :: [(SDoc, 		-- arg name
		SDoc,		-- C type
	        Type,		-- Haskell type
		CmmType)]	-- the CmmType
  arg_info  = [ let stg_type = showStgType ty in
                (arg_cname n stg_type,
                 stg_type,
                 ty, 
		 typeCmmType (getPrimTyOf ty))
	      | (ty,n) <- zip arg_htys [1::Int ..] ]

  arg_cname n stg_ty
        | libffi    = char '*' <> parens (stg_ty <> char '*') <> 
                      ptext (sLit "args") <> brackets (int (n-1))
        | otherwise = text ('a':show n)

  -- generate a libffi-style stub if this is a "wrapper" and libffi is enabled
  libffi = cLibFFI && isNothing maybe_target

  type_string
      -- libffi needs to know the result type too:
      | libffi    = primTyDescChar res_hty : arg_type_string
      | otherwise = arg_type_string

  arg_type_string = [primTyDescChar ty | (_,_,ty,_) <- arg_info]
                -- just the real args

  -- add some auxiliary args; the stable ptr in the wrapper case, and
  -- a slot for the dummy return address in the wrapper + ccall case
  aug_arg_info
    | isNothing maybe_target = stable_ptr_arg : insertRetAddr cc arg_info
    | otherwise              = arg_info

  stable_ptr_arg = 
	(text "the_stableptr", text "StgStablePtr", undefined,
	 typeCmmType (mkStablePtrPrimTy alphaTy))

  -- stuff to do with the return type of the C function
  res_hty_is_unit = res_hty `coreEqType` unitTy	-- Look through any newtypes

  cResType | res_hty_is_unit = text "void"
	   | otherwise	     = showStgType res_hty

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
  pprCconv = case cc of
		CCallConv   -> empty
		StdCallConv -> text (ccallConvAttribute cc)
                _           -> panic ("mkFExportCBits/pprCconv " ++ showPpr cc)

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
     ,   ptext (sLit "cap=rts_evalIO") <> parens (
		cap <>
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
    , braces (text "getStablePtr"
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

typeTyCon :: Type -> TyCon
typeTyCon ty = case tcSplitTyConApp_maybe (repType ty) of
                 Just (tc,_) -> tc
	         Nothing     -> pprPanic "DsForeign.typeTyCon" (ppr ty)

insertRetAddr :: CCallConv -> [(SDoc, SDoc, Type, CmmType)]
                           -> [(SDoc, SDoc, Type, CmmType)]
#if !defined(x86_64_TARGET_ARCH)
insertRetAddr CCallConv args = ret_addr_arg : args
insertRetAddr _ args = args
#else
-- On x86_64 we insert the return address after the 6th
-- integer argument, because this is the point at which we
-- need to flush a register argument to the stack (See rts/Adjustor.c for
-- details).
insertRetAddr CCallConv args = go 0 args
  where  go :: Int -> [(SDoc, SDoc, Type, CmmType)]
                   -> [(SDoc, SDoc, Type, CmmType)]
         go 6 args = ret_addr_arg : args
	 go n (arg@(_,_,_,rep):args)
	  | cmmEqType_ignoring_ptrhood rep b64 = arg : go (n+1) args
	  | otherwise  = arg : go n     args
	 go _ [] = []
insertRetAddr _ args = args
#endif

ret_addr_arg :: (SDoc, SDoc, Type, CmmType)
ret_addr_arg = (text "original_return_addr", text "void*", undefined, 
		typeCmmType addrPrimTy)

-- This function returns the primitive type associated with the boxed
-- type argument to a foreign export (eg. Int ==> Int#).
getPrimTyOf :: Type -> Type
getPrimTyOf ty
  | isBoolTy rep_ty = intPrimTy
  -- Except for Bool, the types we are interested in have a single constructor
  -- with a single primitive-typed argument (see TcType.legalFEArgTyCon).
  | otherwise =
  case splitProductType_maybe rep_ty of
     Just (_, _, data_con, [prim_ty]) ->
	ASSERT(dataConSourceArity data_con == 1)
	ASSERT2(isUnLiftedType prim_ty, ppr prim_ty)
	prim_ty
     _other -> pprPanic "DsForeign.getPrimTyOf" (ppr ty)
  where
	rep_ty = repType ty

-- represent a primitive type as a Char, for building a string that
-- described the foreign function type.  The types are size-dependent,
-- e.g. 'W' is a signed 32-bit integer.
primTyDescChar :: Type -> Char
primTyDescChar ty
 | ty `coreEqType` unitTy = 'v'
 | otherwise
 = case typePrimRep (getPrimTyOf ty) of
     IntRep	 -> signed_word
     WordRep     -> unsigned_word
     Int64Rep    -> 'L'
     Word64Rep   -> 'l'
     AddrRep     -> 'p'
     FloatRep    -> 'f'
     DoubleRep   -> 'd'
     _           -> pprPanic "primTyDescChar" (ppr ty)
  where
    (signed_word, unsigned_word)
       | wORD_SIZE == 4  = ('W','w')
       | wORD_SIZE == 8  = ('L','l')
       | otherwise       = panic "primTyDescChar"
\end{code}
