%
% (c) The AQUA Project, Glasgow University, 1998
%
\section[DsCCall]{Desugaring \tr{foreign} declarations}

Expanding out @foreign import@ and @foreign export@ declarations.

\begin{code}
module DsForeign ( dsForeigns ) where

#include "HsVersions.h"

import CoreSyn

import DsCCall		( dsCCall, mkFCall, boxResult, unboxArg, resultWrapper )
import DsMonad

import HsSyn		( ForeignDecl(..), ForeignExport(..),
			  ForeignImport(..), CImportSpec(..) )
import TcHsSyn		( TypecheckedForeignDecl )
import CoreUtils	( exprType, mkInlineMe )
import Id		( Id, idType, idName, mkVanillaGlobal, mkSysLocal,
			  setInlinePragma )
import IdInfo		( vanillaIdInfo )
import Literal		( Literal(..) )
import Module		( Module, moduleUserString )
import Name		( mkGlobalName, nameModule, nameOccName, getOccString, 
			  mkForeignExportOcc, isLocalName,
			  NamedThing(..),
			)
import Type		( repType, eqType )
import TcType		( Type, mkFunTys, mkForAllTys, mkTyConApp,
			  mkFunTy, applyTy, 
			  tcSplitForAllTys, tcSplitFunTys, tcTyConAppArgs,
			  tcSplitTyConApp_maybe, tcSplitAppTy,
			  tcFunResultTy
			)

import ForeignCall	( ForeignCall(..), CCallSpec(..), 
			  Safety(..), playSafe,
			  CExportSpec(..),
			  CCallConv(..), ccallConvToInt,
			  ccallConvAttribute
			)
import CStrings		( CLabelString )
import TysWiredIn	( addrTy, unitTy, stablePtrTyCon )
import TysPrim		( addrPrimTy )
import PrelNames	( hasKey, ioTyConKey, deRefStablePtrName, newStablePtrName,
			  bindIOName, returnIOName
			)
import BasicTypes	( Activation( NeverActive ) )
import ErrUtils         ( addShortWarnLocLine )
import Outputable
import Maybe 		( fromJust )
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

dsForeigns :: Module
           -> [TypecheckedForeignDecl] 
	   -> DsM ( [Id]		-- Foreign-exported binders; 
					-- we have to generate code to register these
		  , [Binding]
		  , SDoc	      -- Header file prototypes for
                                      -- "foreign exported" functions.
		  , SDoc 	      -- C stubs to use when calling
                                      -- "foreign exported" functions.
		  , [FAST_STRING]     -- headers that need to be included
				      -- into C code generated for this module
		  )
dsForeigns mod_name fos
  = foldlDs combine ([], [], empty, empty, []) fos
 where
  combine (acc_feb, acc_f, acc_h, acc_c, acc_header) 
	  (ForeignImport id _ spec depr loc)
    = dsFImport mod_name id spec	           `thenDs` \(bs, h, c, hd) -> 
      warnDepr depr loc				   `thenDs` \_              ->
      returnDs (acc_feb, bs ++ acc_f, h $$ acc_h, c $$ acc_c, hd ++ acc_header)

  combine (acc_feb, acc_f, acc_h, acc_c, acc_header) 
	  (ForeignExport id _ (CExport (CExportStatic ext_nm cconv)) depr loc)
    = dsFExport mod_name id (idType id) 
		ext_nm cconv False                 `thenDs` \(h, c) ->
      warnDepr depr loc				   `thenDs` \_              ->
      returnDs (acc_feb, acc_f, h $$ acc_h, c $$ acc_c, acc_header)

  warnDepr False _   = returnDs ()
  warnDepr True  loc = dsWarn (addShortWarnLocLine loc msg)
   where
    msg = ptext SLIT("foreign declaration uses deprecated non-standard syntax")
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
dsFImport :: Module
	  -> Id
	  -> ForeignImport
	  -> DsM ([Binding], SDoc, SDoc, [FAST_STRING])
dsFImport modName id (CImport cconv safety header lib spec)
  = dsCImport modName id spec cconv safety	  `thenDs` \(ids, h, c) ->
    returnDs (ids, h, c, if _NULL_ header then [] else [header])
  -- FIXME: the `lib' field is needed for .NET ILX generation when invoking
  --	    routines that are external to the .NET runtime, but GHC doesn't
  --	    support such calls yet; if `_NULL_ lib', the value was not given
dsFImport modName id (DNImport spec)
  = dsFCall modName id (DNCall spec)	          `thenDs` \(ids, h, c) ->
    returnDs (ids, h, c, [])

dsCImport :: Module
	  -> Id
	  -> CImportSpec
	  -> CCallConv
	  -> Safety
	  -> DsM ([Binding], SDoc, SDoc)
dsCImport modName id (CLabel cid)       _     _
 = ASSERT(fromJust resTy `eqType` addrPrimTy)    -- typechecker ensures this
   returnDs ([(id, rhs)], empty, empty)
 where
   (resTy, foRhs) = resultWrapper (idType id)
   rhs		  = foRhs (mkLit (MachLabel cid))
dsCImport modName id (CFunction target) cconv safety
  = dsFCall modName id (CCall (CCallSpec target cconv safety))
dsCImport modName id CWrapper           cconv _
  = dsFExportDynamic modName id cconv
\end{code}


%************************************************************************
%*									*
\subsection{Foreign calls}
%*									*
%************************************************************************

\begin{code}
dsFCall mod_Name fn_id fcall
  = let
	ty		     = idType fn_id
	(tvs, fun_ty)        = tcSplitForAllTys ty
	(arg_tys, io_res_ty) = tcSplitFunTys fun_ty
		-- Must use tcSplit* functions because we want to 
		-- see that (IO t) in the corner
    in
    newSysLocalsDs arg_tys  			`thenDs` \ args ->
    mapAndUnzipDs unboxArg (map Var args)	`thenDs` \ (val_args, arg_wrappers) ->

    let
	work_arg_ids  = [v | Var v <- val_args]	-- All guaranteed to be vars

	-- These are the ids we pass to boxResult, which are used to decide
	-- whether to touch# an argument after the call (used to keep
	-- ForeignObj#s live across a 'safe' foreign import).
	maybe_arg_ids | unsafe_call fcall = work_arg_ids
		      | otherwise	  = []
    in
    boxResult maybe_arg_ids io_res_ty  		`thenDs` \ (ccall_result_ty, res_wrapper) ->

    getUniqueDs					`thenDs` \ ccall_uniq ->
    getUniqueDs					`thenDs` \ work_uniq ->
    let
	-- Build the worker
	worker_ty     = mkForAllTys tvs (mkFunTys (map idType work_arg_ids) ccall_result_ty)
 	the_ccall_app = mkFCall ccall_uniq fcall val_args ccall_result_ty
	work_rhs      = mkLams tvs (mkLams work_arg_ids the_ccall_app)
	work_id       = mkSysLocal SLIT("$wccall") work_uniq worker_ty

	-- Build the wrapper
	work_app     = mkApps (mkVarApps (Var work_id) tvs) val_args
	wrapper_body = foldr ($) (res_wrapper work_app) arg_wrappers
        wrap_rhs     = mkInlineMe (mkLams (tvs ++ args) wrapper_body)
    in
    returnDs ([(work_id, work_rhs), (fn_id, wrap_rhs)], empty, empty)

unsafe_call (CCall (CCallSpec _ _ safety)) = playSafe safety
unsafe_call (DNCall _)			   = False
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
\item a Haskell stub `@M.$ffoo@', which calls
\end{itemize}
the user-written Haskell function `@M.foo@'.

\begin{code}
dsFExport :: Module
	  -> Id			-- Either the exported Id, 
				-- or the foreign-export-dynamic constructor
	  -> Type		-- The type of the thing callable from C
	  -> CLabelString	-- The name to export to C land
	  -> CCallConv
	  -> Bool		-- True => foreign export dynamic
				-- 	   so invoke IO action that's hanging off 
				-- 	   the first argument's stable pointer
	  -> DsM ( SDoc		-- contents of Module_stub.h
		 , SDoc		-- contents of Module_stub.c
		 )

dsFExport mod_name fn_id ty ext_name cconv isDyn
   = 
     let
        (tvs,sans_foralls)		= tcSplitForAllTys ty
        (fe_arg_tys', orig_res_ty)	= tcSplitFunTys sans_foralls
	-- We must use tcSplits here, because we want to see 
	-- the (IO t) in the corner of the type!
        fe_arg_tys | isDyn     = tail fe_arg_tys'
                   | otherwise = fe_arg_tys'
     in
	-- Look at the result type of the exported function, orig_res_ty
	-- If it's IO t, return		(t, True)
	-- If it's plain t, return	(t, False)
     (case tcSplitTyConApp_maybe orig_res_ty of
	-- We must use tcSplit here so that we see the (IO t) in
	-- the type.  [IO t is transparent to plain splitTyConApp.]

	Just (ioTyCon, [res_ty])
	      -> ASSERT( ioTyCon `hasKey` ioTyConKey )
		 -- The function already returns IO t
		 returnDs (res_ty, True)

	other -> -- The function returns t
	         returnDs (orig_res_ty, False)
     )
					`thenDs` \ (res_ty,		-- t
						    is_IO_res_ty) ->	-- Bool
     getModuleDs
					`thenDs` \ mod -> 
     let
      	(h_stub, c_stub) 
           = mkFExportCBits (moduleUserString mod) ext_name 
                            (if isDyn then Nothing else Just fn_id)
                            fe_arg_tys res_ty is_IO_res_ty cconv
     in
     returnDs (h_stub, c_stub)
\end{code}

@foreign export dynamic@ lets you dress up Haskell IO actions
of some fixed type behind an externally callable interface (i.e.,
as a C function pointer). Useful for callbacks and stuff.

\begin{verbatim}
foreign export dynamic f :: (Addr -> Int -> IO Int) -> IO Addr

-- Haskell-visible constructor, which is generated from the above:
-- SUP: No check for NULL from createAdjustor anymore???

f :: (Addr -> Int -> IO Int) -> IO Addr
f cback =
   bindIO (newStablePtr cback)
          (\StablePtr sp# -> IO (\s1# ->
              case _ccall_ createAdjustor cconv sp# ``f_helper'' s1# of
                 (# s2#, a# #) -> (# s2#, A# a# #)))

foreign export "f_helper" f_helper :: StablePtr (Addr -> Int -> IO Int) -> Addr -> Int -> IO Int
-- `special' foreign export that invokes the closure pointed to by the
-- first argument.
\end{verbatim}

\begin{code}
dsFExportDynamic :: Module
		 -> Id
		 -> CCallConv
		 -> DsM ([Binding], SDoc, SDoc)
dsFExportDynamic mod_name id cconv
  =  newSysLocalDs ty					 `thenDs` \ fe_id ->
     let 
        -- hack: need to get at the name of the C stub we're about to generate.
       fe_nm	   = _PK_ (moduleUserString mod_name ++ "_" ++ toCName fe_id)
     in
     dsFExport mod_name id export_ty fe_nm cconv True  	`thenDs` \ (h_code, c_code) ->
     newSysLocalDs arg_ty				`thenDs` \ cback ->
     dsLookupGlobalValue newStablePtrName		`thenDs` \ newStablePtrId ->
     let
	mk_stbl_ptr_app    = mkApps (Var newStablePtrId) [ Type arg_ty, Var cback ]
     in
     dsLookupGlobalValue bindIOName		        `thenDs` \ bindIOId ->
     newSysLocalDs (mkTyConApp stablePtrTyCon [arg_ty]) `thenDs` \ stbl_value ->
     let
      stbl_app cont ret_ty 
	= mkApps (Var bindIOId)
		 [ Type (mkTyConApp stablePtrTyCon [arg_ty])
		 , Type ret_ty
		 , mk_stbl_ptr_app
		 , cont
		 ]

       {-
        The arguments to the external function which will
	create a little bit of (template) code on the fly
	for allowing the (stable pointed) Haskell closure
	to be entered using an external calling convention
	(stdcall, ccall).
       -}
      adj_args      = [ mkIntLitInt (ccallConvToInt cconv)
		      , Var stbl_value
		      , mkLit (MachLabel fe_nm)
		      ]
        -- name of external entry point providing these services.
	-- (probably in the RTS.) 
      adjustor	    = SLIT("createAdjustor")
     in
     dsCCall adjustor adj_args PlayRisky False io_res_ty	`thenDs` \ ccall_adj ->
	-- PlayRisky: the adjustor doesn't allocate in the Haskell heap or do a callback
     let ccall_adj_ty = exprType ccall_adj
         ccall_io_adj = mkLams [stbl_value]		     $
			Note (Coerce io_res_ty ccall_adj_ty)
			     ccall_adj
         io_app = mkLams tvs	 $
		  mkLams [cback] $
		  stbl_app ccall_io_adj res_ty
	 fed = (id `setInlinePragma` NeverActive, io_app)
		-- Never inline the f.e.d. function, because the litlit
		-- might not be in scope in other modules.
     in
     returnDs ([fed], h_code, c_code)

 where
  ty			= idType id
  (tvs,sans_foralls)	= tcSplitForAllTys ty
  ([arg_ty], io_res_ty)	= tcSplitFunTys sans_foralls
  [res_ty]		= tcTyConAppArgs io_res_ty
	-- Must use tcSplit* to see the (IO t), which is a newtype
  export_ty		= mkFunTy (mkTyConApp stablePtrTyCon [arg_ty]) arg_ty

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
mkFExportCBits :: String
	       -> FAST_STRING
	       -> Maybe Id 	-- Just==static, Nothing==dynamic
	       -> [Type] 
	       -> Type 
               -> Bool		-- True <=> returns an IO type
	       -> CCallConv 
	       -> (SDoc, SDoc)
mkFExportCBits mod_nm c_nm maybe_target arg_htys res_hty is_IO_res_ty cc 
 = (header_bits, c_bits)
 where
  -- Create up types and names for the real args
  arg_cnames, arg_ctys :: [SDoc]
  arg_cnames = mkCArgNames 1 arg_htys
  arg_ctys   = map showStgType arg_htys

  -- and also for auxiliary ones; the stable ptr in the dynamic case, and
  -- a slot for the dummy return address in the dynamic + ccall case
  extra_cnames_and_ctys
     = case maybe_target of
          Nothing -> [(text "the_stableptr", text "StgStablePtr")]
          other   -> []
       ++
       case (maybe_target, cc) of
          (Nothing, CCallConv) -> [(text "original_return_addr", text "void*")]
          other                -> []

  all_cnames_and_ctys :: [(SDoc, SDoc)]
  all_cnames_and_ctys 
     = extra_cnames_and_ctys ++ zip arg_cnames arg_ctys

  -- stuff to do with the return type of the C function
  res_hty_is_unit = res_hty `eqType` unitTy	-- Look through any newtypes

  cResType | res_hty_is_unit = text "void"
	   | otherwise	     = showStgType res_hty

  -- Now we can cook up the prototype for the exported function.
  pprCconv = case cc of
		CCallConv   -> empty
		StdCallConv -> text (ccallConvAttribute cc)

  header_bits = ptext SLIT("extern") <+> fun_proto <> semi

  fun_proto = cResType <+> pprCconv <+> ptext c_nm <>
	      parens (hsep (punctuate comma (map (\(nm,ty) -> ty <+> nm) 
                                                 all_cnames_and_ctys)))

  -- the target which will form the root of what we ask rts_evalIO to run
  the_cfun
     = case maybe_target of
          Nothing    -> text "(StgClosure*)deRefStablePtr(the_stableptr)"
          Just hs_fn -> ppr hs_fn <> text "_closure"

  -- the expression we give to rts_evalIO
  expr_to_run
     = foldl appArg the_cfun (zip arg_cnames arg_htys)
       where
          appArg acc (arg_cname, arg_hty) 
             = text "rts_apply" 
               <> parens (acc <> comma <> mkHObj arg_hty <> parens arg_cname)

  -- various other bits for inside the fn
  declareResult = text "HaskellObj ret;"

  return_what | res_hty_is_unit = empty
	      | otherwise       = parens (unpackHObj res_hty <> parens (text "ret"))

  -- an extern decl for the fn being called
  extern_decl
     = case maybe_target of
          Nothing -> empty
          Just hs_fn -> text "extern StgClosure* " <> ppr hs_fn <> text "_closure" <> semi

  -- finally, the whole darn thing
  c_bits =
    space $$
    extern_decl $$
    fun_proto  $$
    vcat 
     [ lbrace
     ,   text "SchedulerStatus rc;"
     ,   declareResult
	  -- create the application + perform it.
     ,   text (if is_IO_res_ty then "rc=rts_evalIO" else "rc=rts_eval")
         <> parens (expr_to_run <+> comma <> text "&ret")
         <> semi
     ,   text "rts_checkSchedStatus" <> parens (doubleQuotes (ptext c_nm)
						<> comma <> text "rc") <> semi
     ,   text "return" <> return_what <> semi
     , rbrace
     ]


mkCArgNames :: Int -> [a] -> [SDoc]
mkCArgNames n as = zipWith (\ _ n -> text ('a':show n)) as [n..] 

mkHObj :: Type -> SDoc
mkHObj t = text "rts_mk" <> text (showFFIType t)

unpackHObj :: Type -> SDoc
unpackHObj t = text "rts_get" <> text (showFFIType t)

showStgType :: Type -> SDoc
showStgType t = text "Hs" <> text (showFFIType t)

showFFIType :: Type -> String
showFFIType t = getOccString (getName tc)
 where
  tc = case tcSplitTyConApp_maybe (repType t) of
	    Just (tc,_) -> tc
	    Nothing	-> pprPanic "showFFIType" (ppr t)
\end{code}
