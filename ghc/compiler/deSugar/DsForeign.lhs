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

import HsSyn		( ForeignDecl(..), FoExport(..), FoImport(..)  )
import TcHsSyn		( TypecheckedForeignDecl )
import CoreUtils	( exprType, mkInlineMe )
import Id		( Id, idType, idName, mkVanillaGlobal, mkSysLocal,
			  setInlinePragma )
import IdInfo		( neverInlinePrag, vanillaIdInfo )
import Literal		( Literal(..) )
import Module		( Module, moduleUserString )
import Name		( mkGlobalName, nameModule, nameOccName, getOccString, 
			  mkForeignExportOcc, isLocalName,
			  NamedThing(..),
			)
import TcType		( tcSplitTyConApp_maybe, tcFunResultTy,
			  tcSplitFunTys, tcSplitForAllTys,
			  Type, mkFunTys, mkForAllTys, mkTyConApp,
			  mkFunTy, tcSplitAppTy, applyTy, tcEqType, isUnitTy
			)
import Type		( repType )
import ForeignCall	( ForeignCall(..), CCallSpec(..), 
			  Safety(..), playSafe,
			  CExportSpec(..),
			  CCallConv(..), ccallConvToInt
			)
import CStrings		( CLabelString )
import TysWiredIn	( addrTy, stablePtrTyCon )
import TysPrim		( addrPrimTy )
import PrelNames	( hasKey, ioTyConKey, deRefStablePtrName, newStablePtrName,
			  bindIOName, returnIOName
			)
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
		  )
dsForeigns mod_name fos
  = foldlDs combine ([], [], empty, empty) fos
 where
  combine (acc_feb, acc_f, acc_h, acc_c) (ForeignImport id _ spec _) 
    = dsFImport mod_name id spec	`thenDs` \ (bs, h, c) -> 
      returnDs (acc_feb, bs ++ acc_f, h $$ acc_h, c $$ acc_c)

  combine (acc_feb, acc_f, acc_h, acc_c) (ForeignExport id _ (CExport (CExportStatic ext_nm cconv)) _)
    = dsFExport mod_name id (idType id) ext_nm cconv False	`thenDs` \ (feb, b, h, c) ->
      returnDs (feb:acc_feb, b : acc_f, h $$ acc_h, c $$ acc_c)
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
	  -> FoImport
	  -> DsM ([Binding], SDoc, SDoc)
dsFImport mod_name lbl_id (LblImport ext_nm) 
 = ASSERT(fromJust res_ty `tcEqType` addrPrimTy) -- typechecker ensures this
   returnDs ([(lbl_id, rhs)], empty, empty)
 where
   (res_ty, fo_rhs) = resultWrapper (idType lbl_id)
   rhs		    = fo_rhs (mkLit (MachLabel ext_nm))

dsFImport mod_name fn_id (CImport spec)     = dsFCall mod_name fn_id (CCall spec)
dsFImport mod_name fn_id (DNImport spec)    = dsFCall mod_name fn_id (DNCall spec)
dsFImport mod_name fn_id (CDynImport cconv) = dsFExportDynamic mod_name fn_id cconv
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
	  -> DsM ( Id		-- The foreign-exported Id
		 , Binding
		 , SDoc
		 , SDoc
		 )
dsFExport mod_name fn_id ty ext_name cconv isDyn
  = 	-- BUILD THE returnIO WRAPPER, if necessary
	-- Look at the result type of the exported function, orig_res_ty
	-- If it's IO t, return		(\x.x,	        IO t, t)
	-- If it's plain t, return	(\x.returnIO x, IO t, t)
     (case tcSplitTyConApp_maybe orig_res_ty of
	Just (ioTyCon, [res_ty])
	      -> ASSERT( ioTyCon `hasKey` ioTyConKey )
			-- The function already returns IO t
		 returnDs (\body -> body, orig_res_ty, res_ty)

	other -> 	-- The function returns t, so wrap the call in returnIO
		 dsLookupGlobalValue returnIOName	`thenDs` \ retIOId ->
	         returnDs (\body -> mkApps (Var retIOId) [Type orig_res_ty, body],
		           tcFunResultTy (applyTy (idType retIOId) orig_res_ty), 
				-- We don't have ioTyCon conveniently to hand
			   orig_res_ty)

     )		`thenDs` \ (return_io_wrapper, 	-- Either identity or returnIO
			    io_res_ty, 		-- IO t
			    res_ty) ->		-- t


	-- BUILD THE deRefStablePtr WRAPPER, if necessary
     (if isDyn then 
        newSysLocalDs stbl_ptr_ty			`thenDs` \ stbl_ptr ->
	newSysLocalDs stbl_ptr_to_ty			`thenDs` \ stbl_value ->
	dsLookupGlobalValue deRefStablePtrName		`thenDs` \ deRefStablePtrId ->
        dsLookupGlobalValue bindIOName			`thenDs` \ bindIOId ->
	let
	 the_deref_app = mkApps (Var deRefStablePtrId)
				[ Type stbl_ptr_to_ty, Var stbl_ptr ]

	 stbl_app cont = mkApps (Var bindIOId)
				[ Type stbl_ptr_to_ty
				, Type res_ty
				, the_deref_app
				, mkLams [stbl_value] cont]
        in
	returnDs (stbl_value, stbl_app, stbl_ptr)
      else
        returnDs (fn_id, 
	          \ body -> body,
		  panic "stbl_ptr"  -- should never be touched.
		  ))			`thenDs` \ (i, getFun_wrapper, stbl_ptr) ->


	-- BUILD THE HELPER
     getModuleDs			`thenDs` \ mod -> 
     getUniqueDs			`thenDs` \ uniq ->
     getSrcLocDs			`thenDs` \ src_loc ->
     newSysLocalsDs fe_arg_tys		`thenDs` \ fe_args ->
     let
        wrapper_args | isDyn      = stbl_ptr:fe_args
		     | otherwise  = fe_args

        wrapper_arg_tys | isDyn      = stbl_ptr_ty:fe_arg_tys
		        | otherwise  = fe_arg_tys

	helper_ty =  mkForAllTys tvs $
		     mkFunTys wrapper_arg_tys io_res_ty

	f_helper_glob = mkVanillaGlobal helper_name helper_ty vanillaIdInfo
		      where
			name	            = idName fn_id
			mod	
			 | isLocalName name = mod_name
			 | otherwise        = nameModule name

			occ	            = mkForeignExportOcc (nameOccName name)
			helper_name         = mkGlobalName uniq mod occ src_loc

      	the_app = getFun_wrapper (return_io_wrapper (mkVarApps (Var i) (tvs ++ fe_args)))
      	the_body = mkLams (tvs ++ wrapper_args) the_app
  
      	(h_stub, c_stub) = fexportEntry (moduleUserString mod)
      				      	ext_name f_helper_glob
                                      	wrapper_arg_tys res_ty cconv isDyn
     in
     returnDs (f_helper_glob, (f_helper_glob, the_body), h_stub, c_stub)

  where
   (tvs,sans_foralls)		= tcSplitForAllTys ty
   (fe_arg_tys', orig_res_ty)	= tcSplitFunTys sans_foralls

   (_, stbl_ptr_ty')		= tcSplitForAllTys stbl_ptr_ty
   (_, stbl_ptr_to_ty)		= tcSplitAppTy stbl_ptr_ty'

   fe_arg_tys | isDyn	  = tail fe_arg_tys'
	      | otherwise = fe_arg_tys'

   stbl_ptr_ty | isDyn     = head fe_arg_tys'
	       | otherwise = error "stbl_ptr_ty"
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
     dsFExport mod_name id export_ty fe_nm cconv True  	`thenDs` \ (feb, fe, h_code, c_code) ->
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
	 fed = (id `setInlinePragma` neverInlinePrag, io_app)
		-- Never inline the f.e.d. function, because the litlit
		-- might not be in scope in other modules.
     in
     returnDs ([fed, fe], h_code, c_code)

 where
  ty				   = idType id
  (tvs,sans_foralls)		   = tcSplitForAllTys ty
  ([arg_ty], io_res_ty)		   = tcSplitFunTys sans_foralls
  Just (ioTyCon, [res_ty])	   = tcSplitTyConApp_maybe io_res_ty
  export_ty			   = mkFunTy (mkTyConApp stablePtrTyCon [arg_ty]) arg_ty

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
fexportEntry :: String
	     -> FAST_STRING
	     -> Id 
	     -> [Type] 
	     -> Type 
	     -> CCallConv 
	     -> Bool
	     -> (SDoc, SDoc)
fexportEntry mod_nm c_nm helper args res_ty cc isDyn = (header_bits, c_bits)
 where
   -- name of the (Haskell) helper function generated by the desugarer.
  h_nm	    = ppr helper <> text "_closure"
   -- prototype for the exported function.
  header_bits = ptext SLIT("extern") <+> fun_proto <> semi

  fun_proto = cResType <+> pprCconv <+> ptext c_nm <>
	      parens (hsep (punctuate comma (zipWith (<+>) cParamTypes proto_args)))

  c_bits =
    externDecl $$
    fun_proto  $$
    vcat 
     [ lbrace
     ,   text "SchedulerStatus rc;"
     ,   declareResult
	  -- create the application + perform it.
     ,   text "rc=rts_evalIO" <> 
                  parens (foldl appArg (text "(StgClosure*)&" <> h_nm) (zip args c_args) <> comma <> text "&ret") <> semi
     ,   text "rts_checkSchedStatus" <> parens (doubleQuotes (ptext c_nm)
						<> comma <> text "rc") <> semi
     ,   text "return" <> return_what <> semi
     , rbrace
     ]

  appArg acc (a,c_a) =
     text "rts_apply" <> parens (acc <> comma <> mkHObj a <> parens c_a)

  cParamTypes  = map showStgType real_args

  res_ty_is_unit = isUnitTy res_ty

  cResType | res_ty_is_unit = text "void"
	   | otherwise	    = showStgType res_ty

  pprCconv = case cc of
		CCallConv   -> empty
		StdCallConv -> ppr cc
     
  declareResult  = text "HaskellObj ret;"

  externDecl     = mkExtern (text "HaskellObj") h_nm

  mkExtern ty nm = text "extern" <+> ty <+> nm <> semi

  return_what | res_ty_is_unit = empty
	      | otherwise      = parens (unpackHObj res_ty <> parens (text "ret"))

  c_args = mkCArgNames 0 args

  {-
   If we're generating an entry point for a 'foreign export ccall dynamic',
   then we receive the return address of the C function that wants to
   invoke a Haskell function as any other C function, as second arg.
   This arg is unused within the body of the generated C stub, but
   needed by the Adjustor.c code to get the stack cleanup right.
  -}
  (proto_args, real_args)
    = case cc of
	CCallConv | isDyn -> ( text "a0" : text "a_" : mkCArgNames 1 (tail args)
			     , head args : addrTy : tail args)
        other		  -> (mkCArgNames 0 args, args)

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
