%
% (c) The AQUA Project, Glasgow University, 1998
%
\section[DsCCall]{Desugaring \tr{foreign} declarations}

Expanding out @foreign import@ and @foreign export@ declarations.

\begin{code}
module DsForeign ( dsForeigns ) where

#include "HsVersions.h"

import CoreSyn

import DsCCall		( dsCCall, mkCCall, boxResult, unboxArg, wrapUnboxedValue )
import DsMonad
import DsUtils

import HsSyn		( ExtName(..), ForeignDecl(..), isDynamicExtName, ForKind(..) )
import HsDecls		( extNameStatic )
import CallConv
import TcHsSyn		( TypecheckedForeignDecl )
import CoreUtils	( exprType, mkInlineMe, bindNonRec )
import DataCon		( DataCon, dataConWrapId )
import Id		( Id, idType, idName, mkWildId, mkVanillaId )
import MkId		( mkCCallOpId, mkWorkerId )
import Literal		( Literal(..) )
import Module		( Module, moduleUserString )
import Name		( mkGlobalName, nameModule, nameOccName, getOccString, 
			  mkForeignExportOcc, isLocalName,
			  NamedThing(..), Provenance(..), ExportFlag(..)
			)
import PrelInfo		( deRefStablePtr_NAME, bindIO_NAME, makeStablePtr_NAME, realWorldPrimId )
import Type		( splitAlgTyConApp_maybe,  unUsgTy,
			  splitTyConApp_maybe, splitFunTys, splitForAllTys,
			  Type, mkFunTys, mkForAllTys, mkTyConApp,
			  mkTyVarTy, mkFunTy, splitAppTy
			)
import PrimOp		( PrimOp(..), CCall(..), CCallTarget(..) )
import Var		( TyVar )
import TysPrim		( realWorldStatePrimTy, addrPrimTy )
import TysWiredIn	( unitTyCon, addrTy, stablePtrTyCon,
			  unboxedTupleCon, addrDataCon
			)
import Unique
import Maybes		( maybeToBool )
import Outputable

#if __GLASGOW_HASKELL__ >= 404
import GlaExts		( fromInt )
#endif
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
dsForeigns :: Module
           -> [TypecheckedForeignDecl] 
	   -> DsM ( [CoreBind]        -- desugared foreign imports
                  , [CoreBind]        -- helper functions for foreign exports
		  , SDoc	      -- Header file prototypes for
                                      -- "foreign exported" functions.
		  , SDoc 	      -- C stubs to use when calling
                                      -- "foreign exported" functions.
		  )
dsForeigns mod_name fos = foldlDs combine ([],[],empty,empty) fos
 where
  combine (acc_fi, acc_fe, acc_h, acc_c) fo@(ForeignDecl i imp_exp _ ext_nm cconv _) 
    | isForeignImport =   -- foreign import (dynamic)?
        dsFImport i (idType i) uns ext_nm cconv  `thenDs` \ bs -> 
	returnDs (bs ++ acc_fi, acc_fe, acc_h, acc_c)
    | isForeignLabel = 
        dsFLabel i ext_nm `thenDs` \ b -> 
	returnDs (b:acc_fi, acc_fe, acc_h, acc_c)
    | isDynamicExtName ext_nm =
        dsFExportDynamic i (idType i) mod_name ext_nm cconv  `thenDs` \ (fi,fe,h,c) -> 
	returnDs (fi:acc_fi, fe:acc_fe, h $$ acc_h, c $$ acc_c)

    | otherwise	       =  -- foreign export
        dsFExport i (idType i) mod_name ext_nm cconv False   `thenDs` \ (fe,h,c) ->
	returnDs (acc_fi, fe:acc_fe, h $$ acc_h, c $$ acc_c)
   where
    isForeignImport = 
	case imp_exp of
	  FoImport _ -> True
	  _          -> False

    isForeignLabel = 
	case imp_exp of
	  FoLabel -> True
	  _       -> False

    (FoImport uns)   = imp_exp

\end{code}

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
	  -> Type		-- Type of foreign import.
	  -> Bool		-- True <=> might cause Haskell GC
	  -> ExtName
	  -> CallConv
	  -> DsM [CoreBind]
dsFImport fn_id ty may_not_gc ext_name cconv 
  = let
	(tvs, arg_tys, mbIoDataCon, io_res_ty) = splitForeignTyDs ty
	is_io_action 			       = maybeToBool mbIoDataCon
    in
    newSysLocalsDs arg_tys  			`thenDs` \ args ->
    newSysLocalDs realWorldStatePrimTy		`thenDs` \ old_s ->
    mapAndUnzipDs unboxArg (map Var args)	`thenDs` \ (unboxed_args, arg_wrappers) ->

    (if not is_io_action then
       newSysLocalDs realWorldStatePrimTy	`thenDs` \ state_tok ->
       wrapUnboxedValue io_res_ty		`thenDs` \ (ccall_result_ty, v, res_v) ->
       returnDs ( ccall_result_ty
                , \ prim_app -> Case prim_app  (mkWildId ccall_result_ty)
				    [(DataAlt (unboxedTupleCon 2), [state_tok, v], res_v)])
     else
       boxResult io_res_ty)			`thenDs` \ (ccall_result_ty, res_wrapper) ->

    (case ext_name of
       Dynamic       -> getUniqueDs `thenDs` \ u -> 
			returnDs (DynamicTarget u)
       ExtName fs _  -> returnDs (StaticTarget fs))	`thenDs` \ lbl ->

    getUniqueDs						`thenDs` \ ccall_uniq ->
    getUniqueDs						`thenDs` \ work_uniq ->
    let
	the_state_arg | is_io_action = old_s
		      | otherwise    = realWorldPrimId

	-- Build the worker
	val_args      = Var the_state_arg : unboxed_args
	work_arg_ids  = [v | Var v <- val_args]		-- All guaranteed to be vars
	worker_ty     = mkForAllTys tvs (mkFunTys (map idType work_arg_ids) ccall_result_ty)
	the_ccall     = CCall lbl False (not may_not_gc) cconv
 	the_ccall_app = mkCCall ccall_uniq the_ccall val_args ccall_result_ty
	work_rhs      = mkLams tvs (mkLams work_arg_ids the_ccall_app)
	work_id       = mkWorkerId work_uniq fn_id worker_ty

	-- Build the wrapper
	work_app     = mkApps (mkVarApps (Var work_id) tvs) val_args
	wrapper_body = foldr ($) (res_wrapper work_app) arg_wrappers
        io_app 	     = case mbIoDataCon of
			   Nothing        -> wrapper_body
			   Just ioDataCon -> mkApps (Var (dataConWrapId ioDataCon)) 
						    [Type io_res_ty, Lam old_s wrapper_body]
        wrap_rhs = mkInlineMe (mkLams (tvs ++ args) io_app)
    in
    returnDs [NonRec fn_id wrap_rhs, NonRec work_id work_rhs]
\end{code}

Given the type of a foreign import declaration, split it up into
its constituent parts.

\begin{code}
splitForeignTyDs :: Type -> ([TyVar], [Type], Maybe DataCon, Type)
splitForeignTyDs ty
  = case splitAlgTyConApp_maybe res_ty of
       Just (_,(io_res_ty:_),(ioCon:_)) ->   -- .... -> IO t
	     (tvs, arg_tys, Just ioCon, io_res_ty)
       _   ->				     -- .... -> t
	     (tvs, arg_tys, Nothing, res_ty)
  where
   (arg_tys, res_ty)   = splitFunTys sans_foralls
   (tvs, sans_foralls) = splitForAllTys ty
\end{code}

foreign labels 

\begin{code}
dsFLabel :: Id -> ExtName -> DsM CoreBind
dsFLabel nm ext_name = returnDs (NonRec nm fo_rhs)
  where
   fo_rhs = mkConApp addrDataCon [mkLit (MachLitLit enm addrPrimTy)]
   enm    = extNameStatic ext_name
\end{code}

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
dsFExport :: Id
	  -> Type		-- Type of foreign export.
	  -> Module
	  -> ExtName
	  -> CallConv
	  -> Bool		-- True => invoke IO action that's hanging off 
				-- the first argument's stable pointer
	  -> DsM ( CoreBind
		 , SDoc
		 , SDoc
		 )
dsFExport i ty mod_name ext_name cconv isDyn =
     getUniqueDs					`thenDs` \ uniq ->
     getSrcLocDs					`thenDs` \ src_loc ->
     let
	f_helper_glob = mkVanillaId helper_name helper_ty
		      where
			name	            = idName i
			mod	
			 | isLocalName name = mod_name
			 | otherwise        = nameModule name

			occ	            = mkForeignExportOcc (nameOccName name)
			prov	            = LocalDef src_loc Exported
			helper_name         = mkGlobalName uniq mod occ prov
     in
     newSysLocalsDs fe_arg_tys				`thenDs` \ fe_args ->
     (if isDyn then 
        newSysLocalDs stbl_ptr_ty			`thenDs` \ stbl_ptr ->
	newSysLocalDs stbl_ptr_to_ty			`thenDs` \ stbl_value ->
	dsLookupGlobalValue deRefStablePtr_NAME		`thenDs` \ deRefStablePtrId ->
	let
	 the_deref_app = mkApps (Var deRefStablePtrId)
				[ Type stbl_ptr_to_ty, Var stbl_ptr ]
        in
	newSysLocalDs (exprType the_deref_app)	 `thenDs` \ x_deref_app ->
        dsLookupGlobalValue bindIO_NAME			 `thenDs` \ bindIOId ->
	newSysLocalDs (mkFunTy stbl_ptr_to_ty 
			       (mkTyConApp ioTyCon [res_ty])) `thenDs` \ x_cont ->
	let
	 stbl_app      = \ cont -> 
		bindNonRec x_cont   (mkLams [stbl_value] cont) $
		bindNonRec x_deref_app the_deref_app  
			   (mkApps (Var bindIOId)
				     [ Type stbl_ptr_to_ty
				     , Type res_ty
				     , Var x_deref_app
				     , Var x_cont])
        in
	returnDs (stbl_value, stbl_app, stbl_ptr)
      else
        returnDs (i, 
	          \ body -> body,
		  panic "stbl_ptr"  -- should never be touched.
		  ))			`thenDs` \ (i, getFun_wrapper, stbl_ptr) ->
     let
      wrapper_args
       | isDyn      = stbl_ptr:fe_args
       | otherwise  = fe_args

      wrapper_arg_tys
       | isDyn      = stbl_ptr_ty:helper_arg_tys
       | otherwise  = helper_arg_tys

      the_app  = 
         getFun_wrapper $
 	 mkApps (Var i) (map (Type . mkTyVarTy) tvs ++ map Var fe_args)
     in
     getModuleDs			`thenDs` \ mod -> 
     getUniqueDs			`thenDs` \ uniq ->
     let
      the_body = mkLams (tvs ++ wrapper_args) the_app
      c_nm     = extNameStatic ext_name

      (h_stub, c_stub) = fexportEntry (moduleUserString mod)
      				      c_nm f_helper_glob
                                      wrapper_arg_tys the_result_ty cconv isDyn
     in
     returnDs (NonRec f_helper_glob the_body, h_stub, c_stub)

  where

   (tvs,sans_foralls)			= splitForAllTys ty
   (fe_arg_tys', io_res)	        = splitFunTys sans_foralls


   Just (ioTyCon, [res_ty])	        = splitTyConApp_maybe io_res

   (_, stbl_ptr_ty')			= splitForAllTys stbl_ptr_ty
   (_, stbl_ptr_to_ty)			= splitAppTy stbl_ptr_ty'

   fe_arg_tys
     | isDyn	    = tail fe_arg_tys'
     | otherwise    = fe_arg_tys'

   (stbl_ptr_ty, helper_arg_tys) = 
     case fe_arg_tys' of
       (x:xs) | isDyn -> (x,xs)
       ls	      -> (error "stbl_ptr_ty", ls)

   helper_ty      =  
	mkForAllTys tvs $
	mkFunTys arg_tys io_res
        where
	  arg_tys
	   | isDyn	= stbl_ptr_ty : helper_arg_tys
	   | otherwise  = helper_arg_tys

   the_result_ty =
     case splitTyConApp_maybe io_res of
       Just (_,[res_ty]) ->
         case splitTyConApp_maybe res_ty of
	   Just (tc,_) | getUnique tc /= getUnique unitTyCon -> Just res_ty
	   _						     -> Nothing
       _		 -> Nothing
   
\end{code}

@foreign export dynamic@ lets you dress up Haskell IO actions
of some fixed type behind an externally callable interface (i.e.,
as a C function pointer). Useful for callbacks and stuff.

\begin{verbatim}
foreign export stdcall f :: (Addr -> Int -> IO Int) -> IO Addr

-- Haskell-visible constructor, which is generated from the
-- above:

f :: (Addr -> Int -> IO Int) -> IO Addr
f cback = IO ( \ s1# ->
  case makeStablePtr# cback s1# of { StateAndStablePtr# s2# sp# ->
  case _ccall_ "mkAdjustor" sp# ``f_helper'' s2# of
    StateAndAddr# s3# a# ->
    case addr2Int# a# of
      0# -> IOfail s# err
      _  -> 
	 let
	  a :: Addr
	  a = A# a#
	 in
         IOok s3# a)

foreign export "f_helper" f_helper :: StablePtr (Addr -> Int -> IO Int) -> Addr -> Int -> IO Int
-- `special' foreign export that invokes the closure pointed to by the
-- first argument.
\end{verbatim}

\begin{code}
dsFExportDynamic :: Id
		 -> Type		-- Type of foreign export.
		 -> Module
		 -> ExtName
		 -> CallConv
		 -> DsM (CoreBind, CoreBind, SDoc, SDoc)
dsFExportDynamic i ty mod_name ext_name cconv =
     newSysLocalDs ty					 `thenDs` \ fe_id ->
     let 
        -- hack: need to get at the name of the C stub we're about to generate.
       fe_nm	   = toCName fe_id
       fe_ext_name = ExtName (_PK_ fe_nm) Nothing
     in
     dsFExport  i export_ty mod_name fe_ext_name cconv True
     `thenDs` \ (fe@(NonRec fe_helper fe_expr), h_code, c_code) ->
     newSysLocalDs arg_ty			            `thenDs` \ cback ->
     dsLookupGlobalValue makeStablePtr_NAME	   `thenDs` \ makeStablePtrId ->
     let
	mk_stbl_ptr_app    = mkApps (Var makeStablePtrId) [ Type arg_ty, Var cback ]
	mk_stbl_ptr_app_ty = exprType mk_stbl_ptr_app
     in
     newSysLocalDs mk_stbl_ptr_app_ty			`thenDs` \ x_mk_stbl_ptr_app ->
     dsLookupGlobalValue bindIO_NAME		        `thenDs` \ bindIOId ->
     newSysLocalDs (mkTyConApp stablePtrTyCon [arg_ty]) `thenDs` \ stbl_value ->
     let
      stbl_app      = \ x_cont cont ret_ty -> 
	bindNonRec x_cont	     cont	     $
	bindNonRec x_mk_stbl_ptr_app mk_stbl_ptr_app $
		   (mkApps (Var bindIOId)
			   [ Type (mkTyConApp stablePtrTyCon [arg_ty])
			   , Type ret_ty
			   , Var x_mk_stbl_ptr_app
			   , Var x_cont
			   ])

       {-
        The arguments to the external function which will
	create a little bit of (template) code on the fly
	for allowing the (stable pointed) Haskell closure
	to be entered using an external calling convention
	(stdcall, ccall).
       -}
      adj_args      = [ mkIntLitInt (callConvToInt cconv)
		      , Var stbl_value
		      , mkLit (MachLitLit (_PK_ fe_nm) addrPrimTy)
		      ]
        -- name of external entry point providing these services.
	-- (probably in the RTS.) 
      adjustor	    = SLIT("createAdjustor")
     in
     dsCCall adjustor adj_args False False addrTy `thenDs` \ ccall_adj ->
     let ccall_adj_ty = exprType ccall_adj
     in
     newSysLocalDs ccall_adj_ty			  `thenDs` \ x_ccall_adj ->
     let ccall_io_adj = 
	    mkLams [stbl_value]		     $
	    bindNonRec x_ccall_adj ccall_adj $
	    Note (Coerce (mkTyConApp ioTyCon [res_ty]) (unUsgTy ccall_adj_ty))
		 (Var x_ccall_adj)
     in
     newSysLocalDs (exprType ccall_io_adj)	  `thenDs` \ x_ccall_io_adj ->
     let io_app = mkLams tvs	 $
		  mkLams [cback] $
		  stbl_app x_ccall_io_adj ccall_io_adj addrTy
     in
     returnDs (NonRec i io_app, fe, h_code, c_code)

 where
  (tvs,sans_foralls)		   = splitForAllTys ty
  ([arg_ty], io_res)		   = splitFunTys sans_foralls

  Just (ioTyCon, [res_ty])	   = splitTyConApp_maybe io_res

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
	     -> Maybe Type 
	     -> CallConv 
	     -> Bool
	     -> (SDoc, SDoc)
fexportEntry mod_nm c_nm helper args res cc isDyn = (header_bits, c_bits)
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
     ,   returnResult
     , rbrace
     ]

  appArg acc (a,c_a) =
     text "rts_apply" <> parens (acc <> comma <> mkHObj a <> parens c_a)

  cParamTypes  = map showStgType real_args

  cResType = 
   case res of
     Nothing -> text "void"
     Just t  -> showStgType t

  pprCconv
   | cc == cCallConv = empty
   | otherwise	     = pprCallConv cc
     
  declareResult  = text "HaskellObj ret;"

  externDecl     = mkExtern (text "HaskellObj") h_nm

  mkExtern ty nm = text "extern" <+> ty <+> nm <> semi

  returnResult = 
    text "rts_checkSchedStatus" <> 
    parens (doubleQuotes (text mod_nm <> char '.' <> ptext c_nm) <> comma <> text "rc") <> semi $$
    (case res of
      Nothing -> text "return"
      Just _  -> text "return" <> parens (res_name)) <> semi

  res_name = 
    case res of
      Nothing -> empty
      Just t  -> unpackHObj t <> parens (text "ret")

  c_args = mkCArgNames 0 args

  {-
   If we're generating an entry point for a 'foreign export ccall dynamic',
   then we receive the return address of the C function that wants to
   invoke a Haskell function as any other C function, as second arg.
   This arg is unused within the body of the generated C stub, but
   needed by the Adjustor.c code to get the stack cleanup right.
  -}
  (proto_args, real_args)
    | cc == cCallConv && isDyn = ( text "a0" : text "a_" : mkCArgNames 1 (tail args)
				, head args : addrTy : tail args)
    | otherwise = (mkCArgNames 0 args, args)

mkCArgNames :: Int -> [a] -> [SDoc]
mkCArgNames n as = zipWith (\ _ n -> text ('a':show n)) as [n..] 

mkHObj :: Type -> SDoc
mkHObj t = text "rts_mk" <> text (showFFIType t)

unpackHObj :: Type -> SDoc
unpackHObj t = text "rts_get" <> text (showFFIType t)

showStgType :: Type -> SDoc
showStgType t = text "Stg" <> text (showFFIType t)

showFFIType :: Type -> String
showFFIType t = getOccString (getName tc)
 where
  tc = case splitTyConApp_maybe t of
	    Just (tc,_) -> tc
	    Nothing	-> pprPanic "showFFIType" (ppr t)
\end{code}
