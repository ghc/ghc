%
% (c) The AQUA Project, Glasgow University, 1998
%
\section[DsCCall]{Desugaring \tr{foreign} declarations}

Expanding out @foreign import@ and @foreign export@ declarations.

\begin{code}
module DsForeign ( dsForeigns ) where

#include "HsVersions.h"

import CoreSyn

import DsCCall		( dsCCall, boxResult, unboxArg, wrapUnboxedValue	)
import DsMonad
import DsUtils

import HsSyn		( ExtName(..), ForeignDecl(..), isDynamic, ForKind(..) )
import CallConv
import TcHsSyn		( TypecheckedForeignDecl )
import CoreUtils	( coreExprType )
import Const		( Con(..), mkMachInt )
import DataCon		( DataCon, dataConId )
import Id		( Id, idType, idName, mkWildId, mkUserId )
import Const		( Literal(..) )
import Name		( mkGlobalName, nameModule, nameOccName, getOccString, 
			  mkForeignExportOcc,
			  NamedThing(..), Provenance(..), ExportFlag(..)
			)
import PrelVals		( realWorldPrimId )
import PrelInfo		( deRefStablePtr_NAME, bindIO_NAME, makeStablePtr_NAME )
import Type		( splitAlgTyConApp_maybe, 
			  splitTyConApp_maybe, splitFunTys, splitForAllTys,
			  Type, mkFunTys, mkForAllTys, mkTyConApp,
			  mkTyVarTy, mkFunTy, splitAppTy
			)
import PrimOp		( PrimOp(..) )
import Var		( TyVar )
import TysPrim		( realWorldStatePrimTy, addrPrimTy )
import TysWiredIn	( unitTyCon, addrTy, stablePtrTyCon,
			  unboxedTupleCon, addrDataCon
			)
import Unique
import Outputable
\end{code}

Desugaring of @foreign@ declarations is naturally split up into
parts, an @import@ and an @export@  part. A @foreign import@ 
declaration 

  foreign import cc nm f :: prim_args -> IO prim_res

is the same as

  f :: prim_args -> IO prim_res
  f a1 ... an = _ccall_ nm cc a1 ... an

so we reuse the desugaring code in @DsCCall@ to deal with these.

\begin{code}
dsForeigns :: [TypecheckedForeignDecl] 
	   -> DsM ( [CoreBind]        -- desugared foreign imports
                  , [CoreBind]        -- helper functions for foreign exports
		  , SDoc	      -- Header file prototypes for "foreign exported" functions.
		  , SDoc 	      -- C stubs to use when calling "foreign exported" funs.
		  )
dsForeigns fos = foldlDs combine ([],[],empty,empty) fos
 where
  combine (acc_fi, acc_fe, acc_h, acc_c) fo@(ForeignDecl i imp_exp _ ext_nm cconv _) 
    | isForeignImport =   -- foreign import (dynamic)?
        dsFImport i (idType i) uns ext_nm cconv  `thenDs` \ b -> 
	returnDs (b:acc_fi, acc_fe, acc_h, acc_c)
    | isForeignLabel = 
        dsFLabel i ext_nm `thenDs` \ b -> 
	returnDs (b:acc_fi, acc_fe, acc_h, acc_c)
    | isDynamic ext_nm =
        dsFExportDynamic i (idType i) ext_nm cconv  `thenDs` \ (fi,fe,h,c) -> 
	returnDs (fi:acc_fi, fe:acc_fe, h $$ acc_h, c $$ acc_c)

    | otherwise	       =  -- foreign export
        dsFExport i (idType i) ext_nm cconv False   `thenDs` \ (fe,h,c) ->
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
(using the CCallOp primop), before boxing the result up and returning it.

\begin{code}
dsFImport :: Id
	  -> Type		-- Type of foreign import.
	  -> Bool		-- True <=> might cause Haskell GC
	  -> ExtName
	  -> CallConv
	  -> DsM CoreBind
dsFImport nm ty may_not_gc ext_name cconv =
    newSysLocalDs realWorldStatePrimTy	`thenDs` \ old_s ->
    splitForeignTyDs ty			`thenDs` \ (tvs, args, mbIoDataCon, io_res_ty)  ->
    let
	 the_state_arg
	   | is_io_action = old_s
	   | otherwise    = realWorldPrimId

         arg_exprs = map (Var) args

	 is_io_action =
	    case mbIoDataCon of
	      Nothing -> False
	      _	      -> True
    in
    mapAndUnzipDs unboxArg arg_exprs    `thenDs` \ (unboxed_args, arg_wrappers) ->
    (if not is_io_action then
       newSysLocalDs realWorldStatePrimTy `thenDs` \ state_tok ->
       wrapUnboxedValue io_res_ty         `thenDs` \ (ccall_result_ty, v, res_v) ->
       returnDs ( ccall_result_ty
                , \ prim_app -> Case prim_app  (mkWildId ccall_result_ty)
				    [(DataCon (unboxedTupleCon 2), [state_tok, v], res_v)])
     else
       boxResult io_res_ty)			`thenDs` \ (final_result_ty, res_wrapper) ->
    (case ext_name of
       Dynamic       -> getUniqueDs `thenDs` \ u -> 
			returnDs (Right u)
       ExtName fs _  -> returnDs (Left fs))	`thenDs` \ label ->
    let
	val_args   = Var the_state_arg : unboxed_args
	final_args = Type inst_ty : val_args

	-- A CCallOp has type (forall a. a), so we must instantiate
	-- it at the full type, including the state argument
	inst_ty = mkFunTys (map coreExprType val_args) final_result_ty

	the_ccall_op = CCallOp label False (not may_not_gc) cconv

 	the_prim_app = mkPrimApp the_ccall_op (final_args :: [CoreArg])

	body     = foldr ($) (res_wrapper the_prim_app) arg_wrappers

	the_body 
	  | not is_io_action = body
	  | otherwise	     = Lam old_s body
    in
    newSysLocalDs (coreExprType the_body) `thenDs` \ ds ->
    let
      io_app = 
        case mbIoDataCon of
	  Nothing -> Var ds
	  Just ioDataCon ->
	       mkApps (Var (dataConId ioDataCon)) 
      		      [Type io_res_ty, Var ds]

      fo_rhs = mkLams (tvs ++ args)
		      (Let (NonRec ds (the_body::CoreExpr)) io_app)
    in
    returnDs (NonRec nm fo_rhs)
\end{code}

Given the type of a foreign import declaration, split it up into
its constituent parts.

\begin{code}
splitForeignTyDs :: Type -> DsM ([TyVar], [Id], Maybe DataCon, Type)
splitForeignTyDs ty = 
    newSysLocalsDs arg_tys  `thenDs` \ ds_args ->
    case splitAlgTyConApp_maybe res_ty of
       Just (_,(io_res_ty:_),(ioCon:_)) ->   -- .... -> IO t
	     returnDs (tvs, ds_args, Just ioCon, io_res_ty)
       _   ->				     -- .... -> t
	     returnDs (tvs, ds_args, Nothing, res_ty)
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
   enm    =
    case ext_name of
      ExtName f _ -> f
      Dynamic	  -> panic "dsFLabel: Dynamic - shouldn't ever happen."

\end{code}

The function that does most of the work for 'foreign export' declarations.
(see below for the boilerplate code a 'foreign export' declaration expands
 into.)

For each 'foreign export foo' in a module M we generate:

* a C function 'foo', which calls
* a Haskell stub 'M.$ffoo', which calls

the user-written Haskell function 'M.foo'.

\begin{code}
dsFExport :: Id
	  -> Type		-- Type of foreign export.
	  -> ExtName
	  -> CallConv
	  -> Bool		-- True => invoke IO action that's hanging off 
				-- the first argument's stable pointer
	  -> DsM ( CoreBind
		 , SDoc
		 , SDoc
		 )
dsFExport i ty ext_name cconv isDyn =
     getUniqueDs					`thenDs` \ uniq ->
     getSrcLocDs					`thenDs` \ src_loc ->
     let
	f_helper_glob = mkUserId helper_name helper_ty
		      where
			name	    = idName i
			mod	    = nameModule name
			occ	    = mkForeignExportOcc (nameOccName name)
			prov	    = LocalDef src_loc Exported
			helper_name = mkGlobalName uniq mod occ prov
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
	newSysLocalDs (coreExprType the_deref_app)	 `thenDs` \ x_deref_app ->
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
		  ))					`thenDs` \ (i, getFun_wrapper, stbl_ptr) ->
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
     getModuleAndGroupDs		`thenDs` \ (mod,_) -> 
     getUniqueDs			`thenDs` \ uniq ->
     let
      the_body = mkLams (tvs ++ wrapper_args) the_app

      c_nm =
        case ext_name of
	  ExtName fs _ -> fs
	  Dynamic      -> panic "dsFExport: Dynamic - shouldn't ever happen."

      (h_stub, c_stub) = fexportEntry c_nm f_helper_glob wrapper_arg_tys the_result_ty cconv isDyn
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

"foreign export dynamic" lets you dress up Haskell IO actions
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
		 -> ExtName
		 -> CallConv
		 -> DsM (CoreBind, CoreBind, SDoc, SDoc)
dsFExportDynamic i ty ext_name cconv =
     newSysLocalDs ty					 `thenDs` \ fe_id ->
     let 
        -- hack: need to get at the name of the C stub we're about to generate.
       fe_nm	   = toCName fe_id
       fe_ext_name = ExtName (_PK_ fe_nm) Nothing
     in
     dsFExport  i export_ty fe_ext_name cconv True `thenDs` \ (fe@(NonRec fe_helper fe_expr), h_code, c_code) ->
     newSysLocalDs arg_ty			   `thenDs` \ cback ->
     dsLookupGlobalValue makeStablePtr_NAME	   `thenDs` \ makeStablePtrId ->
     let
	mk_stbl_ptr_app    = mkApps (Var makeStablePtrId) [ Type arg_ty, Var cback ]
	mk_stbl_ptr_app_ty = coreExprType mk_stbl_ptr_app
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
      adj_args      = [ mkLit (mkMachInt (fromInt (callConvToInt cconv)))
		      , Var stbl_value
		      , mkLit (MachLitLit (_PK_ fe_nm) addrPrimTy)
		      ]
        -- name of external entry point providing these services.
	-- (probably in the RTS.) 
      adjustor	    = SLIT("createAdjustor")
     in
     dsCCall adjustor adj_args False False addrTy `thenDs` \ ccall_adj ->
     let ccall_adj_ty = coreExprType ccall_adj
     in
     newSysLocalDs ccall_adj_ty			  `thenDs` \ x_ccall_adj ->
     let ccall_io_adj = 
	    mkLams [stbl_value]		     $
	    bindNonRec x_ccall_adj ccall_adj $
	    Note (Coerce (mkTyConApp ioTyCon [res_ty]) ccall_adj_ty)
		 (Var x_ccall_adj)
     in
     newSysLocalDs (coreExprType ccall_io_adj)	  `thenDs` \ x_ccall_io_adj ->
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
fexportEntry :: FAST_STRING 
	     -> Id 
	     -> [Type] 
	     -> Maybe Type 
	     -> CallConv 
	     -> Bool
	     -> (SDoc, SDoc)
fexportEntry c_nm helper args res cc isDyn = (header_bits, c_bits)
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
    parens (doubleQuotes (ptext c_nm) <> comma <> text "rc") <> semi $$
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

  mkCArgNames n as = zipWith (\ _ n -> text ('a':show n)) as [n..] 

mkHObj :: Type -> SDoc
mkHObj t = text "rts_mk" <> showFFIType t

unpackHObj :: Type -> SDoc
unpackHObj t = text "rts_get" <> showFFIType t

showStgType :: Type -> SDoc
showStgType t = text "Stg" <> showFFIType t

showFFIType :: Type -> SDoc
showFFIType t = text (getOccString (getName tc))
 where
  tc = case splitTyConApp_maybe t of
	    Just (tc,_) -> tc
	    Nothing	-> pprPanic "showFFIType" (ppr t)
\end{code}
