%
% (c) The AQUA Project, Glasgow University, 1998
%
\section[DsCCall]{Desugaring \tr{foreign} declarations}

Expanding out @foreign import@ and @foreign export@ declarations.

\begin{code}
module DsForeign ( dsForeigns ) where


#include "HsVersions.h"

import CoreSyn

import DsCCall		( getIoOkDataCon, boxResult, unboxArg,
			  can'tSeeDataConsPanic
			)
import DsMonad
import DsUtils

import HsSyn		( ExtName(..), ForeignDecl(..), isDynamic )
import CallConv
import TcHsSyn		( maybeBoxedPrimType, TypecheckedForeignDecl )
import CoreUtils	( coreExprType )
import Id		( Id, dataConArgTys, idType, idName,
			  mkVanillaId, dataConRawArgTys,
			  dataConTyCon, mkIdVisible
			)
import IdInfo		( noIdInfo )
import Literal		( Literal(..), mkMachInt )
import Maybes		( maybeToBool )
import Name		( nameString, occNameString, nameOccName, nameUnique )
import PrelVals		( packStringForCId, eRROR_ID )
import PrimOp		( PrimOp(..) )
import Type		( isUnpointedType, splitAlgTyConApp_maybe, 
			  splitTyConApp_maybe, splitFunTys, splitForAllTys,
			  Type, mkFunTys, applyTy, mkForAllTys, mkTyConApp,
			  typePrimRep, mkTyVarTy, mkFunTy, splitAppTy
			)
import PrimRep		( showPrimRepToUser, PrimRep(..) )
import TyVar		( TyVar )
import TyCon		( tyConDataCons )
import TysPrim		( byteArrayPrimTy, realWorldStatePrimTy,
			  byteArrayPrimTyCon, mutableByteArrayPrimTyCon,
			  realWorldTy, addrPrimTy, mkStablePtrPrimTy,
			  intPrimTy
			)
import TysWiredIn	( getStatePairingConInfo,
			  unitDataCon, stringTy,
			  realWorldStateTy, stateDataCon,
			  isFFIArgumentTy, unitTy,
			  addrTy, stablePtrTyCon,
			  stateAndPtrPrimDataCon
			)
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
	   -> DsM ( [CoreBinding]        -- desugared foreign imports
                  , [CoreBinding]        -- helper functions for foreign exports
		  , SDoc		 -- auxilliary code to emit into .hc file
		  , SDoc		 -- Header file prototypes for "foreign exported" functions.
		  , SDoc 		 -- C stubs to use when calling "foreign exported" funs.
		  )
dsForeigns fos = foldlDs combine ([],[],empty,empty,empty) fos
 where
  combine (acc_fi, acc_fe, acc_hc, acc_h, acc_c) fo@(ForeignDecl i imp_exp _ ext_nm cconv _) 
    | isForeignImport = 
        dsFImport i (idType i) uns ext_nm cconv  `thenDs` \ b -> 
	returnDs (b:acc_fi, acc_fe, acc_hc, acc_h, acc_c)
    | isDynamic ext_nm =
        dsFExportDynamic i (idType i) ext_nm cconv  `thenDs` \ (fi,fe,hc,h,c) -> 
	returnDs (fi:acc_fi, fe:acc_fe, hc $$ acc_hc, h $$ acc_h, c $$ acc_c)
    | otherwise	       =
        dsFExport i (idType i) ext_nm cconv False   `thenDs` \ (fe,hc,h,c) ->
	returnDs (acc_fi, fe:acc_fe, hc $$ acc_hc, h $$ acc_h, c $$ acc_c)

   where
    isForeignImport = maybeToBool imp_exp
    (Just uns)      = imp_exp

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
	  -> DsM CoreBinding
dsFImport nm ty may_not_gc ext_name cconv =
    newSysLocalDs realWorldStatePrimTy	`thenDs` \ old_s ->
    mkArgs ty				`thenDs` \ (tvs, args, io_res_ty)  ->
    mapAndUnzipDs unboxArg args		`thenDs` \ (unboxed_args, arg_wrappers) ->
    let
	 final_args = Var old_s : unboxed_args
	 (ioOkDataCon, ioDataCon, result_ty) = getIoOkDataCon io_res_ty
    in
    boxResult ioOkDataCon result_ty	`thenDs` \ (final_result_ty, res_wrapper) ->
    (case ext_name of
       Dynamic       -> getUniqueDs `thenDs` \ u -> returnDs (Right u)
       ExtName fs _  -> returnDs (Left fs)) `thenDs` \ label ->
    let
	the_ccall_op = CCallOp label False (not may_not_gc) cconv
			       (map coreExprType final_args)
			       final_result_ty
    in
    mkPrimDs the_ccall_op (map VarArg final_args) `thenDs` \ the_prim_app ->
    let
	the_body = mkValLam [old_s]
			    (foldr ($) (res_wrapper the_prim_app) arg_wrappers)
    in
    newSysLocalDs (coreExprType the_body) `thenDs` \ ds ->
    let
      io_app = mkValApp (mkTyApp (Var ioDataCon) [result_ty]) [VarArg ds]
      fo_rhs = mkTyLam  tvs $
	       mkValLam (map (\ (Var x) -> x) args)
			(mkCoLetAny (NonRec ds the_body) io_app)
    in
    returnDs (NonRec nm fo_rhs)

mkArgs :: Type -> DsM ([TyVar], [CoreExpr], Type)
mkArgs ty = 
  case splitFunTys sans_foralls of
    (arg_tys, res_ty) -> 
       newSysLocalsDs arg_tys  `thenDs` \ ds_args ->
       returnDs (tvs, map Var ds_args, res_ty)
  where
   (tvs, sans_foralls) = splitForAllTys ty
        
\end{code}

\begin{code}
dsFExport :: Id
	  -> Type		-- Type of foreign export.
	  -> ExtName
	  -> CallConv
	  -> Bool		-- True => invoke IO action that's hanging off 
				-- the first argument's stable pointer
	  -> DsM (CoreBinding, SDoc, SDoc, SDoc)
dsFExport i ty ext_name cconv isDyn =
     newSysLocalDs  realWorldStatePrimTy	        `thenDs` \ s1 ->
     newSysLocalDs  realWorldStatePrimTy	        `thenDs` \ s3 ->
     newSysLocalDs  helper_ty			        `thenDs` \ f_helper ->
     newSysLocalsDs helper_arg_tys		        `thenDs` \ helper_args ->
     newSysLocalDs  res_ty			        `thenDs` \ v1 ->
     unboxResult    the_prim_result_ty res_ty s3 v1     `thenDs` \ (state_and_prim_ty, unpack_result) ->
     zipWithDs boxArg fe_arg_tys helper_args	        `thenDs` \ stuff ->
     (if isDyn then 
        newSysLocalDs realWorldStatePrimTy		`thenDs` \ s11 ->
        newSysLocalDs stbl_ptr_ty			`thenDs` \ stbl_ptr ->
	newSysLocalDs stbl_ptr_to_ty			`thenDs` \ f ->
	mkPrimDs DeRefStablePtrOp
	         [TyArg stbl_ptr_to_ty,
		  VarArg (Var stbl_ptr),
		  VarArg (Var s1)]			`thenDs` \ the_deref_app ->
	let
	 stbl_app = \ cont ->
	    Case the_deref_app 
		 (AlgAlts [(stateAndPtrPrimDataCon, [s11, f], cont)]
			  NoDefault)
        in
	returnDs (f, stbl_app, s11, stbl_ptr)
      else
        returnDs (i, 
	          \ body -> body,
		  s1,
		  panic "stbl_ptr"  -- should never be touched.
		  ))					`thenDs` \ (i, getFun_wrapper, s2, stbl_ptr) ->
     let
      (boxed_args, arg_wrappers)  = unzip stuff

      wrapper_args
       | isDyn      = stbl_ptr:helper_args
       | otherwise  = helper_args

      wrapper_arg_tys
       | isDyn      = stbl_ptr_ty:helper_arg_tys
       | otherwise  = helper_arg_tys

      fe_app   = mkGenApp (Var i) (map (TyArg . mkTyVarTy) tvs ++ map VarArg boxed_args)
      the_app  = 
        getFun_wrapper $
        mkValApp (Note (Coerce io_result_ty io_res) fe_app)
	         [VarArg s2]
     in
     newFailLocalDs  (coreExprType the_app)	`thenDs` \ wild ->
     getModuleAndGroupDs			`thenDs` \ (mod,_) -> 
     getUniqueDs				`thenDs` \ uniq ->
     let

      the_body = 
	   mkTyLam  tvs		 $
	   mkValLam wrapper_args $
	   mkValLam [s1]	 $
	   foldr ($) (perform_and_unpack) arg_wrappers

      perform_and_unpack =
         Case the_app (AlgAlts [(ioOkDataCon, [s3, v1], unpack_result)]
		      (BindDefault wild err))

      c_nm =
        case ext_name of
	  ExtName fs _ -> fs

      full_msg = "Exception caught: " ++ _UNPK_ (nameString (idName i))
      msg = NoRepStr (_PK_ full_msg)
      err = mkApp (Var eRROR_ID) [state_and_prim_ty] [LitArg msg]

      f_helper_glob = (mkIdVisible mod uniq f_helper)
      (hc_stub, h_stub, c_stub) = fexportEntry c_nm f_helper_glob wrapper_arg_tys the_prim_result_ty cconv
     in
     returnDs (NonRec f_helper_glob the_body, hc_stub, h_stub, c_stub)
  where
   (tvs,sans_foralls)			  = splitForAllTys ty
   (fe_arg_tys', io_res)	          = splitFunTys sans_foralls
   (ioOkDataCon, ioDataCon, res_ty)       = getIoOkDataCon io_res

   maybe_data_type 			  = splitAlgTyConApp_maybe res_ty
   Just (tycon, tycon_arg_tys, data_cons) = maybe_data_type
   (the_data_con : other_data_cons)       = data_cons

   data_con_arg_tys		      = dataConArgTys the_data_con tycon_arg_tys
   (prim_result_ty : other_args_tys)  = data_con_arg_tys

   ioDataConTy				= idType ioDataCon
   (io_tvs, ioDataConTy')               = splitForAllTys ioDataConTy
   ([arg_ty], _) 		        = splitFunTys ioDataConTy'
   io_result_ty				= applyTy (mkForAllTys io_tvs arg_ty) res_ty

   (_, stbl_ptr_ty')			= splitForAllTys stbl_ptr_ty
   (_, stbl_ptr_to_ty)			= splitAppTy stbl_ptr_ty'

   fe_arg_tys
     | isDyn	    = tail fe_arg_tys'
     | otherwise    = fe_arg_tys'

   (stbl_ptr_ty, helper_arg_tys) = 
     case (map unboxTy fe_arg_tys') of
       (x:xs) | isDyn -> (x,xs)
       ls	      -> (error "stbl_ptr_ty", ls)

   helper_ty      =  
	mkForAllTys tvs $
	mkFunTys (arg_tys ++ [realWorldStatePrimTy])
		 state_and_prim_ty
        where
	  arg_tys
	   | isDyn	= stbl_ptr_ty : helper_arg_tys
	   | otherwise  = helper_arg_tys

   the_prim_result_ty
     | null data_con_arg_tys   = Nothing
     | otherwise	       = Just prim_result_ty

   state_and_prim_ty
     | (null other_data_cons) &&
       (null data_con_arg_tys) = realWorldStateTy
     | otherwise	       = snd (getStatePairingConInfo (unboxTy res_ty))
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
		 -> DsM (CoreBinding, CoreBinding, SDoc, SDoc, SDoc)
dsFExportDynamic i ty ext_name cconv =
     newSysLocalDs ty					 `thenDs` \ fe_id ->
     let 
        -- hack: need to get at the name of the C stub we're about to generate.
       fe_nm	     = toCName fe_id
       fe_ext_name = ExtName (_PK_ fe_nm) Nothing
     in
     dsFExport  i export_ty fe_ext_name cconv True	 `thenDs` \ (fe@(NonRec fe_helper fe_expr), hc_code, h_code, c_code) ->
     newSysLocalDs  realWorldStatePrimTy	         `thenDs` \ s1 ->
     newSysLocalDs  realWorldStatePrimTy	         `thenDs` \ s2 ->
     newSysLocalDs  realWorldStatePrimTy	         `thenDs` \ s3 ->
     newSysLocalDs  arg_ty				 `thenDs` \ cback_arg ->
     newSysLocalDs  arg_ty				 `thenDs` \ cback ->
     newSysLocalDs  (mkStablePtrPrimTy arg_ty)		 `thenDs` \ stbl ->
     newSysLocalDs  addrPrimTy				 `thenDs` \ addrPrim ->
     newSysLocalDs  addrTy				 `thenDs` \ addr ->
     mkPrimDs MakeStablePtrOp [TyArg arg_ty,
			       VarArg (Var cback), 
			       VarArg (Var s1)]	         `thenDs` \ mkStablePtr_app ->
     mkPrimDs Addr2IntOp [VarArg (Var addrPrim)]         `thenDs` \ the_addr2Int_app ->
     boxArg addrTy addrPrim				 `thenDs` \ (addr_result, addrPrim_wrapper) ->
     let
       (stateAndStablePtrPrimDataCon, _)	     = getStatePairingConInfo (mkStablePtrPrimTy arg_ty)
       (stateAndAddrPrimDataCon, stateAndAddrPrimTy) = getStatePairingConInfo addrPrimTy

       cc
        | cconv == stdCallConv = 1
	| otherwise	       = 0

       ccall_args   = [Var s2, Lit (mkMachInt cc),
		       Var stbl, 
		       Lit (MachLitLit (_PK_ fe_nm) AddrRep)]

       label	    = Left SLIT("createAdjustor")
       the_ccall_op = CCallOp label False False{-won't GC-} cCallConv
			      (map coreExprType ccall_args)
			      stateAndAddrPrimTy
     in
     mkPrimDs the_ccall_op (map VarArg ccall_args)     `thenDs` \ the_ccall_app ->
     mkConDs  ioOkDataCon  
              [TyArg res_ty, VarArg (Var s3), VarArg (Var addr_result)]
						       `thenDs` \ ioOkApp ->
     newSysLocalDs intPrimTy			       `thenDs` \ default_val ->
     let
        the_mkStablePtr = \ cont ->
          Case mkStablePtr_app
	      (AlgAlts [(stateAndStablePtrPrimDataCon, [s2, stbl], cont)]
		       NoDefault)

        the_ccall = \ cont ->
	  Case the_ccall_app 
	       (AlgAlts [(stateAndAddrPrimDataCon, [s3, addrPrim], cont)]
		        NoDefault)
        the_addr2Int = \ cont ->
	  Case the_addr2Int_app
	       (PrimAlts [(mkMachInt 0, io_fail)]
			 (BindDefault default_val cont))

        io_fail	 = mkApp (Var eRROR_ID) [coreExprType wrap_res] [LitArg msg]
        full_msg = "Exception caught: " ++ _UNPK_ (nameString (idName i))
        msg	 = NoRepStr (_PK_ full_msg)

        wrap_res = addrPrim_wrapper ioOkApp
        the_body = 
	  mkTyLam tvs          $
	  mkValLam  [cback,s1] $
	  the_mkStablePtr      $
	  the_ccall            $
          the_addr2Int  wrap_res
          
      in	       
      newSysLocalDs (coreExprType the_body) `thenDs` \ ds ->
      newSysLocalDs (mkFunTy realWorldStatePrimTy
			     (coreExprType ioOkApp))  `thenDs` \ ap ->
      let
        io_app    = mkValApp (mkTyApp (Var ioDataCon) [res_ty]) [VarArg ap]
	io_action = 
	  mkTyLam tvs           $
	  mkValLam  [cback_arg] $
	  mkCoLetAny (NonRec ds the_body) $
	  mkCoLetAny (NonRec ap (mkValApp (mkTyApp (Var ds) (map mkTyVarTy tvs)) [VarArg cback_arg])) $
	  io_app
      in
      returnDs (NonRec i io_action, fe, hc_code, h_code, c_code)
 where
  (tvs,sans_foralls)		   = splitForAllTys ty
  ([arg_ty], io_res)		   = splitFunTys sans_foralls
  (ioOkDataCon, ioDataCon, res_ty) = getIoOkDataCon io_res

  ioDataConTy			   = idType ioDataCon
  (io_tvs, ioDataConTy')           = splitForAllTys ioDataConTy
--  ([arg_ty], _) 		   = splitFunTys ioDataConTy'
  io_result_ty			   = applyTy (mkForAllTys io_tvs arg_ty) res_ty

  export_ty			   = mkFunTy (mkTyConApp stablePtrTyCon [arg_ty]) arg_ty

toCName :: Id -> String
toCName i = showSDoc (pprCode CStyle (ppr (idName i)))

\end{code}

%*
%
\subsection{Helper functions}
%
%*

@boxArg@ boxes up an argument in preparation for calling
a function that maybe expects a boxed version of it, i.e.,

\begin{verbatim}
boxArg Addr a# ==> let ds_foo :: Addr ; ds_foo = A# a# in f ...ds_foo..
\end{verbatim}

\begin{code}
boxArg :: Type			     -- Expected type after possible boxing of arg.
       -> Id			     -- The (unboxed) argument
       -> DsM (Id,		     -- To pass as the actual, boxed argument
	       CoreExpr -> CoreExpr  -- Wrapper to box the arg
		)
boxArg box_ty prim_arg
  | isUnpointedType box_ty = returnDs (prim_arg, \body -> body)
    -- Data types with a single constructor, 
    -- which has a single, primitive-typed arg
  | otherwise
  = newSysLocalDs box_ty		`thenDs` \ box_arg ->
    returnDs ( box_arg
	     , Let (NonRec box_arg (mkCon box_data_con tys_applied [VarArg prim_arg]))
	     )
  where
    maybe_boxed_prim_arg_ty  = maybeBoxedPrimType box_ty
    (Just (_,tys_applied,_)) = splitAlgTyConApp_maybe box_ty
    (Just (box_data_con, _)) = maybe_boxed_prim_arg_ty
\end{code}

@foreign export@ed functions may return a value back to the outside world.
@unboxResult@ takes care of converting from the (boxed) value that the
exported action returns to the (unboxed) value that is returned across
the border.

\begin{code}
unboxResult :: Maybe Type	     -- the (unboxed) type we want to return (along with the state token)
				     -- Nothing => no result, just the state token.
	    -> Type		     -- the (boxed) type we have in our hand.
            -> Id		     -- the state token
            -> Id		     -- boxed arg
	    -> DsM (Type,	     -- type of returned expression.
		    CoreExpr)	     -- expr that unboxes result and returns state+unboxed result.

unboxResult mb_res_uboxed_ty res_ty new_s v_boxed 
 | not (maybeToBool mb_res_uboxed_ty) 
 =   -- no result, just return state token
    mkConDs stateDataCon [ TyArg realWorldTy
			 , VarArg (Var new_s)] `thenDs` \ the_st ->
    returnDs (realWorldStateTy, the_st)

 | null data_cons
  -- oops! can't see the data constructors
 = can'tSeeDataConsPanic "result" res_ty

 | (maybeToBool maybe_data_type) &&	    -- Data type
   (null other_data_cons)	 &&	    --  - with one constructor,
   isUnpointedType res_uboxed_ty  	    --  - and of primitive type.
					    -- (Glasgow extension)
 =
   newSysLocalDs res_uboxed_ty	       `thenDs` \ v_unboxed ->
   mkConDs state_and_prim_datacon 
	   ((TyArg realWorldTy):map (TyArg ) tycon_arg_tys ++
	    [ VarArg (Var new_s)
	    , VarArg (Var v_unboxed)]) `thenDs` \ the_result ->
   let
    the_alt = (the_data_con, [v_unboxed], the_result)
   in
   returnDs (state_and_prim_ty,
	     Case (Var v_boxed) (AlgAlts [the_alt] NoDefault))

  | otherwise
  = pprPanic "unboxResult: " (ppr res_ty)
 where
    (Just res_uboxed_ty)		   = mb_res_uboxed_ty

    maybe_data_type 			   = splitAlgTyConApp_maybe res_ty
    Just (tycon, tycon_arg_tys, data_cons) = maybe_data_type
    (the_data_con : other_data_cons)       = data_cons

    (state_and_prim_datacon, state_and_prim_ty) = getStatePairingConInfo res_uboxed_ty

\end{code}

Returned the unboxed type of a (primitive) type:

\begin{code}
unboxTy :: Type -> Type
unboxTy ty
 | isUnpointedType ty || (ty == unitTy) = ty
 | otherwise	      = 
     ASSERT( isFFIArgumentTy ty ) -- legal arg types subsume result types.
     case splitTyConApp_maybe ty of
        Just (tyc,ts) -> 
		case (tyConDataCons tyc) of
		  [dc] -> case (dataConArgTys dc ts) of
			      [ubox]   -> ubox
				-- HACK: for the array types, the prim type is
				-- the second tycon arg.
			      [_,ubox] -> ubox
			      _	       -> pprPanic "unboxTy: " (ppr ty)
		  _ ->  pprPanic "unboxTy: " (ppr ty)
	_ ->  pprPanic "unboxTy: " (ppr ty)

\end{code}

%*
%
\subsection{Generating @foreign export@ stubs}
%
%*

[Severe hack to get @foreign export@ off the ground:]

For each @foreign export@ function, a C stub together with a @.hc@ stub
is generated. The C stub enters the .hc stub, setting up the passing of
parameters from C land to STG land through the use of global variables
(don't worry, this just a temporary solution!). Ditto for the result.

[
The generation of .hc code will go once the transition is
made over to the new rts. Hence the hack, instead of extending
AbsCSyn to cope with the .hc code generated.
]

\begin{code}
fexportEntry :: FAST_STRING -> Id -> [Type] -> Maybe Type -> CallConv -> (SDoc, SDoc, SDoc)
fexportEntry c_nm helper args res cc =
   ( paramArea $$ stopTemplate $$ startTemplate $$ vtblTemplate, h_code, c_code )
 where
  (h_code, c_code) = mkCStub c_nm h_stub_nm args res cc

  paramArea = 
    vcat (zipWith declVar ( res_ty : param_tys ) ( res_name : param_names ) )

   -- name of the (Haskell) helper function generated by the desugarer.
  h_nm	    = ppr helper
  h_stub_nm = text foreign_export_prefix <> h_nm
  closure   = h_nm <> text "_closure"

  param_names = zipWith (\ i _ -> h_stub_nm <> text ('_':show i)) [1..] args
  param_tys   = map (ppr.typePrimRep) args

  (res_name, res_ty) = 
    case res of
      Nothing -> (empty, empty)
      Just t  -> (h_stub_nm <> text "_res", ppr (typePrimRep t))

  startTemplate =
    vcat 
      [ text "extern realWorldZh_closure;"
      , ptext SLIT("STGFUN") <> parens (h_stub_nm)
      , lbrace
      ,  ptext SLIT("FUNBEGIN;")
      ,  text  "RestoreAllStgRegs();"
      ,  stackCheck param_names
      ,  pushRetReg
      ,  pushCont
      ,  pushRealWorld
      ,  vcat (map pushArg (reverse param_names))
      ,  text "Node=" <> closure <> semi
      ,  text "ENT_VIA_NODE();"   -- ticky count
      ,  text "InfoPtr=(D_)(INFO_PTR(Node));"
      ,  text "JMP_(ENTRY_CODE(InfoPtr));"
      ,  text "FUNEND;"
      , rbrace
      ]

  stopTemplate =
    vcat
      [ ptext SLIT("STGFUN") <> parens (text "stop" <> h_stub_nm <> text "DirectReturn")
      , lbrace
      ,  ptext SLIT("FUNBEGIN;")
      ,  assignResult
      ,  popRetReg
      ,  text "#if defined(__STG_GCC_REGS__)"
      ,  text "SaveAllStgRegs();"
      ,  text "#else"
      ,  text "SAVE_Hp = Hp;"
      ,  text "SAVE_HpLim = HpLim;"
      ,  text "#endif"
      ,  text "JMP_(miniInterpretEnd);"
      ,  text "FUNEND;"
      , rbrace
      ]

  vtblTemplate =
    vcat
      [ text "const W_ vtbl_" <> h_stub_nm <> text "[] = {"
      , vcat (punctuate comma (replicate 8 dir_ret))
      , text "};"
      ]
   where
    dir_ret = text "(W_)stop" <> h_stub_nm <> text "DirectReturn"

  assignResult =
    case res of
      Nothing -> empty
      Just _  -> res_name <> equals <> text "R3.i;" -- wrong

  pushRetReg =
    text "SpB -= BREL(1);" $$
    text "*SpB = (W_)RetReg;"

  popRetReg =
    text "RetReg=(StgRetAddr)*SpB;" $$
    text "SpB += BREL(1);"

  pushCont =
    text "RetReg=(StgRetAddr)UNVEC(stop" <> h_stub_nm <> 
    text "DirectReturn,vtbl_" <> h_stub_nm <> text ");"

  pushRealWorld =
    text "SpB -= BREL(1);" $$
    text "*SpB = (W_)realWorldZh_closure;"


  pushArg nm = 
     text "SpB -= BREL(1);" $$
     text "*SpB = (W_)" <> nm <> semi

  stackCheck args =
     text "STK_CHK(LivenessReg,0," <> sz <> text ",0,0,0,0);"
   where
     sz = parens $
          hsep $ punctuate (text " + ") (text "1":(map sizer args))

     sizer x = text "BYTES_TO_STGWORDS" <> parens (text "sizeof" <> parens x)

foreign_export_prefix :: String
foreign_export_prefix = "__fexp_"

mkCStub :: FAST_STRING -> SDoc -> [Type] -> Maybe Type -> CallConv -> (SDoc, SDoc)
mkCStub c_nm h_stub_nm args res cc = 
 ( hsep [ ptext SLIT("extern")
	, cResType
        , pprCconv
	, ptext c_nm
	, parens (hsep (punctuate comma (zipWith (<+>) stubParamTypes stubArgs)))
	, semi
	]
 , vcat 
     [ externDecls
     , cResType
     , pprCconv
     , ptext c_nm <> parens (hsep (punctuate comma stubArgs))
     , vcat (zipWith declVar stubParamTypes stubArgs)
     , lbrace
     ,  vcat (zipWith assignArgs param_names c_args)
     ,  text "miniInterpret" <> parens (parens (text "StgFunPtr") <> h_stub_nm) <> semi
     ,  returnResult
     , rbrace
     ]
 )
 where
  -- tedious hack to let us deal with caller-cleans-up-stack
  -- discipline that the C calling convention uses.
  stubParamTypes
     | cc == cCallConv = ptext SLIT("void*") : cParamTypes
     | otherwise       = cParamTypes
  stubArgs
     | cc == cCallConv = ptext SLIT("_a0") : c_args
     | otherwise       = c_args
      
  param_names = zipWith (\ i _ -> h_stub_nm <> text ('_':show i)) [1..] args
  cParamTypes  = map (text.showPrimRepToUser.typePrimRep) args
  (cResType, cResDecl) = 
   case res of
     Nothing -> (text "void", empty)
     Just t  -> (text (showPrimRepToUser (typePrimRep t)),
		 text "extern" <+> cResType <+> res_name <> semi)

  pprCconv
   | cc == cCallConv = empty
   | otherwise	     = pprCallConv cc
     
  externDecls = 
    vcat (zipWith mkExtern cParamTypes param_names) $$
    cResDecl $$
    text "extern void" <+> h_stub_nm <> text "();"

  mkExtern ty nm = text "extern" <+> ty <+> nm <> semi

  c_args = zipWith (\ _ n -> text ('a':show n)) args [0..] 

  assignArgs p_nm c_arg = p_nm <+> equals <+> c_arg <> semi

  returnResult = 
    case res of
      Nothing -> empty
      Just _  -> text "return" <+> res_name <> semi

  (res_name, res_ty) = 
    case res of
      Nothing -> (empty, empty)
      Just t  -> (h_stub_nm <> text "_res", ppr (typePrimRep t))

declVar :: SDoc -> SDoc -> SDoc
declVar ty var = ty <+> var <> semi

\end{code}

When exporting

   f :: Int -> Int -> Int -> IO Int

we'll emit the following stuff into the .hc file 

\begin{pseudocode}
StgInt __f_param_1;
StgInt __f_param_2;
StgInt __f_param_3;
StgInt __f_res;

STGFUN(ds_f)
{
   FUNBEGIN;
   RestoreAllStgRegs();
   STK_CHK(LivenessReg,0/*A*/,(SIZE_IN_WORDS(StgInt) + 
			       SIZE_IN_WORDS(StgInt) +
			       SIZE_IN_WORDS(StgInt) + 1)/*B*/, 0, 0, 0/*prim*/, 0/*re-enter*/);
   RetReg = (StgRetAddr) UNVEC(stopds_fDirectReturn,vtbl_stopds_f);
   SpB  -= BREL(1);
   *SpB  = (W_)__f_param_3;
   SpB  -= BREL(1);
   *SpB  = (W_)__f_param_2;
   SpB  -= BREL(1);
   *SpB  = (W_)__f_param_1;

    SpB -= BREL(1);
    *SpB = (W_) realWorldZh_closure;

    Node = ds_f_helper_closure;
    ENT_VIA_NODE();
    InfoPtr=(D_)(INFO_PTR(Node));
    JMP_(ENTRY_CODE(InfoPtr));
    FUNEND;
}

STGFUN(stop_ds_fDirectReturn)
{
   FUNBEGIN;
   __f_res=R1.i;   
   SaveAllStgRegs();
   RESUME(miniInterpretEnd);
   FUNEND;
}

const W_ vtbl_stopds_f[] = {
  (W_) stopds_fDirectReturn,
  (W_) stopds_fDirectReturn,
  (W_) stopds_fDirectReturn,
  (W_) stopds_fDirectReturn,
  (W_) stopds_fDirectReturn,
  (W_) stopds_fDirectReturn,
  (W_) stopds_fDirectReturn,
  (W_) stopds_fDirectReturn
};

\end{pseudocode}

and a C stub

\begin{pseudocode}
extern StgInt __f_param_1;
extern StgInt __f_param_2;
extern StgInt __f_param_3;
extern StgInt __f_res;

extern void ds_f();
extern void miniInterpret(StgAddr);

int
f(a1,a2,a3)
int a1;
int a2;
int a3;
{
 __f_param_1=a1;
 __f_param_2=a2;
 __f_param_3=a3;
 miniInterpret((StgAddr)ds_f);
 return (__f_res);
}

\end{pseudocode}
