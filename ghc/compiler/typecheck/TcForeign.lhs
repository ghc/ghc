%
% (c) The AQUA Project, Glasgow University, 1998
%
\section[TcForeign]{Typechecking \tr{foreign} declarations}

A foreign declaration is used to either give an externally
implemented function a Haskell type (and calling interface) or
give a Haskell function an external calling interface. Either way,
the range of argument and result types these functions can accommodate
is restricted to what the outside world understands (read C), and this
module checks to see if a foreign declaration has got a legal type.

\begin{code}
module TcForeign 
	( 
	  tcForeignImports
        , tcForeignExports
	) where

#include "HsVersions.h"

import HsSyn		( HsDecl(..), ForeignDecl(..), HsExpr(..),
			  ExtName(Dynamic), isDynamicExtName, MonoBinds(..),
			  OutPat(..), ForKind(..)
			)
import RnHsSyn		( RenamedHsDecl, RenamedForeignDecl )

import TcMonad
import TcEnv		( newLocalId )
import TcType		( typeToTcType, tcSplitRhoTy, zonkTcTypeToType )
import TcMonoType	( tcHsTopBoxedType )
import TcHsSyn		( TcMonoBinds, TypecheckedForeignDecl,
			  TcForeignExportDecl )
import TcExpr		( tcId, tcPolyExpr )			
import Inst		( emptyLIE, LIE, plusLIE )
import CoreSyn

import ErrUtils		( Message )
import Id		( Id, idName, mkVanillaId )
import Name		( nameOccName )
import Type		( splitFunTys
			, splitTyConApp_maybe
			, splitForAllTys
			)
import PprType		( {- instance Outputable Type -} )
import TysWiredIn	( isFFIArgumentTy, isFFIResultTy, 
			  isFFIExternalTy, isAddrTy
			)
import Type             ( Type )
import Unique
import Outputable
import Util
import CmdLineOpts	( opt_GlasgowExts )
import Maybes		( maybeToBool )

\end{code}

\begin{code}
tcForeignImports :: [RenamedHsDecl] -> TcM s ([Id], [TypecheckedForeignDecl])
tcForeignImports decls = 
   mapAndUnzipTc tcFImport [ foreign_decl | ForD foreign_decl <- decls, isForeignImport foreign_decl]

tcForeignExports :: [RenamedHsDecl] -> TcM s (LIE, TcMonoBinds, [TcForeignExportDecl])
tcForeignExports decls = 
   foldlTc combine (emptyLIE, EmptyMonoBinds, [])
		   [ foreign_decl | ForD foreign_decl <- decls, isForeignExport foreign_decl]
  where
   combine (lie, binds, fs) fe = 
       tcFExport fe `thenTc ` \ (a_lie, b, f) ->
       returnTc (lie `plusLIE` a_lie, b `AndMonoBinds` binds, f:fs)

-- defines a binding
isForeignImport :: ForeignDecl name -> Bool
isForeignImport (ForeignDecl _ k _ dyn _ _) =
  case k of
    FoImport _ -> True
    FoExport   -> case dyn of { Dynamic -> True ; _ -> False }
    FoLabel    -> True

-- exports a binding
isForeignExport :: ForeignDecl name -> Bool
isForeignExport (ForeignDecl _ FoExport _ ext_nm _ _) = not (isDynamicExtName ext_nm)
isForeignExport _				      = False

\end{code}

\begin{code}
tcFImport :: RenamedForeignDecl -> TcM s (Id, TypecheckedForeignDecl)
tcFImport fo@(ForeignDecl nm FoExport hs_ty Dynamic cconv src_loc) =
   tcAddSrcLoc src_loc		     $
   tcAddErrCtxt (foreignDeclCtxt fo) $
   tcHsTopBoxedType hs_ty	     `thenTc`	\ sig_ty ->
   let
      -- drop the foralls before inspecting the structure
      -- of the foreign type.
    (_, t_ty) = splitForAllTys sig_ty
   in
   case splitFunTys t_ty of
     (arg_tys, res_ty) -> 
	checkForeignExport True t_ty arg_tys res_ty `thenTc_`
	let i = (mkVanillaId nm sig_ty) in
	returnTc (i, (ForeignDecl i FoExport undefined Dynamic cconv src_loc))

tcFImport fo@(ForeignDecl nm FoLabel hs_ty ext_nm cconv src_loc) =
   tcAddSrcLoc src_loc		     $
   tcAddErrCtxt (foreignDeclCtxt fo) $
   tcHsTopBoxedType hs_ty	    `thenTc`	\ sig_ty ->
   let
      -- drop the foralls before inspecting the structure
      -- of the foreign type.
    (_, t_ty) = splitForAllTys sig_ty
   in
   check (isAddrTy t_ty) (illegalForeignTyErr False{-result-} sig_ty) `thenTc_`
   let i = (mkVanillaId nm sig_ty) in
   returnTc (i, (ForeignDecl i FoLabel undefined ext_nm cconv src_loc))

tcFImport fo@(ForeignDecl nm imp_exp@(FoImport isUnsafe) hs_ty ext_nm cconv src_loc) =
   tcAddSrcLoc src_loc		     $
   tcAddErrCtxt (foreignDeclCtxt fo) $

   tcHsTopBoxedType hs_ty	     `thenTc` \ ty ->
    -- Check that the type has the right shape
    -- and that the argument and result types are acceptable.
   let
      -- drop the foralls before inspecting the structure
      -- of the foreign type.
    (_, t_ty) = splitForAllTys ty
   in
   case splitFunTys t_ty of
     (arg_tys, res_ty) ->
        checkForeignImport (isDynamicExtName ext_nm) (not isUnsafe) ty arg_tys res_ty `thenTc_`
	let i = (mkVanillaId nm ty) in
	returnTc (i, (ForeignDecl i imp_exp undefined ext_nm cconv src_loc))

tcFExport :: RenamedForeignDecl -> TcM s (LIE, TcMonoBinds, TcForeignExportDecl)
tcFExport fo@(ForeignDecl nm imp_exp hs_ty ext_nm cconv src_loc) =
   tcAddSrcLoc src_loc		     $
   tcAddErrCtxt (foreignDeclCtxt fo) $

   tcHsTopBoxedType hs_ty	       `thenTc`	\ sig_ty ->
   let sig_tc_ty = typeToTcType sig_ty in
   tcPolyExpr (HsVar nm) sig_tc_ty     `thenTc`    \ (rhs, lie, _, _, _) ->

   let
      -- drop the foralls before inspecting the structure
      -- of the foreign type.
    (_, t_ty) = splitForAllTys sig_ty
   in
   case splitFunTys t_ty of
     (arg_tys, res_ty) -> 
	checkForeignExport False t_ty arg_tys res_ty `thenTc_`
	  -- we're exporting a function, but at a type possibly more constrained
	  -- than its declared/inferred type. Hence the need
	  -- to create a local binding which will call the exported function
	  -- at a particular type (and, maybe, overloading).
	newLocalId (nameOccName nm) sig_tc_ty src_loc	`thenNF_Tc` \ i ->
	let
	    bind  = VarMonoBind i rhs
	in
	returnTc (lie, bind, ForeignDecl i imp_exp undefined ext_nm cconv src_loc)
        --					    ^^^^^^^^^
        -- ToDo: fill the type field in with something sensible.

\end{code}


\begin{code}
checkForeignImport :: Bool -> Bool -> Type -> [Type] -> Type -> TcM s ()
checkForeignImport is_dynamic is_safe ty args res
 | is_dynamic =
    -- * first arg has got to be an Addr
   case args of
     []     -> check False (illegalForeignTyErr True{-Arg-} ty)
     (x:xs) ->
        check (isAddrTy x) (illegalForeignTyErr True{-Arg-} ty) `thenTc_`
        mapTc (checkForeignArg (isFFIArgumentTy is_safe)) xs	`thenTc_`
	checkForeignRes True {-NonIO ok-} isFFIResultTy res
 | otherwise =
     mapTc (checkForeignArg (isFFIArgumentTy is_safe)) args     `thenTc_`
     checkForeignRes True {-NonIO ok-} isFFIResultTy res

checkForeignExport :: Bool -> Type -> [Type] -> Type -> TcM s ()
checkForeignExport is_dynamic ty args res
 | is_dynamic = 
    -- * the first (and only!) arg has got to be a function type
    --   and it must return IO t
    -- * result type is an Addr or IO Addr
   case args of
     [arg]  ->
	case splitFunTys arg of
	   (arg_tys, res_ty) -> 
		mapTc (checkForeignArg isFFIExternalTy) arg_tys		`thenTc_`
		checkForeignRes True  {-NonIO ok-} isFFIResultTy res_ty	`thenTc_`
		checkForeignRes False {-Must be IO-} isAddrTy	   res
     _      -> check False (illegalForeignTyErr True{-Arg-} ty)
 | otherwise =
     mapTc (checkForeignArg isFFIExternalTy) args  	        `thenTc_`
     checkForeignRes True {-NonIO ok-} isFFIResultTy res
 
checkForeignArg :: (Type -> Bool) -> Type -> TcM s ()
checkForeignArg pred ty = check (pred ty) (illegalForeignTyErr True{-Arg-} ty)

-- Check that the type has the form 
--    (IO t) or (t) , and that t satisfies the given predicate.
--
checkForeignRes :: Bool -> (Type -> Bool) -> Type -> TcM s ()
checkForeignRes non_io_result_ok pred_res_ty ty =
 case (splitTyConApp_maybe ty) of
    Just (io, [res_ty]) 
        | (getUnique io) == ioTyConKey && pred_res_ty res_ty 
	-> returnTc ()
    _   
        -> check (non_io_result_ok && pred_res_ty ty) 
		 (illegalForeignTyErr False{-Res-} ty)
\end{code}

Warnings

\begin{code}
check :: Bool -> Message -> TcM s ()
check True _	   = returnTc ()
check _    the_err = addErrTc the_err `thenNF_Tc_` returnTc ()

illegalForeignTyErr isArg ty
  = hang (hsep [ptext SLIT("Unacceptable"), arg_or_res, ptext SLIT("type in foreign declaration:")])
	 4 (hsep [ppr ty])
  where
   arg_or_res
    | isArg     = ptext SLIT("argument")
    | otherwise = ptext SLIT("result")

foreignDeclCtxt fo = 
 hang (ptext SLIT("When checking declaration:"))
  4   (ppr fo)
\end{code}
