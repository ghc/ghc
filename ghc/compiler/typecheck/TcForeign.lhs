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
			  MonoBinds(..), FoImport(..), FoExport(..)
			)
import RnHsSyn		( RenamedHsDecl, RenamedForeignDecl )

import TcMonad
import TcEnv		( newLocalId )
import TcMonoType	( tcHsSigType, UserTypeCtxt(..) )
import TcHsSyn		( TcMonoBinds, TypecheckedForeignDecl, TcForeignExportDecl )
import TcExpr		( tcPolyExpr )			
import Inst		( emptyLIE, LIE, plusLIE )

import ErrUtils		( Message )
import Id		( Id, mkLocalId )
import Name		( nameOccName )
import PrimRep		( getPrimRepSize )
import Type		( typePrimRep )
import TcType		( Type, tcSplitFunTys, tcSplitTyConApp_maybe, tcSplitForAllTys,
			  isFFIArgumentTy, isFFIImportResultTy, 
			  isFFIExportResultTy, isFFILabelTy,
			  isFFIExternalTy, isFFIDynArgumentTy, isFFIDynResultTy
			)
import ForeignCall	( CCallSpec(..), CExportSpec(..), CCallTarget(..), isDynamicTarget, isCasmTarget )
import CStrings		( CLabelString, isCLabelString )
import PrelNames	( hasKey, ioTyConKey )
import CmdLineOpts	( dopt_HscLang, HscLang(..) )
import Outputable

\end{code}

\begin{code}
-- Defines a binding
isForeignImport :: ForeignDecl name -> Bool
isForeignImport (ForeignImport _ _ _ _) = True
isForeignImport _			= False

-- Exports a binding
isForeignExport :: ForeignDecl name -> Bool
isForeignExport (ForeignExport _ _ _ _) = True
isForeignExport _	  	        = False
\end{code}

%************************************************************************
%*									*
\subsection{Imports}
%*									*
%************************************************************************

\begin{code}
tcForeignImports :: [RenamedHsDecl] -> TcM ([Id], [TypecheckedForeignDecl])
tcForeignImports decls = 
   mapAndUnzipTc tcFImport [ foreign_decl | ForD foreign_decl <- decls, isForeignImport foreign_decl]

tcFImport :: RenamedForeignDecl -> TcM (Id, TypecheckedForeignDecl)
tcFImport fo@(ForeignImport nm hs_ty imp_decl src_loc)
 = tcAddSrcLoc src_loc			$
   tcAddErrCtxt (foreignDeclCtxt fo)	$
   tcHsSigType (ForSigCtxt nm) hs_ty	`thenTc`	\ sig_ty ->
   let 
      -- drop the foralls before inspecting the structure
      -- of the foreign type.
	(_, t_ty)	  = tcSplitForAllTys sig_ty
	(arg_tys, res_ty) = tcSplitFunTys t_ty
	id		  = mkLocalId nm sig_ty
   in
   tcCheckFIType sig_ty arg_tys res_ty imp_decl		`thenNF_Tc_` 
   returnTc (id, ForeignImport id undefined imp_decl src_loc)
\end{code}


------------ Checking types for foreign import ----------------------
\begin{code}
tcCheckFIType _ _ _ (DNImport _)
  = checkCg checkDotNet

tcCheckFIType sig_ty arg_tys res_ty (LblImport _)
  = checkCg checkCOrAsm		`thenNF_Tc_`
    check (isFFILabelTy sig_ty) (illegalForeignTyErr empty sig_ty)

tcCheckFIType sig_ty arg_tys res_ty (CDynImport _)
  = 	-- Foreign export dynamic
   	-- The first (and only!) arg has got to be a function type
	-- and it must return IO t; result type is IO Addr
    checkCg checkCOrAsm		`thenNF_Tc_`
    case arg_tys of
	[arg1_ty] -> checkForeignArgs isFFIExternalTy arg1_tys			`thenNF_Tc_`
		     checkForeignRes nonIOok  isFFIExportResultTy res1_ty	`thenNF_Tc_`
		     checkForeignRes mustBeIO isFFIDynResultTy	  res_ty	`thenNF_Tc_`
		     checkFEDArgs arg1_tys
		  where
		     (arg1_tys, res1_ty) = tcSplitFunTys arg1_ty
        other -> addErrTc (illegalForeignTyErr empty sig_ty)

tcCheckFIType sig_ty arg_tys res_ty (CImport (CCallSpec target _ safety))
  | isDynamicTarget target	-- Foreign import dynamic
  = checkCg checkCOrAsmOrInterp		`thenNF_Tc_`
    case arg_tys of		-- The first arg must be Addr
      []     		-> check False (illegalForeignTyErr empty sig_ty)
      (arg1_ty:arg_tys) -> getDOptsTc							`thenNF_Tc` \ dflags ->
			   check (isFFIDynArgumentTy arg1_ty)
				 (illegalForeignTyErr argument arg1_ty)			`thenNF_Tc_`
			   checkForeignArgs (isFFIArgumentTy dflags safety) arg_tys	`thenNF_Tc_`
			   checkForeignRes nonIOok (isFFIImportResultTy dflags) res_ty

  | otherwise 		-- Normal foreign import
  = checkCg (if isCasmTarget target
	     then checkC else checkCOrAsmOrDotNetOrInterp)	`thenNF_Tc_`
    checkCTarget target						`thenNF_Tc_`
    getDOptsTc							`thenNF_Tc` \ dflags ->
    checkForeignArgs (isFFIArgumentTy dflags safety) arg_tys	`thenNF_Tc_`
    checkForeignRes nonIOok (isFFIImportResultTy dflags) res_ty

-- This makes a convenient place to check
-- that the C identifier is valid for C
checkCTarget (StaticTarget str) 
  = checkCg checkCOrAsmOrDotNetOrInterp	 	`thenNF_Tc_`
    check (isCLabelString str) (badCName str)

checkCTarget (CasmTarget _)
  = checkCg checkC
\end{code}

On a SPARC, with foreign export dynamic, due to a giant hack when building
adjustor thunks, we only allow 16 bytes of arguments!

So for example, args (Int,Double,Int) would be OK (1+2+1)
as would (Int,Int,Int,Int) (1+1+1+1) but not (Int,Double,Double) (1+2+2).

On an Alpha, due to a similar hack, we only allow 4 integer arguments with
foreign export dynamic (i.e., 32 bytes of arguments after padding each
argument to a quadword, excluding floating-point arguments).

The check is needed for both via-C and native-code routes

\begin{code}
#include "nativeGen/NCG.h"
#if sparc_TARGET_ARCH
checkFEDArgs arg_tys
  = check (words_of_args <= 4) err
  where
    words_of_args = sum (map (getPrimRepSize . typePrimRep) arg_tys)
    err = ptext SLIT("On SPARC, I can only handle 4 words of arguments to foreign export dynamic")
#elsif alpha_TARGET_ARCH
checkFEDArgs arg_tys
  = check (integral_args <= 4) err
  where
    integral_args = sum (map (getPrimRepSize . filter (not . isFloatingRep)
                                             . typePrimRep) arg_tys)
    err = ptext SLIT("On Alpha, I can only handle 4 non-floating-point arguments to foreign export dynamic")
#else
checkFEDArgs arg_tys = returnNF_Tc ()
#endif
\end{code}


%************************************************************************
%*									*
\subsection{Exports}
%*									*
%************************************************************************

\begin{code}
tcForeignExports :: [RenamedHsDecl] -> TcM (LIE, TcMonoBinds, [TcForeignExportDecl])
tcForeignExports decls = 
   foldlTc combine (emptyLIE, EmptyMonoBinds, [])
		   [ foreign_decl | ForD foreign_decl <- decls, isForeignExport foreign_decl]
  where
   combine (lie, binds, fs) fe = 
       tcFExport fe `thenTc ` \ (a_lie, b, f) ->
       returnTc (lie `plusLIE` a_lie, b `AndMonoBinds` binds, f:fs)

tcFExport :: RenamedForeignDecl -> TcM (LIE, TcMonoBinds, TcForeignExportDecl)
tcFExport fo@(ForeignExport nm hs_ty spec src_loc) =
   tcAddSrcLoc src_loc			$
   tcAddErrCtxt (foreignDeclCtxt fo)	$

   tcHsSigType (ForSigCtxt nm) hs_ty	`thenTc` \ sig_ty ->
   tcPolyExpr (HsVar nm) sig_ty		`thenTc` \ (rhs, lie, _, _, _) ->

   tcCheckFEType sig_ty spec		`thenTc_`

	  -- we're exporting a function, but at a type possibly more constrained
	  -- than its declared/inferred type. Hence the need
	  -- to create a local binding which will call the exported function
	  -- at a particular type (and, maybe, overloading).
   newLocalId (nameOccName nm) sig_ty src_loc	`thenNF_Tc` \ id ->
   let
	bind  = VarMonoBind id rhs
   in
   returnTc (lie, bind, ForeignExport id undefined spec src_loc)
\end{code}

------------ Checking argument types for foreign export ----------------------

\begin{code}
tcCheckFEType sig_ty (CExport (CExportStatic str _))
  = check (isCLabelString str) (badCName str)		`thenNF_Tc_`
    checkForeignArgs isFFIExternalTy arg_tys  	        `thenNF_Tc_`
    checkForeignRes nonIOok isFFIExportResultTy res_ty
  where
      -- Drop the foralls before inspecting n
      -- the structure of the foreign type.
    (_, t_ty) = tcSplitForAllTys sig_ty
    (arg_tys, res_ty) = tcSplitFunTys t_ty
\end{code}



%************************************************************************
%*									*
\subsection{Miscellaneous}
%*									*
%************************************************************************

\begin{code}
------------ Checking argument types for foreign import ----------------------
checkForeignArgs :: (Type -> Bool) -> [Type] -> NF_TcM ()
checkForeignArgs pred tys
  = mapNF_Tc go tys	`thenNF_Tc_` returnNF_Tc ()
  where
    go ty = check (pred ty) (illegalForeignTyErr argument ty)


------------ Checking result types for foreign calls ----------------------
-- Check that the type has the form 
--    (IO t) or (t) , and that t satisfies the given predicate.
--
checkForeignRes :: Bool -> (Type -> Bool) -> Type -> NF_TcM ()

nonIOok  = True
mustBeIO = False

checkForeignRes non_io_result_ok pred_res_ty ty
 = case tcSplitTyConApp_maybe ty of
      Just (io, [res_ty]) 
        | io `hasKey` ioTyConKey && pred_res_ty res_ty 
	-> returnNF_Tc ()
      _   
        -> check (non_io_result_ok && pred_res_ty ty) 
		 (illegalForeignTyErr result ty)
\end{code}

\begin{code} 
checkDotNet HscILX = Nothing
checkDotNet other  = Just (text "requires .NET code generation (-filx)")

checkC HscC  = Nothing
checkC other = Just (text "requires C code generation (-fvia-C)")
			   
checkCOrAsm HscC   = Nothing
checkCOrAsm HscAsm = Nothing
checkCOrAsm other  
   = Just (text "requires via-C or native code generation (-fvia-C)")

checkCOrAsmOrInterp HscC           = Nothing
checkCOrAsmOrInterp HscAsm         = Nothing
checkCOrAsmOrInterp HscInterpreted = Nothing
checkCOrAsmOrInterp other  
   = Just (text "requires interpreted, C or native code generation")

checkCOrAsmOrDotNet HscC   = Nothing
checkCOrAsmOrDotNet HscAsm = Nothing
checkCOrAsmOrDotNet HscILX = Nothing
checkCOrAsmOrDotNet other  
   = Just (text "requires C, native or .NET ILX code generation")

checkCOrAsmOrDotNetOrInterp HscC           = Nothing
checkCOrAsmOrDotNetOrInterp HscAsm         = Nothing
checkCOrAsmOrDotNetOrInterp HscILX         = Nothing
checkCOrAsmOrDotNetOrInterp HscInterpreted = Nothing
checkCOrAsmOrDotNetOrInterp other  
   = Just (text "requires interpreted, C, native or .NET ILX code generation")

checkCg check
 = getDOptsTc		`thenNF_Tc` \ dflags ->
   let hscLang = dopt_HscLang dflags in
   case hscLang of
        HscNothing -> returnNF_Tc ()
        otherwise ->
 	  case check hscLang of
	       Nothing  -> returnNF_Tc ()
	       Just err -> addErrTc (text "Illegal foreign declaration:" <+> err)
\end{code} 
			   
Warnings

\begin{code}
check :: Bool -> Message -> NF_TcM ()
check True _	   = returnTc ()
check _    the_err = addErrTc the_err

illegalForeignTyErr arg_or_res ty
  = hang (hsep [ptext SLIT("Unacceptable"), arg_or_res, 
                ptext SLIT("type in foreign declaration:")])
	 4 (hsep [ppr ty])

-- Used for 'arg_or_res' argument to illegalForeignTyErr
argument = text "argument"
result   = text "result"

badCName :: CLabelString -> Message
badCName target 
   = sep [quotes (ppr target) <+> ptext SLIT("is not a valid C identifier")]

foreignDeclCtxt fo
  = hang (ptext SLIT("When checking declaration:"))
         4 (ppr fo)
\end{code}

