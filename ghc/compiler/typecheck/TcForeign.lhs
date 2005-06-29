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

import HsSyn

import TcRnMonad
import TcHsType		( tcHsSigType, UserTypeCtxt(..) )
import TcExpr		( tcCheckSigma )			

import ForeignCall	( CCallConv(..) )
import ErrUtils		( Message )
import Id		( Id, mkLocalId, mkExportedLocalId )
#if alpha_TARGET_ARCH
import Type		( typePrimRep )
import SMRep		( argMachRep, primRepToCgRep, primRepHint )
#endif
import OccName		( mkForeignExportOcc )
import Name		( Name, NamedThing(..), mkExternalName )
import TcType		( Type, tcSplitFunTys, tcSplitTyConApp_maybe,
			  tcSplitForAllTys, 
			  isFFIArgumentTy, isFFIImportResultTy, 
			  isFFIExportResultTy, isFFILabelTy,
			  isFFIExternalTy, isFFIDynArgumentTy,
			  isFFIDynResultTy, isFFIDotnetTy, isFFIDotnetObjTy,
			  toDNType
			)
import ForeignCall	( CExportSpec(..), CCallTarget(..), 
			  CLabelString, isCLabelString,
			  isDynamicTarget, withDNTypes, DNKind(..), DNCallSpec(..) ) 
import PrelNames	( hasKey, ioTyConKey )
import DynFlags		( DynFlags(..), HscTarget(..) )
import Outputable
import SrcLoc		( Located(..), srcSpanStart )
import Bag		( consBag )

#if alpha_TARGET_ARCH
import MachOp		( machRepByteWidth, MachHint(FloatHint) )
#endif
\end{code}

\begin{code}
-- Defines a binding
isForeignImport :: LForeignDecl name -> Bool
isForeignImport (L _ (ForeignImport _ _ _ _)) = True
isForeignImport _			      = False

-- Exports a binding
isForeignExport :: LForeignDecl name -> Bool
isForeignExport (L _ (ForeignExport _ _ _ _)) = True
isForeignExport _	  	              = False
\end{code}

%************************************************************************
%*									*
\subsection{Imports}
%*									*
%************************************************************************

\begin{code}
tcForeignImports :: [LForeignDecl Name] -> TcM ([Id], [LForeignDecl Id])
tcForeignImports decls
  = mapAndUnzipM (wrapLocSndM tcFImport) (filter isForeignImport decls)

tcFImport :: ForeignDecl Name -> TcM (Id, ForeignDecl Id)
tcFImport fo@(ForeignImport (L loc nm) hs_ty imp_decl isDeprec)
 = addErrCtxt (foreignDeclCtxt fo)	$
   tcHsSigType (ForSigCtxt nm) hs_ty	`thenM`	\ sig_ty ->
   let 
      -- drop the foralls before inspecting the structure
      -- of the foreign type.
	(_, t_ty)	  = tcSplitForAllTys sig_ty
	(arg_tys, res_ty) = tcSplitFunTys t_ty
	id		  = mkLocalId nm sig_ty
 		-- Use a LocalId to obey the invariant that locally-defined 
		-- things are LocalIds.  However, it does not need zonking,
		-- (so TcHsSyn.zonkForeignExports ignores it).
   in
   tcCheckFIType sig_ty arg_tys res_ty imp_decl		`thenM` \ imp_decl' -> 
   -- can't use sig_ty here because it :: Type and we need HsType Id
   -- hence the undefined
   returnM (id, ForeignImport (L loc id) undefined imp_decl' isDeprec)
\end{code}


------------ Checking types for foreign import ----------------------
\begin{code}
tcCheckFIType _ arg_tys res_ty (DNImport spec)
  = checkCg checkDotnet  `thenM_`
    getDOpts		 `thenM`  \ dflags ->
    checkForeignArgs (isFFIDotnetTy dflags) arg_tys	`thenM_`
    checkForeignRes True{-non IO ok-} (isFFIDotnetTy dflags) res_ty `thenM_`
    let (DNCallSpec isStatic kind _ _ _ _) = spec in
    (case kind of
       DNMethod | not isStatic ->
         case arg_tys of
	   [] -> addErrTc illegalDNMethodSig
	   _  
	    | not (isFFIDotnetObjTy (last arg_tys)) -> addErrTc illegalDNMethodSig
	    | otherwise -> returnM ()
       _ -> returnM ()) `thenM_`
    returnM (DNImport (withDNTypes spec (map toDNType arg_tys) (toDNType res_ty)))

tcCheckFIType sig_ty arg_tys res_ty idecl@(CImport _ _ _ _ (CLabel _))
  = checkCg checkCOrAsm		`thenM_`
    check (isFFILabelTy sig_ty) (illegalForeignTyErr empty sig_ty) `thenM_`
    return idecl

tcCheckFIType sig_ty arg_tys res_ty idecl@(CImport cconv _ _ _ CWrapper)
  = 	-- Foreign wrapper (former f.e.d.)
   	-- The type must be of the form ft -> IO (FunPtr ft), where ft is a
   	-- valid foreign type.  For legacy reasons ft -> IO (Ptr ft) as well
   	-- as ft -> IO Addr is accepted, too.  The use of the latter two forms
   	-- is DEPRECATED, though.
    checkCg checkCOrAsmOrInterp `thenM_`
    checkCConv cconv 		`thenM_`
    (case arg_tys of
	[arg1_ty] -> checkForeignArgs isFFIExternalTy arg1_tys		     `thenM_`
		     checkForeignRes nonIOok  isFFIExportResultTy res1_ty    `thenM_`
		     checkForeignRes mustBeIO isFFIDynResultTy	  res_ty     `thenM_`
		     checkFEDArgs arg1_tys
		  where
		     (arg1_tys, res1_ty) = tcSplitFunTys arg1_ty
        other -> addErrTc (illegalForeignTyErr empty sig_ty)	)            `thenM_`
    return idecl

tcCheckFIType sig_ty arg_tys res_ty idecl@(CImport cconv safety _ _ (CFunction target))
  | isDynamicTarget target	-- Foreign import dynamic
  = checkCg checkCOrAsmOrInterp		`thenM_`
    checkCConv cconv 			`thenM_`
    case arg_tys of		-- The first arg must be Ptr, FunPtr, or Addr
      []     		-> 
      	check False (illegalForeignTyErr empty sig_ty) `thenM_`
      	return idecl
      (arg1_ty:arg_tys) -> 
      	getDOpts				                     `thenM` \ dflags ->
	check (isFFIDynArgumentTy arg1_ty)
	      (illegalForeignTyErr argument arg1_ty)		     `thenM_`
        checkForeignArgs (isFFIArgumentTy dflags safety) arg_tys     `thenM_`
	checkForeignRes nonIOok (isFFIImportResultTy dflags) res_ty  `thenM_`
	return idecl
  | otherwise 		-- Normal foreign import
  = checkCg (checkCOrAsmOrDotNetOrInterp)			`thenM_`
    checkCConv cconv 						`thenM_`
    checkCTarget target						`thenM_`
    getDOpts							`thenM` \ dflags ->
    checkForeignArgs (isFFIArgumentTy dflags safety) arg_tys	`thenM_`
    checkForeignRes nonIOok (isFFIImportResultTy dflags) res_ty `thenM_`
    return idecl

-- This makes a convenient place to check
-- that the C identifier is valid for C
checkCTarget (StaticTarget str) 
  = checkCg checkCOrAsmOrDotNetOrInterp	 	`thenM_`
    check (isCLabelString str) (badCName str)
\end{code}

On an Alpha, with foreign export dynamic, due to a giant hack when
building adjustor thunks, we only allow 4 integer arguments with
foreign export dynamic (i.e., 32 bytes of arguments after padding each
argument to a quadword, excluding floating-point arguments).

The check is needed for both via-C and native-code routes

\begin{code}
#include "nativeGen/NCG.h"
#if alpha_TARGET_ARCH
checkFEDArgs arg_tys
  = check (integral_args <= 32) err
  where
    integral_args = sum [ (machRepByteWidth . argMachRep . primRepToCgRep) prim_rep
			| prim_rep <- map typePrimRep arg_tys,
			  primRepHint prim_rep /= FloatHint ]
    err = ptext SLIT("On Alpha, I can only handle 32 bytes of non-floating-point arguments to foreign export dynamic")
#else
checkFEDArgs arg_tys = returnM ()
#endif
\end{code}


%************************************************************************
%*									*
\subsection{Exports}
%*									*
%************************************************************************

\begin{code}
tcForeignExports :: [LForeignDecl Name] 
    		 -> TcM (LHsBinds TcId, [LForeignDecl TcId])
tcForeignExports decls
  = foldlM combine (emptyLHsBinds, []) (filter isForeignExport decls)
  where
   combine (binds, fs) fe = 
       wrapLocSndM tcFExport fe	`thenM` \ (b, f) ->
       returnM (b `consBag` binds, f:fs)

tcFExport :: ForeignDecl Name -> TcM (LHsBind Id, ForeignDecl Id)
tcFExport fo@(ForeignExport (L loc nm) hs_ty spec isDeprec) =
   addErrCtxt (foreignDeclCtxt fo)	$

   tcHsSigType (ForSigCtxt nm) hs_ty	`thenM` \ sig_ty ->
   tcCheckSigma (nlHsVar nm) sig_ty	`thenM` \ rhs ->

   tcCheckFEType sig_ty spec		`thenM_`

	  -- we're exporting a function, but at a type possibly more
	  -- constrained than its declared/inferred type. Hence the need
	  -- to create a local binding which will call the exported function
	  -- at a particular type (and, maybe, overloading).

   newUnique			`thenM` \ uniq ->
   getModule			`thenM` \ mod ->
   let
        gnm  = mkExternalName uniq mod (mkForeignExportOcc (getOccName nm)) 
			      Nothing (srcSpanStart loc)
	id   = mkExportedLocalId gnm sig_ty
	bind = L loc (VarBind id rhs)
   in
   returnM (bind, ForeignExport (L loc id) undefined spec isDeprec)
\end{code}

------------ Checking argument types for foreign export ----------------------

\begin{code}
tcCheckFEType sig_ty (CExport (CExportStatic str _))
  = check (isCLabelString str) (badCName str)		`thenM_`
    checkForeignArgs isFFIExternalTy arg_tys  	        `thenM_`
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
checkForeignArgs :: (Type -> Bool) -> [Type] -> TcM ()
checkForeignArgs pred tys
  = mappM go tys		`thenM_` 
    returnM ()
  where
    go ty = check (pred ty) (illegalForeignTyErr argument ty)

------------ Checking result types for foreign calls ----------------------
-- Check that the type has the form 
--    (IO t) or (t) , and that t satisfies the given predicate.
--
checkForeignRes :: Bool -> (Type -> Bool) -> Type -> TcM ()

nonIOok  = True
mustBeIO = False

checkForeignRes non_io_result_ok pred_res_ty ty
 = case tcSplitTyConApp_maybe ty of
      Just (io, [res_ty]) 
        | io `hasKey` ioTyConKey && pred_res_ty res_ty 
	-> returnM ()
      _   
        -> check (non_io_result_ok && pred_res_ty ty) 
		 (illegalForeignTyErr result ty)
\end{code}

\begin{code}
checkDotnet HscILX = Nothing
#if defined(mingw32_TARGET_OS)
checkDotnet HscC   = Nothing
checkDotnet _      = Just (text "requires C code generation (-fvia-C)")
#else
checkDotnet other  = Just (text "requires .NET support (-filx or win32)")
#endif

checkCOrAsm HscC   = Nothing
checkCOrAsm HscAsm = Nothing
checkCOrAsm other  
   = Just (text "requires via-C or native code generation (-fvia-C)")

checkCOrAsmOrInterp HscC           = Nothing
checkCOrAsmOrInterp HscAsm         = Nothing
checkCOrAsmOrInterp HscInterpreted = Nothing
checkCOrAsmOrInterp other  
   = Just (text "requires interpreted, C or native code generation")

checkCOrAsmOrDotNetOrInterp HscC           = Nothing
checkCOrAsmOrDotNetOrInterp HscAsm         = Nothing
checkCOrAsmOrDotNetOrInterp HscILX         = Nothing
checkCOrAsmOrDotNetOrInterp HscInterpreted = Nothing
checkCOrAsmOrDotNetOrInterp other  
   = Just (text "requires interpreted, C, native or .NET ILX code generation")

checkCg check
 = getDOpts		`thenM` \ dflags ->
   let target = hscTarget dflags in
   case target of
     HscNothing -> returnM ()
     otherwise  ->
       case check target of
	 Nothing  -> returnM ()
	 Just err -> addErrTc (text "Illegal foreign declaration:" <+> err)
\end{code} 
			   
Calling conventions

\begin{code}
checkCConv :: CCallConv -> TcM ()
checkCConv CCallConv  = return ()
#if i386_TARGET_ARCH
checkCConv StdCallConv = return ()
#else
checkCConv StdCallConv = addErrTc (text "calling convention not supported on this architecture: stdcall")
#endif
\end{code}

Warnings

\begin{code}
check :: Bool -> Message -> TcM ()
check True _	   = returnM ()
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

illegalDNMethodSig 
  = ptext SLIT("'This pointer' expected as last argument")

\end{code}

