%
% (c) The University of Glasgow 2006
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
import TcHsType
import TcExpr
import TcEnv

import FamInst
import FamInstEnv
import Coercion      
import Type
import TypeRep
import ForeignCall
import ErrUtils
import Id
import Name
import RdrName
import DataCon
import TyCon
import TcType
import PrelNames
import DynFlags
import Outputable
import Platform
import SrcLoc
import Bag
import FastString

import Control.Monad
\end{code}

\begin{code}
-- Defines a binding
isForeignImport :: LForeignDecl name -> Bool
isForeignImport (L _ (ForeignImport _ _ _ _)) = True
isForeignImport _                             = False

-- Exports a binding
isForeignExport :: LForeignDecl name -> Bool
isForeignExport (L _ (ForeignExport _ _ _ _)) = True
isForeignExport _                             = False
\end{code}

\begin{code}
-- normaliseFfiType takes the type from an FFI declaration, and
-- evaluates any type synonyms, type functions, and newtypes. However,
-- we are only allowed to look through newtypes if the constructor is
-- in scope.  We return a bag of all the newtype constructors thus found.
normaliseFfiType :: Type -> TcM (Coercion, Type, Bag GlobalRdrElt)
normaliseFfiType ty
    = do fam_envs <- tcGetFamInstEnvs
         normaliseFfiType' fam_envs ty

normaliseFfiType' :: FamInstEnvs -> Type -> TcM (Coercion, Type, Bag GlobalRdrElt)
normaliseFfiType' env ty0 = go [] ty0
  where
    go :: [TyCon] -> Type -> TcM (Coercion, Type, Bag GlobalRdrElt)
    go rec_nts ty | Just ty' <- coreView ty     -- Expand synonyms
        = go rec_nts ty'

    go rec_nts (TyConApp tc tys)
        -- We don't want to look through the IO newtype, even if it is
        -- in scope, so we have a special case for it:
        | tc_key `elem` [ioTyConKey, funPtrTyConKey]
        = children_only

        | isNewTyCon tc         -- Expand newtypes
        = do { rdr_env <- getGlobalRdrEnv 
             ; case checkNewtypeFFI rdr_env rec_nts tc of
                 Nothing  -> children_only
                 Just gre -> do { let nt_co = mkUnbranchedAxInstCo (newTyConCo tc) tys
                                ; (co', ty', gres) <- go rec_nts' nt_rhs
                                ; return (mkTransCo nt_co co', ty', gre `consBag` gres) } }

        | isFamilyTyCon tc              -- Expand open tycons
        , (co, ty) <- normaliseTcApp env tc tys
        , not (isReflCo co)
        = do (co', ty', gres) <- go rec_nts ty
             return (mkTransCo co co', ty', gres)  

        | otherwise
        = children_only
        where
          tc_key = getUnique tc
          children_only 
            = do xs <- mapM (go rec_nts) tys
                 let (cos, tys', gres) = unzip3 xs
                 return (mkTyConAppCo tc cos, mkTyConApp tc tys', unionManyBags gres)
          nt_rhs = newTyConInstRhs tc tys
          rec_nts' | isRecursiveTyCon tc = tc:rec_nts
                   | otherwise           = rec_nts

    go rec_nts (AppTy ty1 ty2)
      = do (coi1, nty1, gres1) <- go rec_nts ty1
           (coi2, nty2, gres2) <- go rec_nts ty2
           return (mkAppCo coi1 coi2, mkAppTy nty1 nty2, gres1 `unionBags` gres2)

    go rec_nts (FunTy ty1 ty2)
      = do (coi1,nty1,gres1) <- go rec_nts ty1
           (coi2,nty2,gres2) <- go rec_nts ty2
           return (mkFunCo coi1 coi2, mkFunTy nty1 nty2, gres1 `unionBags` gres2)

    go rec_nts (ForAllTy tyvar ty1)
      = do (coi,nty1,gres1) <- go rec_nts ty1
           return (mkForAllCo tyvar coi, ForAllTy tyvar nty1, gres1)

    go _ ty@(TyVarTy {}) = return (Refl ty, ty, emptyBag)
    go _ ty@(LitTy {})   = return (Refl ty, ty, emptyBag)


checkNewtypeFFI :: GlobalRdrEnv -> [TyCon] -> TyCon -> Maybe GlobalRdrElt
checkNewtypeFFI rdr_env rec_nts tc 
  | not (tc `elem` rec_nts) 
      -- See Note [Expanding newtypes] in Type.lhs
      -- We can't just use isRecursiveTyCon; sometimes recursion is ok:
      --     newtype T = T (Ptr T)
      --   Here, we don't reject the type for being recursive.
      -- If this is a recursive newtype then it will normally
      -- be rejected later as not being a valid FFI type.
  , Just con <- tyConSingleDataCon_maybe tc
  , [gre] <- lookupGRE_Name rdr_env (dataConName con)
  = Just gre    -- See Note [Newtype constructor usage in foreign declarations]
  | otherwise
  = Nothing
\end{code}

Note [Newtype constructor usage in foreign declarations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GHC automatically "unwraps" newtype constructors in foreign import/export
declarations.  In effect that means that a newtype data constructor is 
used even though it is not mentioned expclitly in the source, so we don't
want to report it as "defined but not used" or "imported but not used".
eg     newtype D = MkD Int
       foreign import foo :: D -> IO ()
Here 'MkD' us used.  See Trac #7408.

GHC also expands type functions during this process, so it's not enough
just to look at the free variables of the declaration.  
eg     type instance F Bool = D
       foreign import bar :: F Bool -> IO ()
Here again 'MkD' is used.

So we really have wait until the type checker to decide what is used.
That's why tcForeignImports and tecForeignExports return a (Bag GRE)
for the newtype constructors they see. Then TcRnDriver can add them 
to the module's usages.


%************************************************************************
%*                                                                      *
\subsection{Imports}
%*                                                                      *
%************************************************************************

\begin{code}
tcForeignImports :: [LForeignDecl Name] -> TcM ([Id], [LForeignDecl Id], Bag GlobalRdrElt)
-- For the (Bag GlobalRdrElt) result, 
-- see Note [Newtype constructor usage in foreign declarations]
tcForeignImports decls
  = do { (ids, decls, gres) <- mapAndUnzip3M tcFImport $
                               filter isForeignImport decls
       ; return (ids, decls, unionManyBags gres) }

tcFImport :: LForeignDecl Name -> TcM (Id, LForeignDecl Id, Bag GlobalRdrElt)
tcFImport (L dloc fo@(ForeignImport (L nloc nm) hs_ty _ imp_decl))
  = setSrcSpan dloc $ addErrCtxt (foreignDeclCtxt fo)  $
    do { sig_ty <- tcHsSigType (ForSigCtxt nm) hs_ty
       ; (norm_co, norm_sig_ty, gres) <- normaliseFfiType sig_ty
       ; let
           -- Drop the foralls before inspecting the
           -- structure of the foreign type.
             (_, t_ty)         = tcSplitForAllTys norm_sig_ty
             (arg_tys, res_ty) = tcSplitFunTys t_ty
             id                = mkLocalId nm sig_ty
                 -- Use a LocalId to obey the invariant that locally-defined
                 -- things are LocalIds.  However, it does not need zonking,
                 -- (so TcHsSyn.zonkForeignExports ignores it).

       ; imp_decl' <- tcCheckFIType sig_ty arg_tys res_ty imp_decl
          -- Can't use sig_ty here because sig_ty :: Type and
          -- we need HsType Id hence the undefined
       ; let fi_decl = ForeignImport (L nloc id) undefined (mkSymCo norm_co) imp_decl'
       ; return (id, L dloc fi_decl, gres) }
tcFImport d = pprPanic "tcFImport" (ppr d)
\end{code}


------------ Checking types for foreign import ----------------------
\begin{code}
tcCheckFIType :: Type -> [Type] -> Type -> ForeignImport -> TcM ForeignImport

tcCheckFIType sig_ty arg_tys res_ty (CImport cconv safety mh l@(CLabel _))
  -- Foreign import label
  = do checkCg checkCOrAsmOrLlvmOrInterp
       -- NB check res_ty not sig_ty!
       --    In case sig_ty is (forall a. ForeignPtr a)
       check (null arg_tys && isFFILabelTy res_ty) (illegalForeignLabelErr sig_ty)
       cconv' <- checkCConv cconv
       return (CImport cconv' safety mh l)

tcCheckFIType sig_ty arg_tys res_ty (CImport cconv safety mh CWrapper) = do
        -- Foreign wrapper (former f.e.d.)
        -- The type must be of the form ft -> IO (FunPtr ft), where ft is a valid
        -- foreign type.  For legacy reasons ft -> IO (Ptr ft) is accepted, too.
        -- The use of the latter form is DEPRECATED, though.
    checkCg checkCOrAsmOrLlvmOrInterp
    cconv' <- checkCConv cconv
    case arg_tys of
        [arg1_ty] -> do checkForeignArgs isFFIExternalTy arg1_tys
                        checkForeignRes nonIOok  checkSafe isFFIExportResultTy res1_ty
                        checkForeignRes mustBeIO checkSafe (isFFIDynTy arg1_ty) res_ty
                  where
                     (arg1_tys, res1_ty) = tcSplitFunTys arg1_ty
        _ -> addErrTc (illegalForeignTyErr empty sig_ty)
    return (CImport cconv' safety mh CWrapper)

tcCheckFIType sig_ty arg_tys res_ty idecl@(CImport cconv safety mh (CFunction target))
  | isDynamicTarget target = do -- Foreign import dynamic
      checkCg checkCOrAsmOrLlvmOrInterp
      cconv' <- checkCConv cconv
      case arg_tys of           -- The first arg must be Ptr or FunPtr
        []                -> do
          check False (illegalForeignTyErr empty sig_ty)
        (arg1_ty:arg_tys) -> do
          dflags <- getDynFlags
          let curried_res_ty = foldr FunTy res_ty arg_tys
          check (isFFIDynTy curried_res_ty arg1_ty)
                (illegalForeignTyErr argument arg1_ty)
          checkForeignArgs (isFFIArgumentTy dflags safety) arg_tys
          checkForeignRes nonIOok checkSafe (isFFIImportResultTy dflags) res_ty
      return $ CImport cconv' safety mh (CFunction target)
  | cconv == PrimCallConv = do
      dflags <- getDynFlags
      check (xopt Opt_GHCForeignImportPrim dflags)
            (text "Use -XGHCForeignImportPrim to allow `foreign import prim'.")
      checkCg (checkCOrAsmOrLlvmOrDotNetOrInterp)
      checkCTarget target
      check (playSafe safety)
            (text "The safe/unsafe annotation should not be used with `foreign import prim'.")
      checkForeignArgs (isFFIPrimArgumentTy dflags) arg_tys
      -- prim import result is more liberal, allows (#,,#)
      checkForeignRes nonIOok checkSafe (isFFIPrimResultTy dflags) res_ty
      return idecl
  | otherwise = do              -- Normal foreign import
      checkCg checkCOrAsmOrLlvmOrDotNetOrInterp
      cconv' <- checkCConv cconv
      checkCTarget target
      dflags <- getDynFlags
      checkForeignArgs (isFFIArgumentTy dflags safety) arg_tys
      checkForeignRes nonIOok checkSafe (isFFIImportResultTy dflags) res_ty
      checkMissingAmpersand dflags arg_tys res_ty
      case target of
          StaticTarget _ _ False
           | not (null arg_tys) ->
              addErrTc (text "`value' imports cannot have function types")
          _ -> return ()
      return $ CImport cconv' safety mh (CFunction target)


-- This makes a convenient place to check
-- that the C identifier is valid for C
checkCTarget :: CCallTarget -> TcM ()
checkCTarget (StaticTarget str _ _) = do
    checkCg checkCOrAsmOrLlvmOrDotNetOrInterp
    check (isCLabelString str) (badCName str)

checkCTarget DynamicTarget = panic "checkCTarget DynamicTarget"


checkMissingAmpersand :: DynFlags -> [Type] -> Type -> TcM ()
checkMissingAmpersand dflags arg_tys res_ty
  | null arg_tys && isFunPtrTy res_ty &&
    wopt Opt_WarnDodgyForeignImports dflags
  = addWarn (ptext (sLit "possible missing & in foreign import of FunPtr"))
  | otherwise
  = return ()
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Exports}
%*                                                                      *
%************************************************************************

\begin{code}
tcForeignExports :: [LForeignDecl Name]
                 -> TcM (LHsBinds TcId, [LForeignDecl TcId], Bag GlobalRdrElt)
-- For the (Bag GlobalRdrElt) result, 
-- see Note [Newtype constructor usage in foreign declarations]
tcForeignExports decls
  = foldlM combine (emptyLHsBinds, [], emptyBag) (filter isForeignExport decls)
  where
   combine (binds, fs, gres1) (L loc fe) = do
       (b, f, gres2) <- setSrcSpan loc (tcFExport fe)
       return (b `consBag` binds, L loc f : fs, gres1 `unionBags` gres2)

tcFExport :: ForeignDecl Name -> TcM (LHsBind Id, ForeignDecl Id, Bag GlobalRdrElt)
tcFExport fo@(ForeignExport (L loc nm) hs_ty _ spec)
  = addErrCtxt (foreignDeclCtxt fo) $ do

    sig_ty <- tcHsSigType (ForSigCtxt nm) hs_ty
    rhs <- tcPolyExpr (nlHsVar nm) sig_ty

    (norm_co, norm_sig_ty, gres) <- normaliseFfiType sig_ty

    spec' <- tcCheckFEType norm_sig_ty spec

           -- we're exporting a function, but at a type possibly more
           -- constrained than its declared/inferred type. Hence the need
           -- to create a local binding which will call the exported function
           -- at a particular type (and, maybe, overloading).


    -- We need to give a name to the new top-level binding that
    -- is *stable* (i.e. the compiler won't change it later),
    -- because this name will be referred to by the C code stub.
    id  <- mkStableIdFromName nm sig_ty loc mkForeignExportOcc
    return (mkVarBind id rhs, ForeignExport (L loc id) undefined norm_co spec', gres)
tcFExport d = pprPanic "tcFExport" (ppr d)
\end{code}

------------ Checking argument types for foreign export ----------------------

\begin{code}
tcCheckFEType :: Type -> ForeignExport -> TcM ForeignExport
tcCheckFEType sig_ty (CExport (CExportStatic str cconv)) = do
    checkCg checkCOrAsmOrLlvm
    check (isCLabelString str) (badCName str)
    cconv' <- checkCConv cconv
    checkForeignArgs isFFIExternalTy arg_tys
    checkForeignRes nonIOok noCheckSafe isFFIExportResultTy res_ty
    return (CExport (CExportStatic str cconv'))
  where
      -- Drop the foralls before inspecting n
      -- the structure of the foreign type.
    (_, t_ty) = tcSplitForAllTys sig_ty
    (arg_tys, res_ty) = tcSplitFunTys t_ty
\end{code}



%************************************************************************
%*                                                                      *
\subsection{Miscellaneous}
%*                                                                      *
%************************************************************************

\begin{code}
------------ Checking argument types for foreign import ----------------------
checkForeignArgs :: (Type -> Bool) -> [Type] -> TcM ()
checkForeignArgs pred tys = mapM_ go tys
  where go ty = check (pred ty) (illegalForeignTyErr argument ty)

------------ Checking result types for foreign calls ----------------------
-- | Check that the type has the form
--    (IO t) or (t) , and that t satisfies the given predicate.
-- When calling this function, any newtype wrappers (should) have been
-- already dealt with by normaliseFfiType.
-- 
-- We also check that the Safe Haskell condition of FFI imports having
-- results in the IO monad holds.
--
checkForeignRes :: Bool -> Bool -> (Type -> Bool) -> Type -> TcM ()
checkForeignRes non_io_result_ok check_safe pred_res_ty ty
  = case tcSplitIOType_maybe ty of
        -- Got an IO result type, that's always fine!
        Just (_, res_ty) | pred_res_ty res_ty -> return ()

        -- Case for non-IO result type with FFI Import
        _ -> do
            dflags <- getDynFlags
            case (pred_res_ty ty && non_io_result_ok) of
                -- handle normal typecheck fail, we want to handle this first and
                -- only report safe haskell errors if the normal type check is OK.
                False -> addErrTc $ illegalForeignTyErr result ty

                -- handle safe infer fail
                _ | check_safe && safeInferOn dflags
                    -> recordUnsafeInfer

                -- handle safe language typecheck fail
                _ | check_safe && safeLanguageOn dflags
                    -> addErrTc $ illegalForeignTyErr result ty $+$ safeHsErr

                -- sucess! non-IO return is fine
                _ -> return ()

  where 
    safeHsErr = ptext $ sLit "Safe Haskell is on, all FFI imports must be in the IO monad"

nonIOok, mustBeIO :: Bool
nonIOok  = True
mustBeIO = False

checkSafe, noCheckSafe :: Bool
checkSafe   = True
noCheckSafe = False
\end{code}

Checking a supported backend is in use

\begin{code}
checkCOrAsmOrLlvm :: HscTarget -> Maybe SDoc
checkCOrAsmOrLlvm HscC    = Nothing
checkCOrAsmOrLlvm HscAsm  = Nothing
checkCOrAsmOrLlvm HscLlvm = Nothing
checkCOrAsmOrLlvm _
  = Just (text "requires via-C, llvm (-fllvm) or native code generation (-fvia-C)")

checkCOrAsmOrLlvmOrInterp :: HscTarget -> Maybe SDoc
checkCOrAsmOrLlvmOrInterp HscC           = Nothing
checkCOrAsmOrLlvmOrInterp HscAsm         = Nothing
checkCOrAsmOrLlvmOrInterp HscLlvm        = Nothing
checkCOrAsmOrLlvmOrInterp HscInterpreted = Nothing
checkCOrAsmOrLlvmOrInterp _
  = Just (text "requires interpreted, C, Llvm or native code generation")

checkCOrAsmOrLlvmOrDotNetOrInterp :: HscTarget -> Maybe SDoc
checkCOrAsmOrLlvmOrDotNetOrInterp HscC           = Nothing
checkCOrAsmOrLlvmOrDotNetOrInterp HscAsm         = Nothing
checkCOrAsmOrLlvmOrDotNetOrInterp HscLlvm        = Nothing
checkCOrAsmOrLlvmOrDotNetOrInterp HscInterpreted = Nothing
checkCOrAsmOrLlvmOrDotNetOrInterp _
  = Just (text "requires interpreted, C, Llvm or native code generation")

checkCg :: (HscTarget -> Maybe SDoc) -> TcM ()
checkCg check = do
    dflags <- getDynFlags
    let target = hscTarget dflags
    case target of
      HscNothing -> return ()
      _ ->
        case check target of
          Nothing  -> return ()
          Just err -> addErrTc (text "Illegal foreign declaration:" <+> err)
\end{code}

Calling conventions

\begin{code}
checkCConv :: CCallConv -> TcM CCallConv
checkCConv CCallConv    = return CCallConv
checkCConv CApiConv     = return CApiConv
checkCConv StdCallConv  = do dflags <- getDynFlags
                             let platform = targetPlatform dflags
                             if platformArch platform == ArchX86
                                 then return StdCallConv
                                 else do -- This is a warning, not an error. see #3336
                                         when (wopt Opt_WarnUnsupportedCallingConventions dflags) $
                                             addWarnTc (text "the 'stdcall' calling convention is unsupported on this platform," $$ text "treating as ccall")
                                         return CCallConv
checkCConv PrimCallConv = do addErrTc (text "The `prim' calling convention can only be used with `foreign import'")
                             return PrimCallConv
\end{code}

Warnings

\begin{code}
check :: Bool -> MsgDoc -> TcM ()
check True _       = return ()
check _    the_err = addErrTc the_err

illegalForeignLabelErr :: Type -> SDoc
illegalForeignLabelErr ty
  = vcat [ illegalForeignTyErr empty ty
         , ptext (sLit "A foreign-imported address (via &foo) must have type (Ptr a) or (FunPtr a)") ]

illegalForeignTyErr :: SDoc -> Type -> SDoc
illegalForeignTyErr arg_or_res ty
  = hang (hsep [ptext (sLit "Unacceptable"), arg_or_res,
                ptext (sLit "type in foreign declaration:")])
       2 (hsep [ppr ty])

-- Used for 'arg_or_res' argument to illegalForeignTyErr
argument, result :: SDoc
argument = text "argument"
result   = text "result"

badCName :: CLabelString -> MsgDoc
badCName target
  = sep [quotes (ppr target) <+> ptext (sLit "is not a valid C identifier")]

foreignDeclCtxt :: ForeignDecl Name -> SDoc
foreignDeclCtxt fo
  = hang (ptext (sLit "When checking declaration:"))
       2 (ppr fo)
\end{code}
