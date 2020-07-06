{-
(c) The University of Glasgow 2006
(c) The AQUA Project, Glasgow University, 1998

-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

-- | Typechecking @foreign@ declarations
--
-- A foreign declaration is used to either give an externally
-- implemented function a Haskell type (and calling interface) or
-- give a Haskell function an external calling interface. Either way,
-- the range of argument and result types these functions can accommodate
-- is restricted to what the outside world understands (read C), and this
-- module checks to see if a foreign declaration has got a legal type.
module GHC.Tc.Gen.Foreign
        ( tcForeignImports
        , tcForeignExports

        -- Low-level exports for hooks
        , isForeignImport, isForeignExport
        , tcFImport, tcFExport
        , tcForeignImports'
        , tcCheckFIType, checkCTarget, checkForeignArgs, checkForeignRes
        , normaliseFfiType
        , nonIOok, mustBeIO
        , checkSafe, noCheckSafe
        , tcForeignExports'
        , tcCheckFEType
        ) where

#include "HsVersions.h"

import GHC.Prelude

import GHC.Hs

import GHC.Tc.Utils.Monad
import GHC.Tc.Gen.HsType
import GHC.Tc.Gen.Expr
import GHC.Tc.Utils.Env

import GHC.Tc.Instance.Family
import GHC.Core.FamInstEnv
import GHC.Core.Coercion
import GHC.Core.Type
import GHC.Core.Multiplicity
import GHC.Types.ForeignCall
import GHC.Utils.Error
import GHC.Types.Id
import GHC.Types.Name
import GHC.Types.Name.Reader
import GHC.Core.DataCon
import GHC.Core.TyCon
import GHC.Core.TyCon.RecWalk
import GHC.Tc.Utils.TcType
import GHC.Builtin.Names
import GHC.Driver.Session
import GHC.Driver.Backend
import GHC.Utils.Outputable as Outputable
import GHC.Utils.Panic
import GHC.Platform
import GHC.Types.SrcLoc
import GHC.Data.Bag
import GHC.Driver.Hooks
import qualified GHC.LanguageExtensions as LangExt

import Control.Monad

-- Defines a binding
isForeignImport :: forall name. UnXRec name => LForeignDecl name -> Bool
isForeignImport (unXRec @name -> ForeignImport {}) = True
isForeignImport _                        = False

-- Exports a binding
isForeignExport :: forall name. UnXRec name => LForeignDecl name -> Bool
isForeignExport (unXRec @name -> ForeignExport {}) = True
isForeignExport _                        = False

{-
Note [Don't recur in normaliseFfiType']
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
normaliseFfiType' is the workhorse for normalising a type used in a foreign
declaration. If we have

newtype Age = MkAge Int

we want to see that Age -> IO () is the same as Int -> IO (). But, we don't
need to recur on any type parameters, because no paramaterized types (with
interesting parameters) are marshalable! The full list of marshalable types
is in the body of boxedMarshalableTyCon in GHC.Tc.Utils.TcType. The only members of that
list not at kind * are Ptr, FunPtr, and StablePtr, all of which get marshaled
the same way regardless of type parameter. So, no need to recur into
parameters.

Similarly, we don't need to look in AppTy's, because nothing headed by
an AppTy will be marshalable.
-}

-- normaliseFfiType takes the type from an FFI declaration, and
-- evaluates any type synonyms, type functions, and newtypes. However,
-- we are only allowed to look through newtypes if the constructor is
-- in scope.  We return a bag of all the newtype constructors thus found.
-- Always returns a Representational coercion
normaliseFfiType :: Type -> TcM (Coercion, Type, Bag GlobalRdrElt)
normaliseFfiType ty
    = do fam_envs <- tcGetFamInstEnvs
         normaliseFfiType' fam_envs ty

normaliseFfiType' :: FamInstEnvs -> Type -> TcM (Coercion, Type, Bag GlobalRdrElt)
normaliseFfiType' env ty0 = go Representational initRecTc ty0
  where
    go :: Role -> RecTcChecker -> Type -> TcM (Coercion, Type, Bag GlobalRdrElt)
    go role rec_nts ty
      | Just ty' <- tcView ty     -- Expand synonyms
      = go role rec_nts ty'

      | Just (tc, tys) <- splitTyConApp_maybe ty
      = go_tc_app role rec_nts tc tys

      | (bndrs, inner_ty) <- splitForAllTyCoVarBinders ty
      , not (null bndrs)
      = do (coi, nty1, gres1) <- go role rec_nts inner_ty
           return ( mkHomoForAllCos (binderVars bndrs) coi
                  , mkForAllTys bndrs nty1, gres1 )

      | otherwise -- see Note [Don't recur in normaliseFfiType']
      = return (mkReflCo role ty, ty, emptyBag)

    go_tc_app :: Role -> RecTcChecker -> TyCon -> [Type]
              -> TcM (Coercion, Type, Bag GlobalRdrElt)
    go_tc_app role rec_nts tc tys
        -- We don't want to look through the IO newtype, even if it is
        -- in scope, so we have a special case for it:
        | tc_key `elem` [ioTyConKey, funPtrTyConKey, funTyConKey]
        = children_only

        | isNewTyCon tc         -- Expand newtypes
        , Just rec_nts' <- checkRecTc rec_nts tc
                   -- See Note [Expanding newtypes] in GHC.Core.TyCon
                   -- We can't just use isRecursiveTyCon; sometimes recursion is ok:
                   --     newtype T = T (Ptr T)
                   --   Here, we don't reject the type for being recursive.
                   -- If this is a recursive newtype then it will normally
                   -- be rejected later as not being a valid FFI type.
        = do { rdr_env <- getGlobalRdrEnv
             ; case checkNewtypeFFI rdr_env tc of
                 Nothing  -> nothing
                 Just gre -> do { (co', ty', gres) <- go role rec_nts' nt_rhs
                                ; return (mkTransCo nt_co co', ty', gre `consBag` gres) } }

        | isFamilyTyCon tc              -- Expand open tycons
        , (co, ty) <- normaliseTcApp env role tc tys
        , not (isReflexiveCo co)
        = do (co', ty', gres) <- go role rec_nts ty
             return (mkTransCo co co', ty', gres)

        | otherwise
        = nothing -- see Note [Don't recur in normaliseFfiType']
        where
          tc_key = getUnique tc
          children_only
            = do xs <- zipWithM (\ty r -> go r rec_nts ty) tys (tyConRolesX role tc)
                 let (cos, tys', gres) = unzip3 xs
                 return ( mkTyConAppCo role tc cos
                        , mkTyConApp tc tys', unionManyBags gres)
          nt_co  = mkUnbranchedAxInstCo role (newTyConCo tc) tys []
          nt_rhs = newTyConInstRhs tc tys

          ty      = mkTyConApp tc tys
          nothing = return (mkReflCo role ty, ty, emptyBag)

checkNewtypeFFI :: GlobalRdrEnv -> TyCon -> Maybe GlobalRdrElt
checkNewtypeFFI rdr_env tc
  | Just con <- tyConSingleDataCon_maybe tc
  , Just gre <- lookupGRE_Name rdr_env (dataConName con)
  = Just gre    -- See Note [Newtype constructor usage in foreign declarations]
  | otherwise
  = Nothing

{-
Note [Newtype constructor usage in foreign declarations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GHC automatically "unwraps" newtype constructors in foreign import/export
declarations.  In effect that means that a newtype data constructor is
used even though it is not mentioned expclitly in the source, so we don't
want to report it as "defined but not used" or "imported but not used".
eg     newtype D = MkD Int
       foreign import foo :: D -> IO ()
Here 'MkD' us used.  See #7408.

GHC also expands type functions during this process, so it's not enough
just to look at the free variables of the declaration.
eg     type instance F Bool = D
       foreign import bar :: F Bool -> IO ()
Here again 'MkD' is used.

So we really have wait until the type checker to decide what is used.
That's why tcForeignImports and tecForeignExports return a (Bag GRE)
for the newtype constructors they see. Then GHC.Tc.Module can add them
to the module's usages.


************************************************************************
*                                                                      *
\subsection{Imports}
*                                                                      *
************************************************************************
-}

tcForeignImports :: [LForeignDecl GhcRn]
                 -> TcM ([Id], [LForeignDecl GhcTc], Bag GlobalRdrElt)
tcForeignImports decls
  = getHooked tcForeignImportsHook tcForeignImports' >>= ($ decls)

tcForeignImports' :: [LForeignDecl GhcRn]
                  -> TcM ([Id], [LForeignDecl GhcTc], Bag GlobalRdrElt)
-- For the (Bag GlobalRdrElt) result,
-- see Note [Newtype constructor usage in foreign declarations]
tcForeignImports' decls
  = do { (ids, decls, gres) <- mapAndUnzip3M tcFImport $
                               filter isForeignImport decls
       ; return (ids, decls, unionManyBags gres) }

tcFImport :: LForeignDecl GhcRn
          -> TcM (Id, LForeignDecl GhcTc, Bag GlobalRdrElt)
tcFImport (L dloc fo@(ForeignImport { fd_name = L nloc nm, fd_sig_ty = hs_ty
                                    , fd_fi = imp_decl }))
  = setSrcSpanA dloc $ addErrCtxt (foreignDeclCtxt fo)  $
    do { sig_ty <- tcHsSigType (ForSigCtxt nm) hs_ty
       ; (norm_co, norm_sig_ty, gres) <- normaliseFfiType sig_ty
       ; let
           -- Drop the foralls before inspecting the
           -- structure of the foreign type.
             (arg_tys, res_ty) = tcSplitFunTys (dropForAlls norm_sig_ty)
             id                = mkLocalId nm Many sig_ty
                 -- Use a LocalId to obey the invariant that locally-defined
                 -- things are LocalIds.  However, it does not need zonking,
                 -- (so GHC.Tc.Utils.Zonk.zonkForeignExports ignores it).

       ; imp_decl' <- tcCheckFIType arg_tys res_ty imp_decl
          -- Can't use sig_ty here because sig_ty :: Type and
          -- we need HsType Id hence the undefined
       ; let fi_decl = ForeignImport { fd_name = L nloc id
                                     , fd_sig_ty = undefined
                                     , fd_i_ext = mkSymCo norm_co
                                     , fd_fi = imp_decl' }
       ; return (id, L dloc fi_decl, gres) }
tcFImport d = pprPanic "tcFImport" (ppr d)

-- ------------ Checking types for foreign import ----------------------

tcCheckFIType :: [Scaled Type] -> Type -> ForeignImport -> TcM ForeignImport

tcCheckFIType arg_tys res_ty (CImport (L lc cconv) safety mh l@(CLabel _) src)
  -- Foreign import label
  = do checkCg checkCOrAsmOrLlvmOrInterp
       -- NB check res_ty not sig_ty!
       --    In case sig_ty is (forall a. ForeignPtr a)
       check (isFFILabelTy (mkVisFunTys arg_tys res_ty)) (illegalForeignTyErr Outputable.empty)
       cconv' <- checkCConv cconv
       return (CImport (L lc cconv') safety mh l src)

tcCheckFIType arg_tys res_ty (CImport (L lc cconv) safety mh CWrapper src) = do
        -- Foreign wrapper (former f.e.d.)
        -- The type must be of the form ft -> IO (FunPtr ft), where ft is a valid
        -- foreign type.  For legacy reasons ft -> IO (Ptr ft) is accepted, too.
        -- The use of the latter form is DEPRECATED, though.
    checkCg checkCOrAsmOrLlvmOrInterp
    cconv' <- checkCConv cconv
    case arg_tys of
        [Scaled arg1_mult arg1_ty] -> do
                        checkNoLinearFFI arg1_mult
                        checkForeignArgs isFFIExternalTy arg1_tys
                        checkForeignRes nonIOok  checkSafe isFFIExportResultTy res1_ty
                        checkForeignRes mustBeIO checkSafe (isFFIDynTy arg1_ty) res_ty
                  where
                     (arg1_tys, res1_ty) = tcSplitFunTys arg1_ty
        _ -> addErrTc (illegalForeignTyErr Outputable.empty (text "One argument expected"))
    return (CImport (L lc cconv') safety mh CWrapper src)

tcCheckFIType arg_tys res_ty idecl@(CImport (L lc cconv) (L ls safety) mh
                                            (CFunction target) src)
  | isDynamicTarget target = do -- Foreign import dynamic
      checkCg checkCOrAsmOrLlvmOrInterp
      cconv' <- checkCConv cconv
      case arg_tys of           -- The first arg must be Ptr or FunPtr
        []                ->
          addErrTc (illegalForeignTyErr Outputable.empty (text "At least one argument expected"))
        (Scaled arg1_mult arg1_ty:arg_tys) -> do
          dflags <- getDynFlags
          let curried_res_ty = mkVisFunTys arg_tys res_ty
          checkNoLinearFFI arg1_mult
          check (isFFIDynTy curried_res_ty arg1_ty)
                (illegalForeignTyErr argument)
          checkForeignArgs (isFFIArgumentTy dflags safety) arg_tys
          checkForeignRes nonIOok checkSafe (isFFIImportResultTy dflags) res_ty
      return $ CImport (L lc cconv') (L ls safety) mh (CFunction target) src
  | cconv == PrimCallConv = do
      dflags <- getDynFlags
      checkTc (xopt LangExt.GHCForeignImportPrim dflags)
              (text "Use GHCForeignImportPrim to allow `foreign import prim'.")
      checkCg checkCOrAsmOrLlvmOrInterp
      checkCTarget target
      checkTc (playSafe safety)
              (text "The safe/unsafe annotation should not be used with `foreign import prim'.")
      checkForeignArgs (isFFIPrimArgumentTy dflags) arg_tys
      -- prim import result is more liberal, allows (#,,#)
      checkForeignRes nonIOok checkSafe (isFFIPrimResultTy dflags) res_ty
      return idecl
  | otherwise = do              -- Normal foreign import
      checkCg checkCOrAsmOrLlvmOrInterp
      cconv' <- checkCConv cconv
      checkCTarget target
      dflags <- getDynFlags
      checkForeignArgs (isFFIArgumentTy dflags safety) arg_tys
      checkForeignRes nonIOok checkSafe (isFFIImportResultTy dflags) res_ty
      checkMissingAmpersand dflags (map scaledThing arg_tys) res_ty
      case target of
          StaticTarget _ _ _ False
           | not (null arg_tys) ->
              addErrTc (text "`value' imports cannot have function types")
          _ -> return ()
      return $ CImport (L lc cconv') (L ls safety) mh (CFunction target) src


-- This makes a convenient place to check
-- that the C identifier is valid for C
checkCTarget :: CCallTarget -> TcM ()
checkCTarget (StaticTarget _ str _ _) = do
    checkCg checkCOrAsmOrLlvmOrInterp
    checkTc (isCLabelString str) (badCName str)

checkCTarget DynamicTarget = panic "checkCTarget DynamicTarget"


checkMissingAmpersand :: DynFlags -> [Type] -> Type -> TcM ()
checkMissingAmpersand dflags arg_tys res_ty
  | null arg_tys && isFunPtrTy res_ty &&
    wopt Opt_WarnDodgyForeignImports dflags
  = addWarn (Reason Opt_WarnDodgyForeignImports)
        (text "possible missing & in foreign import of FunPtr")
  | otherwise
  = return ()

{-
************************************************************************
*                                                                      *
\subsection{Exports}
*                                                                      *
************************************************************************
-}

tcForeignExports :: [LForeignDecl GhcRn]
             -> TcM (LHsBinds GhcTc, [LForeignDecl GhcTc], Bag GlobalRdrElt)
tcForeignExports decls =
  getHooked tcForeignExportsHook tcForeignExports' >>= ($ decls)

tcForeignExports' :: [LForeignDecl GhcRn]
             -> TcM (LHsBinds GhcTc, [LForeignDecl GhcTc], Bag GlobalRdrElt)
-- For the (Bag GlobalRdrElt) result,
-- see Note [Newtype constructor usage in foreign declarations]
tcForeignExports' decls
  = foldlM combine (emptyLHsBinds, [], emptyBag) (filter isForeignExport decls)
  where
   combine (binds, fs, gres1) (L loc fe) = do
       (b, f, gres2) <- setSrcSpanA loc (tcFExport fe)
       return (b `consBag` binds, L loc f : fs, gres1 `unionBags` gres2)

tcFExport :: ForeignDecl GhcRn
          -> TcM (LHsBind GhcTc, ForeignDecl GhcTc, Bag GlobalRdrElt)
tcFExport fo@(ForeignExport { fd_name = L loc nm, fd_sig_ty = hs_ty, fd_fe = spec })
  = addErrCtxt (foreignDeclCtxt fo) $ do

    sig_ty <- tcHsSigType (ForSigCtxt nm) hs_ty
    rhs <- tcCheckPolyExpr (nlHsVar nm) sig_ty

    (norm_co, norm_sig_ty, gres) <- normaliseFfiType sig_ty

    spec' <- tcCheckFEType norm_sig_ty spec

           -- we're exporting a function, but at a type possibly more
           -- constrained than its declared/inferred type. Hence the need
           -- to create a local binding which will call the exported function
           -- at a particular type (and, maybe, overloading).


    -- We need to give a name to the new top-level binding that
    -- is *stable* (i.e. the compiler won't change it later),
    -- because this name will be referred to by the C code stub.
    id  <- mkStableIdFromName nm sig_ty (locA loc) mkForeignExportOcc
    return ( mkVarBind id rhs
           , ForeignExport { fd_name = L loc id
                           , fd_sig_ty = undefined
                           , fd_e_ext = norm_co, fd_fe = spec' }
           , gres)
tcFExport d = pprPanic "tcFExport" (ppr d)

-- ------------ Checking argument types for foreign export ----------------------

tcCheckFEType :: Type -> ForeignExport -> TcM ForeignExport
tcCheckFEType sig_ty (CExport (L l (CExportStatic esrc str cconv)) src) = do
    checkCg checkCOrAsmOrLlvm
    checkTc (isCLabelString str) (badCName str)
    cconv' <- checkCConv cconv
    checkForeignArgs isFFIExternalTy arg_tys
    checkForeignRes nonIOok noCheckSafe isFFIExportResultTy res_ty
    return (CExport (L l (CExportStatic esrc str cconv')) src)
  where
      -- Drop the foralls before inspecting
      -- the structure of the foreign type.
    (arg_tys, res_ty) = tcSplitFunTys (dropForAlls sig_ty)

{-
************************************************************************
*                                                                      *
\subsection{Miscellaneous}
*                                                                      *
************************************************************************
-}

------------ Checking argument types for foreign import ----------------------
checkForeignArgs :: (Type -> Validity) -> [Scaled Type] -> TcM ()
checkForeignArgs pred tys = mapM_ go tys
  where
    go (Scaled mult ty) = checkNoLinearFFI mult >>
                          check (pred ty) (illegalForeignTyErr argument)

checkNoLinearFFI :: Mult -> TcM ()  -- No linear types in FFI (#18472)
checkNoLinearFFI Many = return ()
checkNoLinearFFI _    = addErrTc $ illegalForeignTyErr argument
                                   (text "Linear types are not supported in FFI declarations, see #18472")

------------ Checking result types for foreign calls ----------------------
-- | Check that the type has the form
--    (IO t) or (t) , and that t satisfies the given predicate.
-- When calling this function, any newtype wrappers (should) have been
-- already dealt with by normaliseFfiType.
--
-- We also check that the Safe Haskell condition of FFI imports having
-- results in the IO monad holds.
--
checkForeignRes :: Bool -> Bool -> (Type -> Validity) -> Type -> TcM ()
checkForeignRes non_io_result_ok check_safe pred_res_ty ty
  | Just (_, res_ty) <- tcSplitIOType_maybe ty
  =     -- Got an IO result type, that's always fine!
     check (pred_res_ty res_ty) (illegalForeignTyErr result)

  -- We disallow nested foralls in foreign types
  -- (at least, for the time being). See #16702.
  | tcIsForAllTy ty
  = addErrTc $ illegalForeignTyErr result (text "Unexpected nested forall")

  -- Case for non-IO result type with FFI Import
  | not non_io_result_ok
  = addErrTc $ illegalForeignTyErr result (text "IO result type expected")

  | otherwise
  = do { dflags <- getDynFlags
       ; case pred_res_ty ty of
                -- Handle normal typecheck fail, we want to handle this first and
                -- only report safe haskell errors if the normal type check is OK.
           NotValid msg -> addErrTc $ illegalForeignTyErr result msg

           -- handle safe infer fail
           _ | check_safe && safeInferOn dflags
               -> recordUnsafeInfer emptyBag

           -- handle safe language typecheck fail
           _ | check_safe && safeLanguageOn dflags
               -> addErrTc (illegalForeignTyErr result safeHsErr)

           -- success! non-IO return is fine
           _ -> return () }
  where
    safeHsErr =
      text "Safe Haskell is on, all FFI imports must be in the IO monad"

nonIOok, mustBeIO :: Bool
nonIOok  = True
mustBeIO = False

checkSafe, noCheckSafe :: Bool
checkSafe   = True
noCheckSafe = False

-- | Checking a supported backend is in use
checkCOrAsmOrLlvm :: Backend -> Validity
checkCOrAsmOrLlvm ViaC = IsValid
checkCOrAsmOrLlvm NCG  = IsValid
checkCOrAsmOrLlvm LLVM = IsValid
checkCOrAsmOrLlvm _
  = NotValid (text "requires unregisterised, llvm (-fllvm) or native code generation (-fasm)")

-- | Checking a supported backend is in use
checkCOrAsmOrLlvmOrInterp :: Backend -> Validity
checkCOrAsmOrLlvmOrInterp ViaC        = IsValid
checkCOrAsmOrLlvmOrInterp NCG         = IsValid
checkCOrAsmOrLlvmOrInterp LLVM        = IsValid
checkCOrAsmOrLlvmOrInterp Interpreter = IsValid
checkCOrAsmOrLlvmOrInterp _
  = NotValid (text "requires interpreted, unregisterised, llvm or native code generation")

checkCg :: (Backend -> Validity) -> TcM ()
checkCg check = do
    dflags <- getDynFlags
    let bcknd = backend dflags
    case bcknd of
      NoBackend -> return ()
      _ ->
        case check bcknd of
          IsValid      -> return ()
          NotValid err -> addErrTc (text "Illegal foreign declaration:" <+> err)

-- Calling conventions

checkCConv :: CCallConv -> TcM CCallConv
checkCConv CCallConv    = return CCallConv
checkCConv CApiConv     = return CApiConv
checkCConv StdCallConv  = do dflags <- getDynFlags
                             let platform = targetPlatform dflags
                             if platformArch platform == ArchX86
                                 then return StdCallConv
                                 else do -- This is a warning, not an error. see #3336
                                         when (wopt Opt_WarnUnsupportedCallingConventions dflags) $
                                             addWarnTc (Reason Opt_WarnUnsupportedCallingConventions)
                                                 (text "the 'stdcall' calling convention is unsupported on this platform," $$ text "treating as ccall")
                                         return CCallConv
checkCConv PrimCallConv = do addErrTc (text "The `prim' calling convention can only be used with `foreign import'")
                             return PrimCallConv
checkCConv JavaScriptCallConv = do dflags <- getDynFlags
                                   if platformArch (targetPlatform dflags) == ArchJavaScript
                                       then return JavaScriptCallConv
                                       else do addErrTc (text "The `javascript' calling convention is unsupported on this platform")
                                               return JavaScriptCallConv

-- Warnings

check :: Validity -> (MsgDoc -> MsgDoc) -> TcM ()
check IsValid _             = return ()
check (NotValid doc) err_fn = addErrTc (err_fn doc)

illegalForeignTyErr :: SDoc -> SDoc -> SDoc
illegalForeignTyErr arg_or_res extra
  = hang msg 2 extra
  where
    msg = hsep [ text "Unacceptable", arg_or_res
               , text "type in foreign declaration:"]

-- Used for 'arg_or_res' argument to illegalForeignTyErr
argument, result :: SDoc
argument = text "argument"
result   = text "result"

badCName :: CLabelString -> MsgDoc
badCName target
  = sep [quotes (ppr target) <+> text "is not a valid C identifier"]

foreignDeclCtxt :: ForeignDecl GhcRn -> SDoc
foreignDeclCtxt fo
  = hang (text "When checking declaration:")
       2 (ppr fo)
