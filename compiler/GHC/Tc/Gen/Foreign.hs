{-
(c) The University of Glasgow 2006
(c) The AQUA Project, Glasgow University, 1998

-}


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

import GHC.Prelude

import GHC.Hs

import GHC.Tc.Errors.Types
import GHC.Tc.Utils.Monad
import GHC.Tc.Gen.HsType
import GHC.Tc.Gen.Expr
import GHC.Tc.Utils.Env
import GHC.Tc.Utils.TcType
import GHC.Tc.Instance.Family

import GHC.Core.FamInstEnv
import GHC.Core.Coercion
import GHC.Core.Reduction
import GHC.Core.Type
import GHC.Core.Multiplicity
import GHC.Core.DataCon
import GHC.Core.TyCon
import GHC.Core.TyCon.RecWalk

import GHC.Types.ForeignCall
import GHC.Types.Id
import GHC.Types.Name
import GHC.Types.Name.Reader
import GHC.Types.SrcLoc

import GHC.Builtin.Names
import GHC.Builtin.Types.Prim( isArrowTyCon )

import GHC.Driver.Session
import GHC.Driver.Backend

import GHC.Utils.Error
import GHC.Utils.Outputable as Outputable
import GHC.Utils.Panic
import GHC.Platform

import GHC.Data.Bag
import GHC.Driver.Hooks
import qualified GHC.LanguageExtensions as LangExt

import Control.Monad ( zipWithM )
import Control.Monad.Trans.Writer.CPS
  ( WriterT, runWriterT, tell )
import Control.Monad.Trans.Class
  ( lift )

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
need to recur on any type parameters, because no parameterized types (with
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
normaliseFfiType :: Type -> TcM (Reduction, Bag GlobalRdrElt)
normaliseFfiType ty
    = do fam_envs <- tcGetFamInstEnvs
         normaliseFfiType' fam_envs ty

normaliseFfiType' :: FamInstEnvs -> Type -> TcM (Reduction, Bag GlobalRdrElt)
normaliseFfiType' env ty0 = runWriterT $ go Representational initRecTc ty0
  where
    go :: Role -> RecTcChecker -> Type -> WriterT (Bag GlobalRdrElt) TcM Reduction
    go role rec_nts ty
      | Just ty' <- coreView ty     -- Expand synonyms
      = go role rec_nts ty'

      | Just (tc, tys) <- splitTyConApp_maybe ty
      = go_tc_app role rec_nts tc tys

      | (bndrs, inner_ty) <- splitForAllForAllTyBinders ty
      , not (null bndrs)
      = do redn <- go role rec_nts inner_ty
           return $ mkHomoForAllRedn bndrs redn

      | otherwise -- see Note [Don't recur in normaliseFfiType']
      = return $ mkReflRedn role ty

    go_tc_app :: Role -> RecTcChecker -> TyCon -> [Type]
              -> WriterT (Bag GlobalRdrElt) TcM Reduction
    go_tc_app role rec_nts tc tys
        | isArrowTyCon tc  -- Recurse through arrows, or at least the top
        = children_only    -- level arrows.  Remember, the default case is
                           -- "don't recurse" (see last eqn for go_tc_app)

        | tc_key `elem` [ioTyConKey, funPtrTyConKey]
        -- We don't want to look through the IO newtype, even if it is
        -- in scope, so we have a special case for it:
        = children_only

        | isNewTyCon tc         -- Expand newtypes
        , Just rec_nts' <- checkRecTc rec_nts tc
                   -- See Note [Expanding newtypes and products] in GHC.Core.TyCon.RecWalk
                   -- We can't just use isRecursiveTyCon; sometimes recursion is ok:
                   --     newtype T = T (Ptr T)
                   --   Here, we don't reject the type for being recursive.
                   -- If this is a recursive newtype then it will normally
                   -- be rejected later as not being a valid FFI type.
        = do { rdr_env <- lift $ getGlobalRdrEnv
             ; case checkNewtypeFFI rdr_env tc of
                 Nothing  -> nothing
                 Just gre ->
                   do { redn <- go role rec_nts' nt_rhs
                      ; tell (unitBag gre)
                      ; return $ nt_co `mkTransRedn` redn } }

        | isFamilyTyCon tc              -- Expand open tycons
        , Reduction co ty <- normaliseTcApp env role tc tys
        , not (isReflexiveCo co)
        = do redn <- go role rec_nts ty
             return $ co `mkTransRedn` redn

        | otherwise
        = nothing -- see Note [Don't recur in normaliseFfiType']
        where
          tc_key = getUnique tc
          children_only
            = do { args <- unzipRedns <$>
                            zipWithM ( \ ty r -> go r rec_nts ty )
                                     tys (tyConRoleListX role tc)
                 ; return $ mkTyConAppRedn role tc args }
          nt_co  = mkUnbranchedAxInstCo role (newTyConCo tc) tys []
          nt_rhs = newTyConInstRhs tc tys

          ty      = mkTyConApp tc tys
          nothing = return $ mkReflRedn role ty

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
Here 'MkD' is used.  See #7408.

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
tcForeignImports decls = do
    hooks <- getHooks
    case tcForeignImportsHook hooks of
        Nothing -> tcForeignImports' decls
        Just h  -> h decls

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
       ; (Reduction norm_co norm_sig_ty, gres) <- normaliseFfiType sig_ty
       ; let
             -- Drop the foralls before inspecting the
             -- structure of the foreign type.
             -- Use splitFunTys, which splits (=>) as well as (->)
             -- so that for  foreign import foo :: Eq a => a -> blah
             -- we get "unacceptable argument Eq a" rather than
             --        "unacceptable result Eq a => a -> blah"
             -- Not a big deal.  We could make a better error message specially
             -- for overloaded functions, but doesn't seem worth it
             (arg_tys, res_ty) = splitFunTys (dropForAlls norm_sig_ty)

             id = mkLocalId nm ManyTy sig_ty
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

tcCheckFIType :: [Scaled Type] -> Type -> ForeignImport GhcRn -> TcM (ForeignImport GhcTc)

tcCheckFIType arg_tys res_ty idecl@(CImport src (L lc cconv) safety mh l@(CLabel _))
  -- Foreign import label
  = do checkCg (Right idecl) backendValidityOfCImport
       -- NB check res_ty not sig_ty!
       --    In case sig_ty is (forall a. ForeignPtr a)
       check (isFFILabelTy (mkScaledFunTys arg_tys res_ty))
             (TcRnIllegalForeignType Nothing)
       cconv' <- checkCConv (Right idecl) cconv
       return (CImport src (L lc cconv') safety mh l)

tcCheckFIType arg_tys res_ty idecl@(CImport src (L lc cconv) safety mh CWrapper) = do
        -- Foreign wrapper (former f.e.d.)
        -- The type must be of the form ft -> IO (FunPtr ft), where ft is a valid
        -- foreign type.  For legacy reasons ft -> IO (Ptr ft) is accepted, too.
        -- The use of the latter form is DEPRECATED, though.
    checkCg (Right idecl) backendValidityOfCImport
    cconv' <- checkCConv (Right idecl) cconv
    case arg_tys of
        [Scaled arg1_mult arg1_ty] -> do
                        checkNoLinearFFI arg1_mult
                        checkForeignArgs isFFIExternalTy arg1_tys
                        checkForeignRes nonIOok  checkSafe isFFIExportResultTy res1_ty
                        checkForeignRes mustBeIO checkSafe (isFFIDynTy arg1_ty) res_ty
                  where
                     (arg1_tys, res1_ty) = tcSplitFunTys arg1_ty
        _ -> addErrTc (TcRnIllegalForeignType Nothing OneArgExpected)
    return (CImport src (L lc cconv') safety mh CWrapper)

tcCheckFIType arg_tys res_ty idecl@(CImport src (L lc cconv) (L ls safety) mh
                                            (CFunction target))
  | isDynamicTarget target = do -- Foreign import dynamic
      checkCg (Right idecl) backendValidityOfCImport
      cconv' <- checkCConv (Right idecl) cconv
      case arg_tys of           -- The first arg must be Ptr or FunPtr
        []                ->
          addErrTc (TcRnIllegalForeignType Nothing AtLeastOneArgExpected)
        (Scaled arg1_mult arg1_ty:arg_tys) -> do
          dflags <- getDynFlags
          let curried_res_ty = mkScaledFunTys arg_tys res_ty
          checkNoLinearFFI arg1_mult
          check (isFFIDynTy curried_res_ty arg1_ty)
                (TcRnIllegalForeignType (Just Arg))
          checkForeignArgs (isFFIArgumentTy dflags safety) arg_tys
          checkForeignRes nonIOok checkSafe (isFFIImportResultTy dflags) res_ty
      return $ CImport src (L lc cconv') (L ls safety) mh (CFunction target)
  | cconv == PrimCallConv = do
      dflags <- getDynFlags
      checkTc (xopt LangExt.GHCForeignImportPrim dflags)
              (TcRnForeignImportPrimExtNotSet idecl)
      checkCg (Right idecl) backendValidityOfCImport
      checkCTarget idecl target
      checkTc (playSafe safety)
              (TcRnForeignImportPrimSafeAnn idecl)
      checkForeignArgs (isFFIPrimArgumentTy dflags) arg_tys
      -- prim import result is more liberal, allows (#,,#)
      checkForeignRes nonIOok checkSafe (isFFIPrimResultTy dflags) res_ty
      return (CImport src (L lc cconv) (L ls safety) mh (CFunction target))
  | cconv == JavaScriptCallConv = do
      cconv' <- checkCConv (Right idecl) cconv
      checkCg (Right idecl) backendValidityOfCImport
      -- leave the rest to the JS backend (at least for now)
      return (CImport src (L lc cconv') (L ls safety) mh (CFunction target))
  | otherwise = do              -- Normal foreign import
      checkCg (Right idecl) backendValidityOfCImport
      cconv' <- checkCConv (Right idecl) cconv
      checkCTarget idecl target
      dflags <- getDynFlags
      checkForeignArgs (isFFIArgumentTy dflags safety) arg_tys
      checkForeignRes nonIOok checkSafe (isFFIImportResultTy dflags) res_ty
      checkMissingAmpersand idecl (map scaledThing arg_tys) res_ty
      case target of
          StaticTarget _ _ _ False
           | not (null arg_tys) ->
              addErrTc (TcRnForeignFunctionImportAsValue idecl)
          _ -> return ()
      return $ CImport src (L lc cconv') (L ls safety) mh (CFunction target)

-- This makes a convenient place to check
-- that the C identifier is valid for C
checkCTarget :: ForeignImport GhcRn -> CCallTarget -> TcM ()
checkCTarget idecl (StaticTarget _ str _ _) = do
    checkCg (Right idecl) backendValidityOfCImport
    checkTc (isCLabelString str) (TcRnInvalidCIdentifier str)

checkCTarget _ DynamicTarget = panic "checkCTarget DynamicTarget"

checkMissingAmpersand :: ForeignImport GhcRn -> [Type] -> Type -> TcM ()
checkMissingAmpersand idecl arg_tys res_ty
  | null arg_tys && isFunPtrTy res_ty
  = addDiagnosticTc $ TcRnFunPtrImportWithoutAmpersand idecl
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
tcForeignExports decls = do
    hooks <- getHooks
    case tcForeignExportsHook hooks of
        Nothing -> tcForeignExports' decls
        Just h  -> h decls

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

    (Reduction norm_co norm_sig_ty, gres) <- normaliseFfiType sig_ty

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
                           , fd_e_ext = norm_co
                           , fd_fe = spec' }
           , gres)
tcFExport d = pprPanic "tcFExport" (ppr d)

-- ------------ Checking argument types for foreign export ----------------------

tcCheckFEType :: Type -> ForeignExport GhcRn -> TcM (ForeignExport GhcTc)
tcCheckFEType sig_ty edecl@(CExport src (L l (CExportStatic esrc str cconv))) = do
    checkCg (Left edecl) backendValidityOfCExport
    checkTc (isCLabelString str) (TcRnInvalidCIdentifier str)
    cconv' <- checkCConv (Left edecl) cconv
    checkForeignArgs isFFIExternalTy arg_tys
    checkForeignRes nonIOok noCheckSafe isFFIExportResultTy res_ty
    return (CExport src (L l (CExportStatic esrc str cconv')))
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
checkForeignArgs :: (Type -> Validity' IllegalForeignTypeReason) -> [Scaled Type] -> TcM ()
checkForeignArgs pred tys = mapM_ go tys
  where
    go (Scaled mult ty) = checkNoLinearFFI mult >>
                          check (pred ty) (TcRnIllegalForeignType (Just Arg))

checkNoLinearFFI :: Mult -> TcM ()  -- No linear types in FFI (#18472)
checkNoLinearFFI ManyTy = return ()
checkNoLinearFFI _      = addErrTc $ TcRnIllegalForeignType (Just Arg)
                                   LinearTypesNotAllowed

------------ Checking result types for foreign calls ----------------------
-- | Check that the type has the form
--    (IO t) or (t) , and that t satisfies the given predicate.
-- When calling this function, any newtype wrappers (should) have been
-- already dealt with by normaliseFfiType.
--
-- We also check that the Safe Haskell condition of FFI imports having
-- results in the IO monad holds.
--
checkForeignRes :: Bool -> Bool -> (Type -> Validity' IllegalForeignTypeReason) -> Type -> TcM ()
checkForeignRes non_io_result_ok check_safe pred_res_ty ty
  | Just (_, res_ty) <- tcSplitIOType_maybe ty
  =     -- Got an IO result type, that's always fine!
     check (pred_res_ty res_ty)
           (TcRnIllegalForeignType (Just Result))

  -- We disallow nested foralls in foreign types
  -- (at least, for the time being). See #16702.
  | isForAllTy ty
  = addErrTc $ TcRnIllegalForeignType (Just Result) UnexpectedNestedForall

  -- Case for non-IO result type with FFI Import
  | not non_io_result_ok
  = addErrTc $ TcRnIllegalForeignType (Just Result) IOResultExpected

  | otherwise
  = do { dflags <- getDynFlags
       ; case pred_res_ty ty of
                -- Handle normal typecheck fail, we want to handle this first and
                -- only report safe haskell errors if the normal type check is OK.
           NotValid msg -> addErrTc $ TcRnIllegalForeignType (Just Result) msg

           -- handle safe infer fail
           _ | check_safe && safeInferOn dflags
               -> recordUnsafeInfer emptyMessages

           -- handle safe language typecheck fail
           _ | check_safe && safeLanguageOn dflags
               -> addErrTc (TcRnIllegalForeignType (Just Result) SafeHaskellMustBeInIO)

           -- success! non-IO return is fine
           _ -> return () }

nonIOok, mustBeIO :: Bool
nonIOok  = True
mustBeIO = False

checkSafe, noCheckSafe :: Bool
checkSafe   = True
noCheckSafe = False

checkCg :: Either (ForeignExport GhcRn) (ForeignImport GhcRn)
        -> (Backend -> Validity' ExpectedBackends) -> TcM ()
checkCg decl check = do
    dflags <- getDynFlags
    let bcknd = backend dflags
    case check bcknd of
      IsValid -> return ()
      NotValid expectedBcknds ->
        addErrTc $ TcRnIllegalForeignDeclBackend decl bcknd expectedBcknds

-- Calling conventions

checkCConv :: Either (ForeignExport GhcRn) (ForeignImport GhcRn)
           -> CCallConv -> TcM CCallConv
checkCConv _ CCallConv    = return CCallConv
checkCConv _ CApiConv     = return CApiConv
checkCConv decl StdCallConv = do
  dflags <- getDynFlags
  let platform = targetPlatform dflags
  if platformArch platform == ArchX86
      then return StdCallConv
      else do -- This is a warning, not an error. see #3336
              let msg = TcRnUnsupportedCallConv decl StdCallConvUnsupported
              addDiagnosticTc msg
              return CCallConv
checkCConv decl PrimCallConv = do
  addErrTc $ TcRnUnsupportedCallConv decl PrimCallConvUnsupported
  return PrimCallConv
checkCConv decl JavaScriptCallConv = do
  dflags <- getDynFlags
  if platformArch (targetPlatform dflags) == ArchJavaScript
      then return JavaScriptCallConv
      else do
        addErrTc $ TcRnUnsupportedCallConv decl JavaScriptCallConvUnsupported
        return JavaScriptCallConv

-- Warnings

check :: Validity' IllegalForeignTypeReason
      -> (IllegalForeignTypeReason -> TcRnMessage)
      -> TcM ()
check IsValid _                   = return ()
check (NotValid reason) mkMessage = addErrTc (mkMessage reason)

foreignDeclCtxt :: ForeignDecl GhcRn -> SDoc
foreignDeclCtxt fo
  = hang (text "When checking declaration:")
       2 (ppr fo)
