module Main where

import GHC
import GHC.Core.InstEnv (instEnvElts, instanceHead)
import GHC.Core.Class (classMinimalDef)
import GHC.Core.TyCo.FVs (tyConsOfType)
import GHC.Driver.Ppr (showSDocForUser)
import GHC.Unit.State (lookupUnitId, lookupPackageName)
import GHC.Unit.Info (UnitInfo, unitExposedModules, unitId, PackageName(..))
import GHC.Unit.Types (UnitId)
import GHC.Data.FastString (fsLit)
import GHC.Driver.Env (hsc_units, hscEPS)
import GHC.Utils.Outputable
import GHC.Types.Unique.Set (nonDetEltsUniqSet)
import GHC.Types.TyThing (tyThingParent_maybe)
import GHC.Types.TyThing.Ppr (pprTyThing)
import GHC.Types.Name (nameOccName, nameModule_maybe, stableNameCmp)
import GHC.Types.Name.Occurrence (OccName, mkDataOcc, mkVarOcc, occNameString)
import GHC.Unit.External (eps_inst_env)
import GHC.Iface.Syntax (ShowSub(..), ShowHowMuch(..), AltPpr(..))
import GHC.Iface.Type (ShowForAllFlag(..))

import Data.Function (on)
import Data.List (sortBy, isPrefixOf)
import Control.Monad.IO.Class
import System.Environment (getArgs)
import Prelude hiding ((<>))

main :: IO ()
main = do
    ghcRoot:pkg_names <- getArgs
    mapM_ (run ghcRoot) pkg_names

run :: FilePath -> String -> IO ()
run root pkg_nm = runGhc (Just root) $ do
    let args = map noLoc
            [ "-package=" ++ pkg_nm
            , "-dppr-cols=1000"
            , "-fprint-explicit-runtime-reps"
            , "-fprint-explicit-foralls"
            , "-fsuppress-unit-ids"
            ]
    dflags <- do
        dflags <- getSessionDynFlags
        logger <- getLogger
        (dflags', _fileish_args, _dynamicFlagWarnings) <-
          GHC.parseDynamicFlags logger dflags args
        return dflags'

    _ <- setProgramDynFlags dflags
    unit_state <- hsc_units <$> getSession
    unit_id <- case lookupPackageName unit_state (PackageName $ fsLit pkg_nm) of
                    Just unit_id -> return unit_id
                    Nothing -> fail "failed to find package"
    unit_info <- case lookupUnitId unit_state unit_id of
      Just unit_info -> return unit_info
      Nothing -> fail "unknown package"

    decls_doc <- reportUnitDecls unit_info
    insts_doc <- reportInstances

    name_ppr_ctx <- GHC.getNamePprCtx
    let rendered = showSDocForUser dflags unit_state name_ppr_ctx (vcat [decls_doc, insts_doc])
    liftIO $ putStrLn rendered

ignoredModules :: [ModuleName]
ignoredModules =
    map mkModuleName $ concat
    [ unstableModules
    , platformDependentModules
    ]
  where
    unstableModules =
        [ "GHC.Conc.POSIX"
        , "GHC.Conc.IO"
        ]
    platformDependentModules =
        [ "System.Posix.Types"
        , "Foreign.C.Types"
        ]

ignoredOccNames :: [OccName]
ignoredOccNames =
    map mkDataOcc cTypeCons ++
    map mkVarOcc integerConversionIds
  where
    -- Data constructors from Foreign.C.Types whose RHSs are inherently platform-dependent
    cTypeCons =
        [ "CBool"
        , "CChar"
        , "CClock"
        , "CDouble"
        , "CFile"
        , "CFloat"
        , "CFpos"
        , "CInt"
        , "CIntMax"
        , "CIntPtr"
        , "CJmpBuf"
        , "CLLong"
        , "CLong"
        , "CPtrdiff"
        , "CSChar"
        , "CSUSeconds"
        , "CShort"
        , "CSigAtomic"
        , "CSize"
        , "CTime"
        , "CUChar"
        , "CUInt"
        , "CUIntMax"
        , "CUIntPtr"
        , "CULLong"
        , "CULong"
        , "CUSeconds"
        , "CUShort"
        , "CWchar"
        ]

    -- Conversion functions in GHC.Integer which are only exposed on 32-bit
    -- platforms
    integerConversionIds =
        [ "int64ToInteger"
        , "integerToInt64"
        , "integerToWord64"
        , "word64ToInteger"
        ]

ignoredOccName :: OccName -> Bool
ignoredOccName occ = occ `elem` ignoredOccNames

ignoredName :: Name -> Bool
ignoredName nm
  | ignoredOccName (getOccName nm)
  = True
  | Just md <- nameModule_maybe nm
  , moduleName md `elem` ignoredModules
  = True
  | otherwise
  = False

ignoredTyThing :: TyThing -> Bool
ignoredTyThing _ = False

ignoredTyCon :: TyCon -> Bool
ignoredTyCon = ignoredName . getName

ignoredType :: Type -> Bool
ignoredType = any ignoredTyCon . nonDetEltsUniqSet . tyConsOfType

ctupleInstance :: Name -> Bool
ctupleInstance name =
  isPrefixOf "CTuple" nameS
  ||
  nameS == "CUnit"
  ||
  nameS == "CSolo"
  where
    nameS = occNameString (getOccName name)

-- | Ignore instances whose heads mention ignored types.
ignoredInstance :: ClsInst -> Bool
ignoredInstance inst
  | ignoredName $ getName cls
  = True
  | any ignoredType tys
  = True
  | ctupleInstance $ getName cls
  = True
  | otherwise
  = False
  where
    (_, cls, tys) = instanceHead inst

reportUnitDecls :: UnitInfo -> Ghc SDoc
reportUnitDecls unit_info = do
    let exposed :: [ModuleName]
        exposed = map fst (unitExposedModules unit_info)
    vcat <$> mapM (reportModuleDecls $ unitId unit_info) exposed

reportModuleDecls :: UnitId -> ModuleName -> Ghc SDoc
reportModuleDecls unit_id modl_nm
  | modl_nm `elem` ignoredModules = do
      return $ vcat [ mod_header, text "-- ignored", text "" ]
  | otherwise = do
    modl <- GHC.lookupQualifiedModule (OtherPkg unit_id) modl_nm
    mb_mod_info <- GHC.getModuleInfo modl
    mod_info <- case mb_mod_info of
      Nothing -> fail "Failed to find module"
      Just mod_info -> return mod_info

    name_ppr_ctx <- mkNamePprCtxForModule modl mod_info
    let names = GHC.modInfoExports mod_info
        sorted_names = sortBy (compare `on` nameOccName) names

        exported_occs :: [OccName]
        exported_occs = map nameOccName names

        is_exported :: OccName -> Bool
        is_exported occ = occ `elem` exported_occs

        show_occ :: OccName -> Bool
        show_occ occ = is_exported occ && not (ignoredOccName occ)

    things <- mapM GHC.lookupName sorted_names
    let contents = vcat $
            [ text "-- Safety:" <+> ppr (modInfoSafe mod_info) ] ++
            [ pprTyThing ss thing $$ extras
            | Just thing <- things
            , case tyThingParent_maybe thing of
                Just parent
                  | is_exported (getOccName parent) -> False
                _ -> True
            , not $ ignoredTyThing thing
            , let ss = ShowSub { ss_how_much = ShowSome (Just show_occ) (AltPpr Nothing)
                               , ss_forall = ShowForAllMust
                               }
            , let extras = case thing of
                             ATyCon tycon
                               | Just cls <- tyConClass_maybe tycon
                               -> nest 2 (text "{-# MINIMAL" <+> ppr (classMinimalDef cls) <+> text "#-}")
                             _ -> empty
            ]

    return $ withUserStyle name_ppr_ctx AllTheWay $
        hang mod_header 2 contents <>
        text ""
  where
    mod_header = vcat
        [ text ""
        , text "module" <+> ppr modl_nm <+> text "where"
        , text ""
        ]

reportInstances :: Ghc SDoc
reportInstances = do
    hsc_env <- getSession
    eps <- liftIO $ hscEPS hsc_env
    let instances = eps_inst_env eps
    return $ vcat $
        [ text ""
        , text ""
        , text "-- Instances:"
        ] ++
        [ ppr inst
        | inst <- sortBy compareInstances (instEnvElts instances)
        , not $ ignoredInstance inst
        ]

compareInstances :: ClsInst -> ClsInst -> Ordering
compareInstances inst1 inst2 = mconcat
    [ stableNameCmp (getName cls1) (getName cls2)
    ]
  where
      (_, cls1, _tys1) = instanceHead inst1
      (_, cls2, _tys2) = instanceHead inst2
