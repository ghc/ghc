module GHC.Iface.Errors
  ( badIfaceFile
  , cannotFindInterface
  , cantFindInstalledErr
  , cannotFindModule
  ) where

import GHC.Platform.Profile
import GHC.Platform.Ways
import GHC.Utils.Panic.Plain
import GHC.Driver.DynFlags
import GHC.Driver.Env
import GHC.Data.Maybe
import GHC.Data.OsPath
import GHC.Prelude
import GHC.Unit
import GHC.Unit.Env
import GHC.Unit.Finder.Types
import GHC.Utils.Outputable as Outputable
import GHC.Iface.Errors.Types

-- -----------------------------------------------------------------------------
-- Error messages

badIfaceFile :: String -> SDoc -> SDoc
badIfaceFile file err
  = vcat [text "Bad interface file:" <+> text file,
          nest 4 err]

cannotFindInterface :: UnitIndex -> Maybe HomeUnit -> Profile
                    -> ModuleName -> InstalledFindResult -> MissingInterfaceError
cannotFindInterface ui mhu p mn ifr =
  CantFindErr ui FindingInterface $
  cantFindInstalledErr ui mhu p mn ifr

cantFindInstalledErr
    :: UnitIndex
    -> Maybe HomeUnit
    -> Profile
    -> ModuleName
    -> InstalledFindResult
    -> CantFindInstalled
cantFindInstalledErr unit_index mhome_unit profile mod_name find_result
  = CantFindInstalled mod_name more_info
  where
    build_tag  = waysBuildTag (profileWays profile)

    more_info
      = case find_result of
            InstalledNoPackage pkg
                -> NoUnitIdMatching pkg (searchPackageId unit_index (PackageId (unitIdFS pkg)))

            InstalledNotFound files mb_pkg
                | Just pkg <- mb_pkg
                , notHomeUnitId mhome_unit pkg
                -> not_found_in_package pkg $ fmap unsafeDecodeUtf files

                | null files
                -> NotAModule

                | otherwise
                -> CouldntFindInFiles $ fmap unsafeDecodeUtf files

            _ -> panic "cantFindInstalledErr"

    not_found_in_package pkg files
       | build_tag /= ""
       = let
            build = if build_tag == "p" then "profiling"
                                        else "\"" ++ build_tag ++ "\""
         in
         MissingPackageWayFiles build pkg files
       | otherwise
       = MissingPackageFiles pkg files



cannotFindModule :: HscEnv -> UnitIndex -> ModuleName -> FindResult -> MissingInterfaceError
cannotFindModule hsc_env unit_index = cannotFindModule'
    (hsc_unit_env hsc_env)
    unit_index
    (targetProfile (hsc_dflags hsc_env))


cannotFindModule' :: UnitEnv -> UnitIndex -> Profile -> ModuleName -> FindResult
                  -> MissingInterfaceError
cannotFindModule' unit_env unit_index profile mod res =
  CantFindErr unit_index FindingModule $
  cantFindErr unit_env
              unit_index
              profile
              mod
              res

cantFindErr
    :: UnitEnv
    -> UnitIndex
    -> Profile
    -> ModuleName
    -> FindResult
    -> CantFindInstalled
cantFindErr _ _ _ mod_name (FoundMultiple mods)
  = CantFindInstalled mod_name (MultiplePackages mods)

cantFindErr unit_env unit_index profile mod_name find_result
  = CantFindInstalled mod_name more_info
  where
    mhome_unit = ue_homeUnit unit_env
    more_info
      = case find_result of
            NoPackage pkg
                -> NoUnitIdMatching (toUnitId pkg) []
            NotFound { fr_paths = files, fr_pkg = mb_pkg
                     , fr_mods_hidden = mod_hiddens, fr_pkgs_hidden = pkg_hiddens
                     , fr_unusables = unusables, fr_suggestions = suggest }
                | Just pkg <- mb_pkg
                , Nothing <- mhome_unit           -- no home-unit
                -> not_found_in_package (toUnitId pkg) files

                | Just pkg <- mb_pkg
                , Just home_unit <- mhome_unit    -- there is a home-unit but the
                , not (isHomeUnit home_unit pkg)  -- module isn't from it
                -> not_found_in_package (toUnitId pkg) files

                | not (null suggest)
                -> ModuleSuggestion suggest files

                | null files && null mod_hiddens &&
                  null pkg_hiddens && null unusables
                -> NotAModule

                | otherwise
                -> GenericMissing
                    (map ((\uid -> (uid, lookupUnit unit_index (ue_homeUnitState unit_env) uid))) pkg_hiddens)
                    mod_hiddens unusables files
            _ -> panic "cantFindErr"

    build_tag = waysBuildTag (profileWays profile)

    not_found_in_package pkg files
       | build_tag /= ""
       = let
            build = if build_tag == "p" then "profiling"
                                        else "\"" ++ build_tag ++ "\""
         in
         MissingPackageWayFiles build pkg files

       | otherwise
       = MissingPackageFiles pkg files
