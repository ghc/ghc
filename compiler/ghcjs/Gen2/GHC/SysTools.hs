-- partial copy

module Gen2.GHC.SysTools where

import Module
import Packages
import Config
import Panic
import Platform
import DynFlags

import Data.List (isPrefixOf, isInfixOf)

import System.FilePath
import System.Directory

import qualified SysTools as ST


linkDynLib :: DynFlags -> [String] -> [PackageId] -> IO ()
linkDynLib dflags0 o_files dep_packages
 = do
    let -- This is a rather ugly hack to fix dynamically linked
        -- GHC on Windows. If GHC is linked with -threaded, then
        -- it links against libHSrts_thr. But if base is linked
        -- against libHSrts, then both end up getting loaded,
        -- and things go wrong. We therefore link the libraries
        -- with the same RTS flags that we link GHC with.
        dflags1 = if cGhcThreaded then addWay' WayThreaded dflags0
                                  else                     dflags0
        dflags2 = if cGhcDebugged then addWay' WayDebug dflags1
                                  else                  dflags1
        dflags = updateWays dflags2

        verbFlags = getVerbFlags dflags
        o_file = outputFile dflags

    pkgs <- getPreloadPackagesAnd dflags dep_packages

    let pkg_lib_paths = collectLibraryPaths pkgs
    let pkg_lib_path_opts = concatMap get_pkg_lib_path_opts pkg_lib_paths
        get_pkg_lib_path_opts l
         | osElfTarget (platformOS (targetPlatform dflags)) &&
           dynLibLoader dflags == SystemDependent &&
           not (gopt Opt_Static dflags)
            = ["-L" ++ l, "-Wl,-rpath", "-Wl," ++ l]
         | otherwise = ["-L" ++ l]

    let lib_paths = libraryPaths dflags
    let lib_path_opts = map ("-L"++) lib_paths

    -- We don't want to link our dynamic libs against the RTS package,
    -- because the RTS lib comes in several flavours and we want to be
    -- able to pick the flavour when a binary is linked.
    -- On Windows we need to link the RTS import lib as Windows does
    -- not allow undefined symbols.
    -- The RTS library path is still added to the library search path
    -- above in case the RTS is being explicitly linked in (see #3807).
    let platform = targetPlatform dflags
        os = platformOS platform
        pkgs_no_rts = case os of
                      OSMinGW32 ->
                          pkgs
                      _ ->
                          filter ((/= rtsPackageId) . packageConfigId) pkgs
    let pkg_link_opts = let (package_hs_libs, extra_libs, other_flags) = ghcjsCollectLinkOpts dflags pkgs_no_rts
                        in  package_hs_libs ++ extra_libs ++ other_flags

        -- probably _stub.o files
    let extra_ld_inputs = ldInputs dflags
    case os of
        OSMinGW32 -> do
            -------------------------------------------------------------
            -- Making a DLL
            -------------------------------------------------------------
            let output_fn = case o_file of
                            Just s -> s
                            Nothing -> "HSdll.dll"

            ST.runLink dflags (
                    map Option verbFlags
                 ++ [ Option "-o"
                    , FileOption "" output_fn
                    , Option "-shared"
                    ] ++
                    [ FileOption "-Wl,--out-implib=" (output_fn ++ ".a")
                    | gopt Opt_SharedImplib dflags
                    ]
                 ++ map (FileOption "") o_files

                 -- Permit the linker to auto link _symbol to _imp_symbol
                 -- This lets us link against DLLs without needing an "import library"
                 ++ [Option "-Wl,--enable-auto-import"]

                 ++ extra_ld_inputs
                 ++ map Option (
                    lib_path_opts
                 ++ pkg_lib_path_opts
                 ++ pkg_link_opts
                ))
        OSDarwin -> do
            -------------------------------------------------------------------
            -- Making a darwin dylib
            -------------------------------------------------------------------
            -- About the options used for Darwin:
            -- -dynamiclib
            --   Apple's way of saying -shared
            -- -undefined dynamic_lookup:
            --   Without these options, we'd have to specify the correct
            --   dependencies for each of the dylibs. Note that we could
            --   (and should) do without this for all libraries except
            --   the RTS; all we need to do is to pass the correct
            --   HSfoo_dyn.dylib files to the link command.
            --   This feature requires Mac OS X 10.3 or later; there is
            --   a similar feature, -flat_namespace -undefined suppress,
            --   which works on earlier versions, but it has other
            --   disadvantages.
            -- -single_module
            --   Build the dynamic library as a single "module", i.e. no
            --   dynamic binding nonsense when referring to symbols from
            --   within the library. The NCG assumes that this option is
            --   specified (on i386, at least).
            -- -install_name
            --   Mac OS/X stores the path where a dynamic library is (to
            --   be) installed in the library itself.  It's called the
            --   "install name" of the library. Then any library or
            --   executable that links against it before it's installed
            --   will search for it in its ultimate install location.
            --   By default we set the install name to the absolute path
            --   at build time, but it can be overridden by the
            --   -dylib-install-name option passed to ghc. Cabal does
            --   this.
            -------------------------------------------------------------------

            let output_fn = case o_file of { Just s -> s; Nothing -> "a.out"; }

            instName <- case dylibInstallName dflags of
                Just n -> return n
                Nothing -> do
                    pwd <- getCurrentDirectory
                    return $ pwd `combine` output_fn
            ST.runLink dflags (
                    map Option verbFlags
                 ++ [ Option "-dynamiclib"
                    , Option "-o"
                    , FileOption "" output_fn
                    ]
                 ++ map Option o_files
                 ++ [ Option "-undefined",
                      Option "dynamic_lookup",
                      Option "-single_module" ]
                 ++ (if platformArch platform == ArchX86_64
                     then [ ]
                     else [ Option "-Wl,-read_only_relocs,suppress" ])
                 ++ [ Option "-install_name", Option instName ]
                 ++ map Option lib_path_opts
                 ++ extra_ld_inputs
                 ++ map Option pkg_lib_path_opts
                 ++ map Option pkg_link_opts
              )
        OSiOS -> throwGhcExceptionIO (ProgramError "dynamic libraries are not supported on iOS target")
        _ -> do
            -------------------------------------------------------------------
            -- Making a DSO
            -------------------------------------------------------------------

            let output_fn = case o_file of { Just s -> s; Nothing -> "a.out"; }
            let buildingRts = thisPackage dflags == rtsPackageId
            let bsymbolicFlag = if buildingRts
                                then -- -Bsymbolic breaks the way we implement
                                     -- hooks in the RTS
                                     []
                                else -- we need symbolic linking to resolve
                                     -- non-PIC intra-package-relocations
                                     ["-Wl,-Bsymbolic"]

            ST.runLink dflags (
                    map Option verbFlags
                 ++ [ Option "-o"
                    , FileOption "" output_fn
                    ]
                 ++ map Option o_files
                 ++ [ Option "-shared" ]
                 ++ map Option bsymbolicFlag
                    -- Set the library soname. We use -h rather than -soname as
                    -- Solaris 10 doesn't support the latter:
                 ++ [ Option ("-Wl,-h," ++ takeFileName output_fn) ]
                 ++ map Option lib_path_opts
                 ++ extra_ld_inputs
                 ++ map Option pkg_lib_path_opts
                 ++ map Option pkg_link_opts
              )

ghcjsCollectLinkOpts :: DynFlags -> [PackageConfig] -> ([String], [String], [String])
ghcjsCollectLinkOpts dflags ps =
    (
        concatMap (map ("-l" ++) . ghcjsPackageHsLibs dflags) ps,
        concatMap (map ("-l" ++) . extraLibraries) ps,
        concatMap ldOptions ps
    )

--------------------------------------------------
-- GHC API gets libs like libHSpackagename-ghcversion
-- change that to libHSpackagename-ghcjsversion_ghcversion
-- We need to do that in two places:
--    - Arguments for the system linker
--    - GHCi dynamic linker
ghcDynLibVersionTag :: String
ghcDynLibVersionTag   = "-ghc" ++ cProjectVersion

-- for now, Cabal installs libs with the GHC version
ghcjsDynLibVersionTag :: String
ghcjsDynLibVersionTag = "-ghcjs" ++ cProjectVersion

ghcjsPackageHsLibs :: DynFlags -> PackageConfig -> [String]
ghcjsPackageHsLibs dflags p = map fixLib (packageHsLibs dflags' p)
  where
    dflags' = dflags { ways = filter (not . isWayCustom) (ways dflags) }
    isWayCustom (WayCustom {}) = True
    isWayCustom _              = False
    fixLib lib | "HS" `isPrefixOf` lib &&
                 ghcDynLibVersionTag `isInfixOf` lib =
      replace ghcDynLibVersionTag ghcjsDynLibVersionTag lib
               | otherwise = lib

--------------------------------------------------------

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace from to xs = go xs
  where
    go [] = []
    go xxs@(x:xs)
      | from `isPrefixOf` xxs = to ++ go (drop (length from) xxs)
      | otherwise = x : go xs

