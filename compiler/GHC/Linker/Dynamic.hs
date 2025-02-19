{-# LANGUAGE CPP #-}

-- | Dynamic linker
module GHC.Linker.Dynamic
   ( linkDynLib
   -- * Platform-specifics
   , libmLinkOpts
   )
where

import GHC.Prelude
import GHC.Platform
import GHC.Platform.Ways
import GHC.Settings (ToolSettings(toolSettings_ldSupportsSingleModule))

import GHC.Driver.Config.Linker
import GHC.Driver.Session

import GHC.Unit.Env
import GHC.Unit.Types
import GHC.Unit.State
import GHC.Linker.MacOS
import GHC.Linker.Unit
import GHC.Linker.External
import GHC.Utils.Logger
import GHC.Utils.TmpFs
import GHC.Data.FastString

import Control.Monad (when)
import System.FilePath

linkDynLib :: Logger -> TmpFs -> DynFlags -> UnitEnv -> [String] -> [UnitId] -> IO ()
linkDynLib logger tmpfs dflags0 unit_env o_files dep_packages
 = do
    let platform   = ue_platform unit_env
        arch       = platformArch platform
        os         = platformOS platform

        -- This is a rather ugly hack to fix dynamically linked
        -- GHC on Windows. If GHC is linked with -threaded, then
        -- it links against libHSrts_thr. But if base is linked
        -- against libHSrts, then both end up getting loaded,
        -- and things go wrong. We therefore link the libraries
        -- with the same RTS flags that we link GHC with.
        dflags | OSMinGW32 <- os
               , hostWays `hasWay` WayDyn
               = dflags0 { targetWays_ = hostWays }
               | otherwise
               = dflags0

        verbFlags = getVerbFlags dflags
        o_file = outputFile_ dflags

    pkgs_with_rts <- mayThrowUnitErr (preloadUnitsInfo' unit_env dep_packages)

    let pkg_lib_paths = collectLibraryDirs (ways dflags) pkgs_with_rts
    let pkg_lib_path_opts = concatMap get_pkg_lib_path_opts pkg_lib_paths
        get_pkg_lib_path_opts l
         | osElfTarget os || osMachOTarget os
         , dynLibLoader dflags == SystemDependent
         , -- Only if we want dynamic libraries
           ways dflags `hasWay` WayDyn
           -- Only use RPath if we explicitly asked for it
         , useXLinkerRPath dflags os
            = ["-L" ++ l, "-Xlinker", "-rpath", "-Xlinker", l]
              -- See Note [-Xlinker -rpath vs -Wl,-rpath]
         | otherwise = ["-L" ++ l]

    let lib_paths = libraryPaths dflags
    let lib_path_opts = map ("-L"++) lib_paths

    -- In general we don't want to link our dynamic libs against the RTS
    -- package, because the RTS lib comes in several flavours and we want to be
    -- able to pick the flavour when a binary is linked.
    --
    -- But:
    --   * on Windows we need to link the RTS import lib as Windows does not
    --   allow undefined symbols.
    --
    --   * the RTS library path is still added to the library search path above
    --   in case the RTS is being explicitly linked in (see #3807).
    --
    --   * if -flink-rts is used, we link with the rts.
    --
    --   * on wasm we need to ensure libHSrts*.so is listed in
    --   WASM_DYLINK_NEEDED, otherwise dyld can't load it.
    --
    --
    let pkgs_without_rts = filter ((/= rtsUnitId) . unitId) pkgs_with_rts
        pkgs
         | ArchWasm32 <- arch      = pkgs_with_rts
         | OSMinGW32 <- os         = pkgs_with_rts
         | gopt Opt_LinkRts dflags = pkgs_with_rts
         | otherwise               = pkgs_without_rts
        pkg_link_opts = hsLibs unit_link_opts ++ extraLibs unit_link_opts ++ otherFlags unit_link_opts
          where
            namever = ghcNameVersion dflags
            ways_   = ways dflags
            unit_link_opts = collectLinkOpts namever ways_ pkgs

        -- probably _stub.o files
        -- and last temporary shared object file
    let extra_ld_inputs = ldInputs dflags

    -- frameworks
    pkg_framework_opts <- getUnitFrameworkOpts unit_env (map unitId pkgs)
    let framework_opts = getFrameworkOpts (initFrameworkOpts dflags) platform

    let require_cxx = any ((==) (PackageName (fsLit "system-cxx-std-lib")) . unitPackageName) pkgs
    let linker_config = initLinkerConfig dflags require_cxx

    case os of
        OSMinGW32 -> do
            -------------------------------------------------------------
            -- Making a DLL
            -------------------------------------------------------------
            let output_fn = case o_file of
                            Just s -> s
                            Nothing -> "HSdll.dll"

            runLink logger tmpfs linker_config (
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
        _ | os == OSDarwin -> do
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
            --   In XCode 15, -single_module is the default and passing the
            --   flag is now obsolete and raises a warning (#24168). We encode
            --   this information into the toolchain field ...SupportsSingleModule.
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
                Nothing -> return $ "@rpath" `combine` (takeFileName output_fn)
            runLink logger tmpfs linker_config (
                    map Option verbFlags
                 ++ [ Option "-dynamiclib"
                    , Option "-o"
                    , FileOption "" output_fn
                    ]
                 ++ map Option o_files
                 ++ [ Option "-undefined",
                      Option "dynamic_lookup"
                    ]
                 ++ (if toolSettings_ldSupportsSingleModule (toolSettings dflags)
                        then [ Option "-single_module" ]
                        else [ ])
                 ++ (if platformArch platform `elem` [ ArchX86_64, ArchAArch64 ]
                     then [ ]
                     else [ Option "-Wl,-read_only_relocs,suppress" ])
                 ++ [ Option "-install_name", Option instName ]
                 ++ map Option lib_path_opts
                 ++ extra_ld_inputs
                 ++ map Option framework_opts
                 ++ map Option pkg_lib_path_opts
                 ++ map Option pkg_link_opts
                 ++ map Option pkg_framework_opts
                 -- dead_strip_dylibs, will remove unused dylibs, and thus save
                 -- space in the load commands. The -headerpad is necessary so
                 -- that we can inject more @rpath's later for the leftover
                 -- libraries in the runInjectRpaths phase below.
                 --
                 -- See Note [Dynamic linking on macOS]
                 ++ [ Option "-Wl,-dead_strip_dylibs", Option "-Wl,-headerpad,8000" ]
              )
            -- Make sure to honour -fno-use-rpaths if set on darwin as well; see #20004
            when (gopt Opt_RPath dflags) $
              runInjectRPaths logger (toolSettings dflags) pkg_lib_paths output_fn
        _ -> do
            -------------------------------------------------------------------
            -- Making a DSO
            -------------------------------------------------------------------

            let output_fn = case o_file of { Just s -> s; Nothing -> "a.out"; }
                platform  = targetPlatform dflags
                unregisterised = platformUnregisterised platform
            let bsymbolicFlag = -- we need symbolic linking to resolve
                                -- non-PIC intra-package-relocations for
                                -- performance (where symbolic linking works)
                                -- See Note [-Bsymbolic assumptions by GHC]
                                -- wasm-ld accepts --Bsymbolic instead
                                ["-Wl,-Bsymbolic" | not unregisterised && arch /= ArchWasm32 ]

            runLink logger tmpfs linker_config (
                    map Option verbFlags
                 ++ libmLinkOpts platform
                 ++ [ Option "-o"
                    , FileOption "" output_fn
                    ]
                 ++ map Option o_files
                 ++ [ Option "-shared" ]
                 ++ map Option bsymbolicFlag
                    -- Set the library soname. We use -h rather than -soname as
                    -- Solaris 10 doesn't support the latter:
                    -- wasm-ld only accepts -soname and it's of little use anyway
                 ++ [ Option ("-Wl,-h," ++ takeFileName output_fn) | arch /= ArchWasm32 ]
                    -- 1. On wasm, --Bsymbolic is an optimization, not
                    --    a requirement. We build the wasi-sdk sysroot
                    --    shared libs as well as all Haskell shared
                    --    libs with --Bsymbolic, but dyld can handle
                    --    shared libs without --Bsymbolic at
                    --    link-time. Though there will be more
                    --    imports/exports to slow things down.
                    -- 2. --experimental-pic silences wasm-ld warnings
                    --    that PIC is experimental.
                    -- 3. --unresolved-symbols=import-dynamic turns
                    --    unresolved symbols to GOT.mem/GOT.func/env
                    --    imports, which can be gracefully handled by
                    --    dyld as lazy bindings. Ideally we'd only
                    --    enable this for rts since it forward
                    --    references ghc-prim/ghc-internal, but too
                    --    many Haskell packages would be rejected at
                    --    link-time even if their code refers to
                    --    something that will not be called at
                    --    run-time in wasm, so enabling it in the
                    --    driver is a more pragmatic solution.
                 ++ [ Option "-Wl,--Bsymbolic,--experimental-pic,--unresolved-symbols=import-dynamic" | arch == ArchWasm32 ]
                 ++ extra_ld_inputs
                 ++ map Option lib_path_opts
                 ++ map Option pkg_lib_path_opts
                 ++ map Option pkg_link_opts
              )

-- | Some platforms require that we explicitly link against @libm@ if any
-- math-y things are used (which we assume to include all programs). See #14022.
libmLinkOpts :: Platform -> [Option]
libmLinkOpts platform
  | platformHasLibm platform = [Option "-lm"]
  | otherwise                = []

{-
Note [-Bsymbolic assumptions by GHC]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

GHC has a few assumptions about interaction of relocations in NCG and linker:

1. -Bsymbolic resolves internal references when the shared library is linked,
   which is important for performance.
2. When there is a reference to data in a shared library from the main program,
   the runtime linker relocates the data object into the main program using an
   R_*_COPY relocation.
3. If we used -Bsymbolic, then this results in multiple copies of the data
   object, because some references have already been resolved to point to the
   original instance. This is bad!

We work around [3.] for native compiled code by avoiding the generation of
R_*_COPY relocations.

Unregisterised compiler can't evade R_*_COPY relocations easily thus we disable
-Bsymbolic linking there.

See related tickets: #4210, #15338
-}
