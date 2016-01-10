module Settings.Builders.Ghc (ghcBuilderArgs, ghcMBuilderArgs, commonGhcArgs) where

import Base
import Expression
import GHC
import Oracles
import Predicates hiding (way, stage)
import Settings
import Settings.Builders.GhcCabal (bootPackageDbArgs)
import Settings.Builders.Common (cIncludeArgs)

-- TODO: add support for -dyno
-- $1/$2/build/%.$$($3_o-bootsuf) : $1/$4/%.hs-boot
--     $$(call cmd,$1_$2_HC) $$($1_$2_$3_ALL_HC_OPTS) -c $$< -o $$@
--     $$(if $$(findstring YES,$$($1_$2_DYNAMIC_TOO)),-dyno
--     $$(addsuffix .$$(dyn_osuf)-boot,$$(basename $$@)))
ghcBuilderArgs :: Args
ghcBuilderArgs = stagedBuilder Ghc ? do
    output <- getOutput
    way    <- getWay
    let buildObj = ("//*." ++ osuf way) ?== output || ("//*." ++ obootsuf way) ?== output
    libs    <- getPkgDataList DepExtraLibs
    libDirs <- getPkgDataList DepLibDirs
    mconcat [ commonGhcArgs
            , arg "-H32m"
            , stage0    ? arg "-O"
            , notStage0 ? arg "-O2"
            , arg "-Wall"
            , arg "-fwarn-tabs"
            , splitObjectsArgs
            , not buildObj ? arg "-no-auto-link-packages"
            , not buildObj ? append [ "-optl-l" ++ lib | lib <- libs    ]
            , not buildObj ? append [ "-optl-L" ++ dir | dir <- libDirs ]
            , buildObj ? arg "-c"
            , append =<< getInputs
            , arg "-o", arg =<< getOutput ]

splitObjectsArgs :: Args
splitObjectsArgs = splitObjects ? do
    lift $ need [ghcSplit]
    arg "-split-objs"

ghcMBuilderArgs :: Args
ghcMBuilderArgs = stagedBuilder GhcM ? do
    ways <- getWays
    mconcat [ arg "-M"
            , commonGhcArgs
            , arg "-include-pkg-deps"
            , arg "-dep-makefile", arg =<< getOutput
            , append $ concat [ ["-dep-suffix", wayPrefix w] | w <- ways ]
            , append =<< getInputs ]

-- This is included into ghcBuilderArgs, ghcMBuilderArgs and haddockBuilderArgs
commonGhcArgs :: Args
commonGhcArgs = do
    way     <- getWay
    path    <- getTargetPath
    hsArgs  <- getPkgDataList HsArgs
    cppArgs <- getPkgDataList CppArgs
    let buildPath = path -/- "build"
    mconcat [ arg "-hisuf", arg $ hisuf way
            , arg "-osuf" , arg $  osuf way
            , arg "-hcsuf", arg $ hcsuf way
            , wayGhcArgs
            , packageGhcArgs
            , includeGhcArgs
            , append hsArgs
            , append $ map ("-optP" ++) cppArgs
            , arg "-odir"    , arg buildPath
            , arg "-hidir"   , arg buildPath
            , arg "-stubdir" , arg buildPath
            , arg "-rtsopts" ] -- TODO: ifeq "$(HC_VERSION_GE_6_13)" "YES"

-- TODO: do '-ticky' in all debug ways?
wayGhcArgs :: Args
wayGhcArgs = do
    way <- getWay
    mconcat [ if (Dynamic `wayUnit` way)
              then append ["-fPIC", "-dynamic"]
              else arg "-static"
            , (Threaded  `wayUnit` way) ? arg "-optc-DTHREADED_RTS"
            , (Debug     `wayUnit` way) ? arg "-optc-DDEBUG"
            , (Profiling `wayUnit` way) ? arg "-prof"
            , (Logging   `wayUnit` way) ? arg "-eventlog"
            , (Parallel  `wayUnit` way) ? arg "-parallel"
            , (GranSim   `wayUnit` way) ? arg "-gransim"
            , (way == debug || way == debugDynamic) ?
              append ["-ticky", "-DTICKY_TICKY"] ]

-- TODO: Improve handling of "-hide-all-packages"
packageGhcArgs :: Args
packageGhcArgs = do
    pkg       <- getPackage
    compId    <- getPkgData ComponentId
    pkgDepIds <- getPkgDataList DepIds
    mconcat
        [ arg "-hide-all-packages"
        , arg "-no-user-package-db"
        , bootPackageDbArgs
        , isLibrary pkg ? (arg $ "-this-package-key " ++ compId)
        , append $ map ("-package-id " ++) pkgDepIds ]

-- TODO: Improve handling of "cabal_macros.h"
includeGhcArgs :: Args
includeGhcArgs = do
    pkg     <- getPackage
    path    <- getTargetPath
    srcDirs <- getPkgDataList SrcDirs
    let buildPath   = path -/- "build"
        autogenPath = buildPath -/- "autogen"
    mconcat [ arg "-i"
            , arg $ "-i" ++ buildPath
            , arg $ "-i" ++ autogenPath
            , append [ "-i" ++ pkgPath pkg -/- dir | dir <- srcDirs ]
            , cIncludeArgs
            , arg "-optP-include"
            , arg $ "-optP" ++ autogenPath -/- "cabal_macros.h" ]

-- # Options for passing to plain ld
-- $1_$2_$3_ALL_LD_OPTS = \
--  $$(WAY_$3_LD_OPTS) \
--  $$($1_$2_DIST_LD_OPTS) \
--  $$($1_$2_$3_LD_OPTS) \
--  $$($1_$2_EXTRA_LD_OPTS) \
--  $$(EXTRA_LD_OPTS)

-- # Options for passing to GHC when we use it for linking
-- $1_$2_$3_GHC_LD_OPTS = \
--  $$(addprefix -optl, $$($1_$2_$3_ALL_LD_OPTS)) \
--  $$($1_$2_$3_MOST_HC_OPTS)

-- TODO: add support for TargetElf and darwin
-- ifeq "$3" "dyn"
-- ifneq "$4" "0"
-- ifeq "$$(TargetElf)" "YES"
-- $1_$2_$3_GHC_LD_OPTS += \
--     -fno-use-rpaths \
--     $$(foreach d,$$($1_$2_TRANSITIVE_DEP_LIB_NAMES),-optl-Wl$$(comma)-rpath -optl-Wl$$(comma)'$$$$ORIGIN/../$$d') -optl-Wl,-zorigin
-- else ifeq "$$(TargetOS_CPP)" "darwin"
-- $1_$2_$3_GHC_LD_OPTS += \
--     -fno-use-rpaths \
--     $$(foreach d,$$($1_$2_TRANSITIVE_DEP_LIB_NAMES),-optl-Wl$$(comma)-rpath -optl-Wl$$(comma)'@loader_path/../$$d')

-- ifeq "$$($1_$2_$$($1_$2_PROGRAM_WAY)_HS_OBJS)" ""
-- # We don't want to link the GHC RTS into C-only programs. There's no
-- # point, and it confuses the test that all GHC-compiled programs
-- # were compiled with the right GHC.
-- $1_$2_$$($1_$2_PROGRAM_WAY)_GHC_LD_OPTS += -no-auto-link-packages -no-hs-main
-- endif

-- # Link a dynamic library
-- # On windows we have to supply the extra libs this one links to when building it.
-- ifeq "$$(HostOS_CPP)" "mingw32"
-- $$($1_$2_$3_LIB) : $$($1_$2_$3_ALL_OBJS) $$(ALL_RTS_LIBS) $$($1_$2_$3_DEPS_LIBS)
-- ifneq "$$($1_$2_$3_LIB0)" ""
--     $$(call build-dll,$1,$2,$3,
--    -L$1/$2/build -l$$($1_$2_$3_LIB0_ROOT),
--    $$(filter-out $$($1_$2_dll0_HS_OBJS),$$($1_$2_$3_HS_OBJS))
--    $$($1_$2_$3_NON_HS_OBJS),$$@)
-- else
--     $$(call build-dll,$1,$2,$3,,$$($1_$2_$3_HS_OBJS) $$($1_$2_$3_NON_HS_OBJS),$$@)
-- endif

-- ifneq "$$($1_$2_$3_LIB0)" ""
-- $$($1_$2_$3_LIB) : $$($1_$2_$3_LIB0)
-- $$($1_$2_$3_LIB0) : $$($1_$2_$3_ALL_OBJS) $$(ALL_RTS_LIBS) $$($1_$2_$3_DEPS_LIBS)
--     $$(call build-dll,$1,$2,$3,,$$($1_$2_dll0_HS_OBJS) $$($1_$2_$3_NON_HS_OBJS),$$($1_$2_$3_LIB0))
-- endif



-- # $1 = dir
-- # $2 = distdir
-- # $3 = way
-- # $4 = extra flags
-- # $5 = object files to link
-- # $6 = output filename
-- define build-dll
--     $(call cmd,$1_$2_HC) $($1_$2_$3_ALL_HC_OPTS) $($1_$2_$3_GHC_LD_OPTS) $4 $5 \
--         -shared -dynamic -dynload deploy \
--         $(addprefix -l,$($1_$2_EXTRA_LIBRARIES)) \
--         -no-auto-link-packages \
--         -o $6
-- # Now check that the DLL doesn't have too many symbols. See trac #5987.
--     SYMBOLS=`$(OBJDUMP) -p $6 | sed -n "1,/^.Ordinal\/Name Pointer/ D; p; /^$$/ q" | tail -n +2 | wc -l`; echo "Number of symbols in $6: $$SYMBOLS"
--     case `$(OBJDUMP) -p $6 | sed -n "1,/^.Ordinal\/Name Pointer/ D; p; /^$$/ q" | grep "\[ *0\]" | wc -l` in 1) echo DLL $6 OK;; 0) echo No symbols in DLL $6; exit 1;; [0-9]*) echo Too many symbols in DLL $6; $(OBJDUMP) -p $6 | sed -n "1,/^.Ordinal\/Name Pointer/ D; p; /^$$/ q" | tail; exit 1;; *) echo bad DLL $6; exit 1;; esac
-- endef



-- TODO: add -dynamic-too?
-- # $1_$2_$3_ALL_HC_OPTS: this is all the options we will pass to GHC
-- # for a given ($1,$2,$3).
-- $1_$2_$3_ALL_HC_OPTS = \
--  -hisuf $$($3_hisuf) -osuf  $$($3_osuf) -hcsuf $$($3_hcsuf) \
--  $$($1_$2_$3_MOST_DIR_HC_OPTS) \
--  $$(if $$(findstring YES,$$($1_$2_SplitObjs)),$$(if $$(findstring dyn,$3),,-split-objs),) \
--  $$(if $$(findstring YES,$$($1_$2_DYNAMIC_TOO)),$$(if $$(findstring v,$3),-dynamic-too))
