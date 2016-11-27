module Settings.Builders.GhcCabal (
    ghcCabalBuilderArgs, ghcCabalHsColourBuilderArgs, buildDll0
    ) where

import Context
import Flavour
import Settings.Builders.Common
import Util

ghcCabalBuilderArgs :: Args
ghcCabalBuilderArgs = builder GhcCabal ? do
    verbosity <- lift $ getVerbosity
    top       <- getTopDirectory
    context   <- getContext
    mconcat [ arg "configure"
            , arg =<< getPackagePath
            , arg $ top -/- buildPath context
            , dll0Args
            , withStaged $ Ghc CompileHs
            , withStaged (GhcPkg Update)
            , bootPackageDatabaseArgs
            , libraryArgs
            , with HsColour
            , configureArgs
            , packageConstraints
            , withStaged $ Cc CompileC
            , notStage0 ? with Ld
            , with Ar
            , with Alex
            , with Happy
            , verbosity < Chatty ? append [ "-v0", "--configure-option=--quiet"
                , "--configure-option=--disable-option-checking"  ] ]

ghcCabalHsColourBuilderArgs :: Args
ghcCabalHsColourBuilderArgs = builder GhcCabalHsColour ? do
    path <- getPackagePath
    dir  <- getContextDirectory
    append [ "hscolour", path, dir ]

-- TODO: Isn't vanilla always built? If yes, some conditions are redundant.
-- TODO: Need compiler_stage1_CONFIGURE_OPTS += --disable-library-for-ghci?
libraryArgs :: Args
libraryArgs = do
    ways     <- getLibraryWays
    withGhci <- lift ghcWithInterpreter
    append [ if vanilla `elem` ways
             then  "--enable-library-vanilla"
             else "--disable-library-vanilla"
           , if vanilla `elem` ways && withGhci && not (dynamicGhcPrograms flavour)
             then  "--enable-library-for-ghci"
             else "--disable-library-for-ghci"
           , if profiling `elem` ways
             then  "--enable-library-profiling"
             else "--disable-library-profiling"
           , if dynamic `elem` ways
             then  "--enable-shared"
             else "--disable-shared" ]

-- TODO: LD_OPTS?
configureArgs :: Args
configureArgs = do
    top <- getTopDirectory
    let conf key = appendSubD $ "--configure-option=" ++ key
        cFlags   = mconcat [ cArgs
                           , remove ["-Werror"]
                           , argStagedSettingList ConfCcArgs
                           , arg $ "-I" ++ top -/- generatedPath ]
        ldFlags  = ldArgs  <> (argStagedSettingList ConfGccLinkerArgs)
        cppFlags = cppArgs <> (argStagedSettingList ConfCppArgs)
    mconcat
        [ conf "CFLAGS"   cFlags
        , conf "LDFLAGS"  ldFlags
        , conf "CPPFLAGS" cppFlags
        , appendSubD "--gcc-options" $ cFlags <> ldFlags
        , conf "--with-iconv-includes"    $ argSetting IconvIncludeDir
        , conf "--with-iconv-libraries"   $ argSetting IconvLibDir
        , conf "--with-gmp-includes"      $ argSetting GmpIncludeDir
        , conf "--with-gmp-libraries"     $ argSetting GmpLibDir
        , crossCompiling ? (conf "--host" $ argSetting TargetPlatformFull)
        , conf "--with-cc" $ argStagedBuilderPath (Cc CompileC) ]

packageConstraints :: Args
packageConstraints = stage0 ? do
    constraints <- lift . readFileLines $ bootPackageConstraints
    append $ concat [ ["--constraint", c] | c <- constraints ]

cppArgs :: Args
cppArgs = arg $ "-I" ++ generatedPath

withBuilderKey :: Builder -> String
withBuilderKey b = case b of
    Ar         -> "--with-ar="
    Ld         -> "--with-ld="
    Cc  _ _    -> "--with-gcc="
    Ghc _ _    -> "--with-ghc="
    Alex       -> "--with-alex="
    Happy      -> "--with-happy="
    GhcPkg _ _ -> "--with-ghc-pkg="
    HsColour   -> "--with-hscolour="
    _          -> error $ "withBuilderKey: not supported builder " ++ show b

-- Expression 'with Alex' appends "--with-alex=/path/to/alex" and needs Alex.
with :: Builder -> Args
with b = isSpecified b ? do
    top  <- getTopDirectory
    path <- getBuilderPath b
    lift $ needBuilder b
    arg $ withBuilderKey b ++ unifyPath (top </> path)

withStaged :: (Stage -> Builder) -> Args
withStaged sb = with . sb =<< getStage

buildDll0 :: Context -> Action Bool
buildDll0 Context {..} = do
    windows <- windowsHost
    return $ windows && stage == Stage1 && package == compiler

-- This is a positional argument, hence:
-- * if it is empty, we need to emit one empty string argument;
-- * otherwise, we must collapse it into one space-separated string.
dll0Args :: Args
dll0Args = do
    context  <- getContext
    dll0     <- lift $ buildDll0 context
    withGhci <- lift ghcWithInterpreter
    arg . unwords . concat $ [ modules     | dll0             ]
                          ++ [ ghciModules | dll0 && withGhci ] -- see #9552
  where
    modules = [ "Annotations"
              , "ApiAnnotation"
              , "Avail"
              , "Bag"
              , "BasicTypes"
              , "Binary"
              , "BooleanFormula"
              , "BreakArray"
              , "BufWrite"
              , "Class"
              , "CmdLineParser"
              , "CmmType"
              , "CoAxiom"
              , "ConLike"
              , "Coercion"
              , "Config"
              , "Constants"
              , "CoreArity"
              , "CoreFVs"
              , "CoreSubst"
              , "CoreSyn"
              , "CoreTidy"
              , "CoreUnfold"
              , "CoreUtils"
              , "CoreSeq"
              , "CoreStats"
              , "CostCentre"
              , "Ctype"
              , "DataCon"
              , "Demand"
              , "Digraph"
              , "DriverPhases"
              , "DynFlags"
              , "Encoding"
              , "ErrUtils"
              , "Exception"
              , "ExtsCompat46"
              , "FamInstEnv"
              , "FastFunctions"
              , "FastMutInt"
              , "FastString"
              , "FastTypes"
              , "Fingerprint"
              , "FiniteMap"
              , "ForeignCall"
              , "Hooks"
              , "HsBinds"
              , "HsDecls"
              , "HsDoc"
              , "HsExpr"
              , "HsImpExp"
              , "HsLit"
              , "PlaceHolder"
              , "HsPat"
              , "HsSyn"
              , "HsTypes"
              , "HsUtils"
              , "HscTypes"
              , "IOEnv"
              , "Id"
              , "IdInfo"
              , "IfaceSyn"
              , "IfaceType"
              , "InstEnv"
              , "Kind"
              , "Lexeme"
              , "Lexer"
              , "ListSetOps"
              , "Literal"
              , "Maybes"
              , "MkCore"
              , "MkId"
              , "Module"
              , "MonadUtils"
              , "Name"
              , "NameEnv"
              , "NameSet"
              , "OccName"
              , "OccurAnal"
              , "OptCoercion"
              , "OrdList"
              , "Outputable"
              , "PackageConfig"
              , "Packages"
              , "Pair"
              , "Panic"
              , "PatSyn"
              , "PipelineMonad"
              , "Platform"
              , "PlatformConstants"
              , "PprCore"
              , "PrelNames"
              , "PrelRules"
              , "Pretty"
              , "PrimOp"
              , "RdrName"
              , "Rules"
              , "Serialized"
              , "SrcLoc"
              , "StaticFlags"
              , "StringBuffer"
              , "TcEvidence"
              , "TcRnTypes"
              , "TcType"
              , "TrieMap"
              , "TyCon"
              , "Type"
              , "TypeRep"
              , "TysPrim"
              , "TysWiredIn"
              , "Unify"
              , "UniqFM"
              , "UniqSet"
              , "UniqSupply"
              , "Unique"
              , "Util"
              , "Var"
              , "VarEnv"
              , "VarSet" ]
    ghciModules = [ "Bitmap"
                  , "BlockId"
                  , "ByteCodeAsm"
                  , "ByteCodeInstr"
                  , "ByteCodeItbls"
                  , "CLabel"
                  , "Cmm"
                  , "CmmCallConv"
                  , "CmmExpr"
                  , "CmmInfo"
                  , "CmmMachOp"
                  , "CmmNode"
                  , "CmmSwitch"
                  , "CmmUtils"
                  , "CodeGen.Platform"
                  , "CodeGen.Platform.ARM"
                  , "CodeGen.Platform.ARM64"
                  , "CodeGen.Platform.NoRegs"
                  , "CodeGen.Platform.PPC"
                  , "CodeGen.Platform.PPC_Darwin"
                  , "CodeGen.Platform.SPARC"
                  , "CodeGen.Platform.X86"
                  , "CodeGen.Platform.X86_64"
                  , "FastBool"
                  , "Hoopl"
                  , "Hoopl.Dataflow"
                  , "InteractiveEvalTypes"
                  , "MkGraph"
                  , "PprCmm"
                  , "PprCmmDecl"
                  , "PprCmmExpr"
                  , "Reg"
                  , "RegClass"
                  , "SMRep"
                  , "StgCmmArgRep"
                  , "StgCmmClosure"
                  , "StgCmmEnv"
                  , "StgCmmLayout"
                  , "StgCmmMonad"
                  , "StgCmmProf"
                  , "StgCmmTicky"
                  , "StgCmmUtils"
                  , "StgSyn"
                  , "Stream" ]
