-----------------------------------------------------------------------------
--
-- GHC Driver
--
-- (c) The University of Glasgow 2002
--
-----------------------------------------------------------------------------

module GHC.Driver.Phases (
   Phase(..),
   happensBefore, eqPhase, isStopLn,
   startPhase,
   phaseInputExt,

   StopPhase(..),
   stopPhaseToPhase,

   isHaskellishSuffix,
   isHaskellSrcSuffix,
   isBackpackishSuffix,
   isObjectSuffix,
   isCishSuffix,
   isDynLibSuffix,
   isHaskellUserSrcSuffix,
   isHaskellSigSuffix,
   isSourceSuffix,

   isHaskellishTarget,

   isHaskellishFilename,
   isHaskellSrcFilename,
   isHaskellSigFilename,
   isObjectFilename,
   isCishFilename,
   isDynLibFilename,
   isHaskellUserSrcFilename,
   isSourceFilename,

   phaseForeignLanguage
 ) where

import GHC.Prelude

import GHC.Platform

import GHC.ForeignSrcLang

import GHC.Types.SourceFile

import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Misc

import System.FilePath

-----------------------------------------------------------------------------
-- Phases

{-
   Phase of the           | Suffix saying | Flag saying   | (suffix of)
   compilation system     | ``start here''| ``stop after''| output file

   literate pre-processor | .lhs          | -             | -
   C pre-processor (opt.) | -             | -E            | -
   Haskell compiler       | .hs           | -C, -S        | .hc, .s
   C compiler (opt.)      | .hc or .c     | -S            | .s
   assembler              | .s  or .S     | -c            | .o
   linker                 | other         | -             | a.out
   linker (merge objects) | other         | -             | .o
-}

-- Phases we can actually stop after
data StopPhase = StopPreprocess -- ^ @-E@
               | StopC          -- ^ @-C@
               | StopAs         -- ^ @-S@
               | NoStop         -- ^ @-c@

stopPhaseToPhase :: StopPhase -> Phase
stopPhaseToPhase StopPreprocess = anyHsc
stopPhaseToPhase StopC          = HCc
stopPhaseToPhase StopAs         = As False
stopPhaseToPhase NoStop         = StopLn

-- | Untyped Phase description
data Phase
        = Unlit HscSource
        | Cpp   HscSource
        | HsPp  HscSource
        | Hsc   HscSource
        | Ccxx          -- Compile C++
        | Cc            -- Compile C
        | Cobjc         -- Compile Objective-C
        | Cobjcxx       -- Compile Objective-C++
        | HCc           -- Haskellised C (as opposed to vanilla C) compilation
        | As Bool       -- Assembler for regular assembly files (Bool: with-cpp)
        | LlvmOpt       -- Run LLVM opt tool over llvm assembly
        | LlvmLlc       -- LLVM bitcode to native assembly
        | LlvmMangle    -- Fix up TNTC by processing assembly produced by LLVM
        | CmmCpp        -- pre-process Cmm source
        | Cmm           -- parse & compile Cmm code
        | MergeForeign  -- merge in the foreign object files

        -- The final phase is a pseudo-phase that tells the pipeline to stop.
        | StopLn        -- Stop, but linking will follow, so generate .o file
  deriving (Eq, Show)

instance Outputable Phase where
    ppr p = text (show p)

anyHsc :: Phase
anyHsc = Hsc (panic "anyHsc")

isStopLn :: Phase -> Bool
isStopLn StopLn = True
isStopLn _      = False

eqPhase :: Phase -> Phase -> Bool
-- Equality of constructors, ignoring the HscSource field
-- NB: the HscSource field can be 'bot'; see anyHsc above
eqPhase (Unlit _)   (Unlit _)  = True
eqPhase (Cpp   _)   (Cpp   _)  = True
eqPhase (HsPp  _)   (HsPp  _)  = True
eqPhase (Hsc   _)   (Hsc   _)  = True
eqPhase Cc          Cc         = True
eqPhase Cobjc       Cobjc      = True
eqPhase HCc         HCc        = True
eqPhase (As x)      (As y)     = x == y
eqPhase LlvmOpt     LlvmOpt    = True
eqPhase LlvmLlc     LlvmLlc    = True
eqPhase LlvmMangle  LlvmMangle = True
eqPhase CmmCpp      CmmCpp     = True
eqPhase Cmm         Cmm        = True
eqPhase MergeForeign MergeForeign  = True
eqPhase StopLn      StopLn     = True
eqPhase Ccxx        Ccxx       = True
eqPhase Cobjcxx     Cobjcxx    = True
eqPhase _           _          = False

-- MP: happensBefore is only used in preprocessPipeline, that usage should
-- be refactored and this usage removed.
happensBefore :: Platform -> Phase -> Phase -> Bool
happensBefore platform p1 p2 = p1 `happensBefore'` p2
    where StopLn `happensBefore'` _ = False
          x      `happensBefore'` y = after_x `eqPhase` y
                                   || after_x `happensBefore'` y
              where after_x = nextPhase platform x

nextPhase :: Platform -> Phase -> Phase
nextPhase platform p
    -- A conservative approximation to the next phase, used in happensBefore
    = case p of
      Unlit sf   -> Cpp  sf
      Cpp   sf   -> HsPp sf
      HsPp  sf   -> Hsc  sf
      Hsc   _    -> maybeHCc
      LlvmOpt    -> LlvmLlc
      LlvmLlc    -> LlvmMangle
      LlvmMangle -> As False
      As _       -> MergeForeign
      Ccxx       -> As False
      Cc         -> As False
      Cobjc      -> As False
      Cobjcxx    -> As False
      CmmCpp     -> Cmm
      Cmm        -> maybeHCc
      HCc        -> As False
      MergeForeign -> StopLn
      StopLn     -> panic "nextPhase: nothing after StopLn"
    where maybeHCc = if platformUnregisterised platform
                     then HCc
                     else As False

-- the first compilation phase for a given file is determined
-- by its suffix.
startPhase :: String -> Phase
startPhase "lhs"      = Unlit HsSrcFile
startPhase "lhs-boot" = Unlit HsBootFile
startPhase "lhsig"    = Unlit HsigFile
startPhase "hs"       = Cpp   HsSrcFile
startPhase "hs-boot"  = Cpp   HsBootFile
startPhase "hsig"     = Cpp   HsigFile
startPhase "hscpp"    = HsPp  HsSrcFile
startPhase "hspp"     = Hsc   HsSrcFile
startPhase "hc"       = HCc
startPhase "c"        = Cc
startPhase "cpp"      = Ccxx
startPhase "C"        = Cc
startPhase "m"        = Cobjc
startPhase "M"        = Cobjcxx
startPhase "mm"       = Cobjcxx
startPhase "cc"       = Ccxx
startPhase "cxx"      = Ccxx
startPhase "s"        = As False
startPhase "S"        = As True
startPhase "ll"       = LlvmOpt
startPhase "bc"       = LlvmLlc
startPhase "lm_s"     = LlvmMangle
startPhase "o"        = StopLn
startPhase "cmm"      = CmmCpp
startPhase "cmmcpp"   = Cmm
startPhase _          = StopLn     -- all unknown file types

-- This is used to determine the extension for the output from the
-- current phase (if it generates a new file).  The extension depends
-- on the next phase in the pipeline.
phaseInputExt :: Phase -> String
phaseInputExt (Unlit HsSrcFile)   = "lhs"
phaseInputExt (Unlit HsBootFile)  = "lhs-boot"
phaseInputExt (Unlit HsigFile)    = "lhsig"
phaseInputExt (Cpp   _)           = "lpp"       -- intermediate only
phaseInputExt (HsPp  _)           = "hscpp"     -- intermediate only
phaseInputExt (Hsc   _)           = "hspp"      -- intermediate only
        -- NB: as things stand, phaseInputExt (Hsc x) must not evaluate x
        --     because runPhase uses the StopBefore phase to pick the
        --     output filename.  That could be fixed, but watch out.
phaseInputExt HCc                 = "hc"
phaseInputExt Ccxx                = "cpp"
phaseInputExt Cobjc               = "m"
phaseInputExt Cobjcxx             = "mm"
phaseInputExt Cc                  = "c"
phaseInputExt (As True)           = "S"
phaseInputExt (As False)          = "s"
phaseInputExt LlvmOpt             = "ll"
phaseInputExt LlvmLlc             = "bc"
phaseInputExt LlvmMangle          = "lm_s"
phaseInputExt CmmCpp              = "cmmcpp"
phaseInputExt Cmm                 = "cmm"
phaseInputExt MergeForeign        = "o"
phaseInputExt StopLn              = "o"

haskellish_src_suffixes, backpackish_suffixes, haskellish_suffixes, cish_suffixes,
    haskellish_user_src_suffixes, haskellish_sig_suffixes
 :: [String]
-- When a file with an extension in the haskellish_src_suffixes group is
-- loaded in --make mode, its imports will be loaded too.
haskellish_src_suffixes      = haskellish_user_src_suffixes ++
                               [ "hspp", "hscpp" ]
haskellish_suffixes          = haskellish_src_suffixes ++
                               [ "hc", "cmm", "cmmcpp" ]
cish_suffixes                = [ "c", "cpp", "C", "cc", "cxx", "s", "S", "ll", "bc", "lm_s", "m", "M", "mm" ]

-- Will not be deleted as temp files:
haskellish_user_src_suffixes =
  haskellish_sig_suffixes ++ [ "hs", "lhs", "hs-boot", "lhs-boot" ]
haskellish_sig_suffixes      = [ "hsig", "lhsig" ]
backpackish_suffixes         = [ "bkp" ]

objish_suffixes :: Platform -> [String]
-- Use the appropriate suffix for the system on which
-- the GHC-compiled code will run
objish_suffixes platform = case platformOS platform of
  OSMinGW32 -> [ "o", "O", "obj", "OBJ" ]
  _         -> [ "o" ]

dynlib_suffixes :: Platform -> [String]
dynlib_suffixes platform = case platformOS platform of
  OSMinGW32 -> ["dll", "DLL"]
  OSDarwin  -> ["dylib", "so"]
  _         -> ["so"]

isHaskellishSuffix, isBackpackishSuffix, isHaskellSrcSuffix, isCishSuffix,
    isHaskellUserSrcSuffix, isHaskellSigSuffix
 :: String -> Bool
isHaskellishSuffix     s = s `elem` haskellish_suffixes
isBackpackishSuffix    s = s `elem` backpackish_suffixes
isHaskellSigSuffix     s = s `elem` haskellish_sig_suffixes
isHaskellSrcSuffix     s = s `elem` haskellish_src_suffixes
isCishSuffix           s = s `elem` cish_suffixes
isHaskellUserSrcSuffix s = s `elem` haskellish_user_src_suffixes

isObjectSuffix, isDynLibSuffix :: Platform -> String -> Bool
isObjectSuffix platform s = s `elem` objish_suffixes platform
isDynLibSuffix platform s = s `elem` dynlib_suffixes platform

isSourceSuffix :: String -> Bool
isSourceSuffix suff  = isHaskellishSuffix suff
                    || isCishSuffix suff
                    || isBackpackishSuffix suff

-- | When we are given files (modified by -x arguments) we need
-- to determine if they are Haskellish or not to figure out
-- how we should try to compile it.  The rules are:
--
--      1. If no -x flag was specified, we check to see if
--         the file looks like a module name, has no extension,
--         or has a Haskell source extension.
--
--      2. If an -x flag was specified, we just make sure the
--         specified suffix is a Haskell one.
isHaskellishTarget :: (String, Maybe Phase) -> Bool
isHaskellishTarget (f,Nothing) =
  looksLikeModuleName f || isHaskellSrcFilename f || not (hasExtension f)
isHaskellishTarget (_,Just phase) =
  phase `notElem` [ As True, As False, Cc, Cobjc, Cobjcxx, CmmCpp, Cmm
                  , StopLn]

isHaskellishFilename, isHaskellSrcFilename, isCishFilename,
    isHaskellUserSrcFilename, isSourceFilename, isHaskellSigFilename
 :: FilePath -> Bool
-- takeExtension return .foo, so we drop 1 to get rid of the .
isHaskellishFilename     f = isHaskellishSuffix     (drop 1 $ takeExtension f)
isHaskellSrcFilename     f = isHaskellSrcSuffix     (drop 1 $ takeExtension f)
isCishFilename           f = isCishSuffix           (drop 1 $ takeExtension f)
isHaskellUserSrcFilename f = isHaskellUserSrcSuffix (drop 1 $ takeExtension f)
isSourceFilename         f = isSourceSuffix         (drop 1 $ takeExtension f)
isHaskellSigFilename     f = isHaskellSigSuffix     (drop 1 $ takeExtension f)

isObjectFilename, isDynLibFilename :: Platform -> FilePath -> Bool
isObjectFilename platform f = isObjectSuffix platform (drop 1 $ takeExtension f)
isDynLibFilename platform f = isDynLibSuffix platform (drop 1 $ takeExtension f)

-- | Foreign language of the phase if the phase deals with a foreign code
phaseForeignLanguage :: Phase -> Maybe ForeignSrcLang
phaseForeignLanguage phase = case phase of
  Cc           -> Just LangC
  Ccxx         -> Just LangCxx
  Cobjc        -> Just LangObjc
  Cobjcxx      -> Just LangObjcxx
  HCc          -> Just LangC
  As _         -> Just LangAsm
  MergeForeign -> Just RawObject
  _            -> Nothing

