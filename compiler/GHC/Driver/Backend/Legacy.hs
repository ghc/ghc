{-# LANGUAGE MultiWayIf #-}


-- mutual recursion: Backend is in DynFlags, so can't include a
-- function that depends on DynFlags, even just to extract something
-- from DynFlags

-- | Code generation backends
module GHC.Driver.Backend.Legacy
   ( Backend
   , PrimitiveImplementation(..)
   , platformDefaultBackend
   , platformNcgSupported
   , backendGeneratesCode
   , backendWantsGlobalBindings

   , backendCDefs
   , backendWantsClangTools

   , backendNeedsFullWays

   , ncgBackend
   , llvmBackend
   , viaCBackend
   , interpreterBackend
   , noBackend

   , backendPrimitiveImplementation

   , backendHasNativeSwitch

   , backendValidityOfCExport
   , backendValidityOfCImport

   , backendGeneratesHc

   , backendSupportsHpc
   , backendNeedsPlatformNcgSupport

   , backendUnregisterisedAbiOnly
   , backendSwappableWithViaC
   , backendDescription

   , backendForcesOptimization0
   , backendSupportsUnsplitProcPoints

   , backendSpecialModuleSource

   , backendWantsBreakpointTicks

   , backendSupportsEmbeddedBlobs

   , backendSimdValidity

   , backendSptIsDynamic

   , backendSupportsInterfaceWriting

   , backendRespectsSpecialise

   , backendWritesFiles

   , backendNormalSuccessorPhase

   , backendPipelineOutput

   , backendPipeline, PipelineName(..)

   , LlvmVersion(..)

   )

where

--import GHC.Driver.Phases
import GHC.IO.Handle
--import GHC.IO.Handle.Text
import GHC.Prelude
import GHC.Platform
import GHC.CmmToLlvm.LlvmVersion
import GHC.Utils.Error
import GHC.Utils.Exception
import GHC.Utils.Logger
import GHC.Utils.Misc
import GHC.Utils.Outputable
import GHC.Driver.Pipeline.Monad
import GHC.Utils.CliOption

import GHC.Driver.Phases

import System.Process

-- | Code generation backends.
--
-- GHC supports several code generation backends serving different purposes
-- (producing machine code, producing ByteCode for the interpreter) and
-- supporting different platforms.
--
data Backend
   = NCG           -- ^ Native code generator backend.
                   --
                   -- Compiles Cmm code into textual assembler, then relies on
                   -- an external assembler toolchain to produce machine code.
                   --
                   -- Only supports a few platforms (X86, PowerPC, AArch64).
                   --
                   -- See "GHC.CmmToAsm".


   | LLVM          -- ^ LLVM backend.
                   --
                   -- Compiles Cmm code into LLVM textual IR, then relies on
                   -- LLVM toolchain to produce machine code.
                   --
                   -- It relies on LLVM support for the calling convention used
                   -- by the NCG backend to produce code objects ABI compatible
                   -- with it (see "cc 10" or "ghccc" calling convention in
                   -- https://llvm.org/docs/LangRef.html#calling-conventions).
                   --
                   -- Support a few platforms (X86, AArch64, s390x, ARM).
                   --
                   -- See "GHC.CmmToLlvm"


   | ViaC          -- ^ Via-C backend.
                   --
                   -- Compiles Cmm code into C code, then relies on a C compiler
                   -- to produce machine code.
                   --
                   -- It produces code objects that are *not* ABI compatible
                   -- with those produced by NCG and LLVM backends.
                   --
                   -- Produced code is expected to be less efficient than the
                   -- one produced by NCG and LLVM backends because STG
                   -- registers are not pinned into real registers.  On the
                   -- other hand, it supports more target platforms (those
                   -- having a valid C toolchain).
                   --
                   -- See "GHC.CmmToC"


   | Interpreter   -- ^ ByteCode interpreter.
                   --
                   -- Produce ByteCode objects (BCO, see "GHC.ByteCode") that
                   -- can be interpreted. It is used by GHCi.
                   --
                   -- Currently some extensions are not supported
                   -- (foreign primops).
                   --
                   -- See "GHC.StgToByteCode"


   | NoBackend     -- ^ No code generated.
                   --
                   -- Use this to disable code generation. It is particularly
                   -- useful when GHC is used as a library for other purpose
                   -- than generating code (e.g. to generate documentation with
                   -- Haddock) or when the user requested it (via -fno-code) for
                   -- some reason.
  deriving (Show)




data PipelineName = ViaCPipeline | NCGPipeline | LLVMPipeline | NoPipeline



data PrimitiveImplementation
    = LlvmPrimitives
    | NcgPrimitives
    | GenericPrimitives
  deriving Show


backendPipeline :: Backend -> PipelineName
backendPipeline ViaC = ViaCPipeline
backendPipeline NCG = NCGPipeline
backendPipeline LLVM = LLVMPipeline
backendPipeline _ = NoPipeline

backendUnregisterisedAbiOnly :: Backend -> Bool

backendUnregisterisedAbiOnly ViaC = True
backendUnregisterisedAbiOnly _ = False

-- | When the target platform supports *only* an unregisterised API,
-- this backend can be replaced with compilation via C.  Or when the
-- target does *not* support an unregisterised API, this back end can
-- replace compilation via C.

backendSwappableWithViaC :: Backend -> Bool
backendSwappableWithViaC NCG = True
backendSwappableWithViaC LLVM = True
backendSwappableWithViaC _ = False


backendNeedsPlatformNcgSupport :: Backend -> Bool
backendNeedsPlatformNcgSupport NCG = True
backendNeedsPlatformNcgSupport _ = False

backendForcesOptimization0 :: Backend -> Bool
backendForcesOptimization0 Interpreter = True
backendForcesOptimization0 _ = False

backendNeedsFullWays :: Backend -> Bool
backendNeedsFullWays Interpreter = True
backendNeedsFullWays _ = False

backendGeneratesHc :: Backend -> Bool
backendGeneratesHc ViaC = True
backendGeneratesHc _ = False

backendDescription :: Backend -> String
-- ^ For use in issuing warning messages *only*.  If code depends
-- on what's in the string, you deserve what happens to you.

backendDescription NCG = "native code generator"
backendDescription LLVM =  "LLVM"
backendDescription ViaC = "compiling via C"
backendDescription Interpreter = "byte-code interpreter"
backendDescription NoBackend = "no code generated"



ncgBackend, llvmBackend, viaCBackend, interpreterBackend, noBackend :: Backend

ncgBackend = NCG
llvmBackend = LLVM
viaCBackend = ViaC
interpreterBackend = Interpreter
noBackend = NoBackend

backendPrimitiveImplementation :: Backend -> PrimitiveImplementation
backendPrimitiveImplementation NCG = NcgPrimitives
backendPrimitiveImplementation LLVM = LlvmPrimitives
backendPrimitiveImplementation _ = GenericPrimitives

-- these are checks for foreign declarations

-- N.B. If there is no back end, all imports and exports are considered valid.
backendValidityOfCExport :: Backend -> Validity
backendValidityOfCExport NoBackend = IsValid
backendValidityOfCExport ViaC = IsValid
backendValidityOfCExport NCG  = IsValid
backendValidityOfCExport LLVM = IsValid
backendValidityOfCExport _
  = NotValid (text "requires unregisterised, llvm (-fllvm) or native code generation (-fasm)")

-- | Checking a supported backend is in use
backendValidityOfCImport :: Backend -> Validity
backendValidityOfCImport ViaC        = IsValid
backendValidityOfCImport NCG         = IsValid
backendValidityOfCImport LLVM        = IsValid
backendValidityOfCImport Interpreter = IsValid
backendValidityOfCImport NoBackend = IsValid


backendSupportsHpc :: Backend -> Bool
backendSupportsHpc Interpreter = False
backendSupportsHpc _ = True


--- these next two migrate to ...Backends.hs

-- | Default backend to use for the given platform.
platformDefaultBackend :: Platform -> Backend
platformDefaultBackend platform = if
      | platformUnregisterised platform -> ViaC
      | platformNcgSupported platform   -> NCG
      | otherwise                       -> LLVM


-- | Is the platform supported by the Native Code Generator?
platformNcgSupported :: Platform -> Bool
platformNcgSupported platform = if
      | platformUnregisterised platform -> False -- NCG doesn't support unregisterised ABI
      | ncgValidArch                    -> True
      | otherwise                       -> False
   where
      ncgValidArch = case platformArch platform of
         ArchX86       -> True
         ArchX86_64    -> True
         ArchPPC       -> True
         ArchPPC_64 {} -> True
         ArchAArch64   -> True
         _             -> False

backendGeneratesCode :: Backend -> Bool
backendGeneratesCode NoBackend = False
backendGeneratesCode _ = True

backendSupportsInterfaceWriting :: Backend -> Bool
backendSupportsInterfaceWriting NoBackend = False
backendSupportsInterfaceWriting _ = True

-- | Does this backend retain *all* top-level bindings for a module,
-- rather than just the exported bindings, in the TypeEnv and compiled
-- code (if any)?
--
-- Interpreter backend does this, so that GHCi can call functions inside a
-- module.
--
-- When no backend is used we also do it, so that Haddock can get access to the
-- GlobalRdrEnv for a module after typechecking it.
backendWantsGlobalBindings :: Backend -> Bool
backendWantsGlobalBindings Interpreter = True
backendWantsGlobalBindings NoBackend   = True
backendWantsGlobalBindings ViaC        = False
backendWantsGlobalBindings NCG         = False
backendWantsGlobalBindings LLVM        = False


-- | Does the backend support switch out of the box? Then leave this to the
-- backend!
backendHasNativeSwitch :: Backend -> Bool
backendHasNativeSwitch ViaC = True
backendHasNativeSwitch LLVM = True
backendHasNativeSwitch _    = False

backendSupportsUnsplitProcPoints :: Backend -> Bool
backendSupportsUnsplitProcPoints NCG = True
backendSupportsUnsplitProcPoints _   = False

-- | Used to help characterize the source of code in GHC.Unit.Module.Graph.
-- The Boolean is a "recomp" flag...
backendSpecialModuleSource :: Bool -> Backend -> Maybe String
backendSpecialModuleSource True Interpreter = Just "interpreted"
backendSpecialModuleSource _ NoBackend = Just "nothing"
backendSpecialModuleSource _ _ = Nothing

backendWantsBreakpointTicks :: Backend -> Bool
backendWantsBreakpointTicks Interpreter = True
backendWantsBreakpointTicks _ = False


backendSimdValidity :: Backend -> Validity' String
backendSimdValidity LLVM = IsValid
backendSimdValidity _ =
    NotValid $ unlines [ "SIMD vector instructions require the LLVM back-end."
                       , "Please use -fllvm."]


-- See Note [Embedding large binary blobs] in GHC.CmmToAsm.Ppr

backendSupportsEmbeddedBlobs :: Backend -> Bool
backendSupportsEmbeddedBlobs NCG = True
backendSupportsEmbeddedBlobs _ = False


-- If we are compiling for the interpreter we will insert
-- any necessary SPT entries dynamically
backendSptIsDynamic :: Backend -> Bool
backendSptIsDynamic Interpreter = True
backendSptIsDynamic _ = False

backendRespectsSpecialise :: Backend -> Bool
backendRespectsSpecialise NoBackend = False
backendRespectsSpecialise Interpreter = False
backendRespectsSpecialise _ = True

-- the back end generates code and writes files, including an interface file and object code


backendWritesFiles :: Backend -> Bool
backendWritesFiles Interpreter = False
backendWritesFiles NoBackend = False
backendWritesFiles _ = True


-- might make more sense in GHC.Driver.Backend.Output
backendNormalSuccessorPhase :: Backend -> Phase
backendNormalSuccessorPhase ViaC        = HCc
backendNormalSuccessorPhase NCG         = As False
backendNormalSuccessorPhase LLVM        = LlvmOpt
backendNormalSuccessorPhase NoBackend   = StopLn
backendNormalSuccessorPhase Interpreter = StopLn

backendWantsClangTools :: Backend -> Bool
backendWantsClangTools LLVM = True
backendWantsClangTools _ = False

backendPipelineOutput :: Backend -> PipelineOutput
backendPipelineOutput Interpreter = NoOutputFile
backendPipelineOutput NoBackend = NoOutputFile
backendPipelineOutput _ = Persistent

backendCDefs :: Backend -> Logger -> (String, [Option]) -> IO [String]
backendCDefs LLVM logger lc = do
    llvmVer <- figureLlvmVersion logger lc
    return $ case fmap llvmVersionList llvmVer of
               Just [m] -> [ "-D__GLASGOW_HASKELL_LLVM__=" ++ format (m,0) ]
               Just (m:n:_) -> [ "-D__GLASGOW_HASKELL_LLVM__=" ++ format (m,n) ]
               _ -> []
  where
    format (major, minor)
      | minor >= 100 = error "backendCDefs: Unsupported minor version"
      | otherwise = show $ (100 * major + minor :: Int) -- Contract is Int
backendCDefs _ _ _ = return []

figureLlvmVersion :: Logger -> (String, [Option]) -> IO (Maybe LlvmVersion)
figureLlvmVersion logger (pgm, opts) = traceSystoolCommand logger "llc" $ do
  let args = filter notNull (map showOpt opts)
      -- we grab the args even though they should be useless just in
      -- case the user is using a customised 'llc' that requires some
      -- of the options they've specified. llc doesn't care what other
      -- options are specified when '-version' is used.
      args' = args ++ ["-version"]
  catchIO (do
              (pin, pout, perr, p) <- runInteractiveProcess pgm args'
                                              Nothing Nothing
              {- > llc -version
                  LLVM (http://llvm.org/):
                    LLVM version 3.5.2
                    ...
              -}
              hSetBinaryMode pout False
              _     <- hGetLine pout
              vline <- hGetLine pout
              let mb_ver = parseLlvmVersion vline
              hClose pin
              hClose pout
              hClose perr
              _ <- waitForProcess p
              return mb_ver
            )
            (\err -> do
                debugTraceMsg logger 2
                    (text "Error (figuring out LLVM version):" <+>
                      text (show err))
                errorMsg logger $ vcat
                    [ text "Warning:", nest 9 $
                          text "Couldn't figure out LLVM version!" $$
                          text ("Make sure you have installed LLVM between ["
                                ++ llvmVersionStr supportedLlvmVersionLowerBound
                                ++ " and "
                                ++ llvmVersionStr supportedLlvmVersionUpperBound
                                ++ ")") ]
                return Nothing)
