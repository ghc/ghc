{-# LANGUAGE MultiWayIf, LambdaCase #-}

{-|
Module      : GHC.Driver.Backend
Description : Back ends for code generation

This module exports the `Backend` type and all the available values
of that type.  The type is abstract, and GHC assumes a "closed world":
all the back ends are known and are known here.  The compiler driver
chooses a `Backend` value based on how it is asked to generate code.

A `Backend` value encapsulates the knowledge needed to take Cmm, STG,
or Core and write assembly language to a file.  A back end also
provides a function that enables the compiler driver to run an
assembler on the code that is written, if any (the "post-backend
pipeline").  Finally, a back end has myriad /properties/.  Properties
mediate interactions between a back end and the rest of the compiler,
especially the driver.  Examples include the following:

 * Property `backendValidityOfCImport` says whether the back end can
   import foreign C functions.

 * Property `backendForcesOptimization0` says whether the back end can
   be used with optimization levels higher than `-O0`.

 * Property `backendCDefs` tells the compiler driver, "if you're using
   this back end, then these are the command-line flags you should add
   to any invocation of the C compiler."

These properties are used elsewhere in GHC, primarily in the driver, to
fine-tune operations according to the capabilities of the chosen back
end.  You might use a property to make GHC aware of a potential
limitation of certain back ends, or a special feature available only
in certain back ends.  If your client code needs to know a fact that
is not exposed in an existing property, you would define and export a
new property.  Conditioning client code on the /identity/ or /name/ of
a back end is Not Done.

For full details, see the documentation of each property.
-}

module GHC.Driver.Backend
   ( -- * The @Backend@ type
     Backend  -- note: type is abstract
   -- * Available back ends
   , ncgBackend
   , llvmBackend
   , jsBackend
   , viaCBackend
   , interpreterBackend
   , noBackend
   , allBackends

    -- * Types used to specify properties of back ends
   , PrimitiveImplementation(..)
     -- ** Properties that stand for functions
     -- *** Back-end function for code generation
   , DefunctionalizedCodeOutput(..)
     -- *** Back-end functions for assembly
   , DefunctionalizedPostHscPipeline(..)
     -- *** Other back-end functions
   , DefunctionalizedCDefs(..)
     -- ** Names of back ends (for API clients of version 9.4 or earlier)
   , BackendName



     -- * Properties of back ends
   , backendDescription
   , backendWritesFiles
   , backendPipelineOutput
   , backendCanReuseLoadedCode
   , backendGeneratesCode
   , backendGeneratesCodeForHsBoot
   , backendSupportsInterfaceWriting
   , backendRespectsSpecialise
   , backendWantsGlobalBindings
   , backendHasNativeSwitch
   , backendPrimitiveImplementation
   , backendSimdValidity
   , backendSupportsEmbeddedBlobs
   , backendNeedsPlatformNcgSupport
   , backendSupportsUnsplitProcPoints
   , backendSwappableWithViaC
   , backendUnregisterisedAbiOnly
   , backendGeneratesHc
   , backendSptIsDynamic
   , backendSupportsBreakpoints
   , backendForcesOptimization0
   , backendNeedsFullWays
   , backendSpecialModuleSource
   , backendSupportsHpc
   , backendSupportsCImport
   , backendSupportsCExport
   , backendCDefs
   , backendCodeOutput
   , backendUseJSLinker
   , backendPostHscPipeline
   , backendNormalSuccessorPhase
   , backendName
   , backendValidityOfCImport
   , backendValidityOfCExport

   -- * Other functions of back ends
   , platformDefaultBackend
   , platformNcgSupported
   )

where


import GHC.Prelude

import GHC.Driver.Backend.Internal (BackendName(..))
import GHC.Driver.Phases


import GHC.Utils.Error
import GHC.Utils.Panic

import GHC.Driver.Pipeline.Monad
import GHC.Platform


---------------------------------------------------------------------------------
--
--   DESIGN CONSIDERATIONS
--
--
--
-- The `Backend` type is made abstract in order to make it possible to
-- add new back ends without having to inspect or modify much code
-- elsewhere in GHC.  Adding a new back end would be /easiest/ if
-- `Backend` were represented as a record type, but in peer review,
-- the clear will of the majority was to use a sum type.  As a result,
-- when adding a new back end it is necessary to modify /every/
-- function in this module that expects `Backend` as its first argument.
-- **By design, these functions have no default/wildcard cases.** This
-- design forces the author of a new back end to consider the semantics
-- in every case, rather than relying on a default that may be wrong.
-- The names and documentation of the functions defined in the `Backend`
-- record are sufficiently descriptive that the author of a new back
-- end will be able to identify correct result values without having to go
-- spelunking throughout the compiler.
--
-- While the design localizes /most/ back-end logic in this module,
-- the author of a new back end will still have to make changes
-- elsewhere in the compiler:
--
--   * For reasons described in Note [Backend Defunctionalization],
--     code-generation and post-backend pipeline functions, among other
--     functions, cannot be placed in the `Backend` record itself.
--     Instead, the /names/ of those functions are placed.  Each name is
--     a value constructor in one of the algebraic data types defined in
--     this module.  The named function is then defined near its point
--     of use.
--
--     The author of a new back end will have to consider whether an
--     existing function will do or whether a new function needs to be
--     defined.  When a new function needs to be defined, the author
--     must take two steps:
--
--       - Add a value constructor to the relevant data type here
--         in the `Backend` module
--
--       - Add a case to the location in the compiler (there should be
--         exactly one) where the value constructors of the relevant
--         data type are used
--
--   * When a new back end is defined, it's quite possible that the
--     compiler driver will have to be changed in some way.  Just because
--     the driver supports five back ends doesn't mean it will support a sixth
--     without changes.
--
-- The collection of functions exported from this module hasn't
-- really been "designed"; it's what emerged from a refactoring of
-- older code.  The real design criterion was "make it crystal clear
-- what has to be done to add a new back end."
--
-- One issue remains unresolved: some of the error messages and
-- warning messages used in the driver assume a "closed world": they
-- think they know all the back ends that exist, and they are not shy
-- about enumerating them.  Just one set of error messages has been
-- ported to have an open-world assumption: these are the error
-- messages associated with type checking of foreign imports and
-- exports.  To allow other errors to be issued with an open-world
-- assumption, use functions `backendValidityOfCImport` and
-- `backendValidityOfCExport` as models, and have a look at how the
-- 'expected back ends' are used in modules "GHC.Tc.Gen.Foreign" and
-- "GHC.Tc.Errors.Ppr"
--
---------------------------------------------------------------------------------


platformDefaultBackend :: Platform -> Backend
platformDefaultBackend platform = if
      | platformUnregisterised platform -> viaCBackend
      | platformNcgSupported platform   -> ncgBackend
      | platformJSSupported platform    -> jsBackend
      | otherwise                       -> llvmBackend

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
         ArchWasm32    -> True
         ArchRISCV64   -> True
         _             -> False

-- | Is the platform supported by the JS backend?
platformJSSupported :: Platform -> Bool
platformJSSupported platform
  | platformArch platform == ArchJavaScript = True
  | otherwise                               = False


-- | A value of type @Backend@ represents one of GHC's back ends.
-- The set of back ends cannot be extended except by modifying the
-- definition of @Backend@ in this module.
--
-- The @Backend@ type is abstract; that is, its value constructors are
-- not exported.  It's crucial that they not be exported, because a
-- value of type @Backend@ carries only the back end's /name/, not its
-- behavior or properties.  If @Backend@ were not abstract, then code
-- elsewhere in the compiler could depend directly on the name, not on
-- the semantics, which would make it challenging to create a new back end.
-- Because @Backend@ /is/ abstract, all the obligations of a new back
-- end are enumerated in this module, in the form of functions that
-- take @Backend@ as an argument.
--
-- The issue of abstraction is discussed at great length in #20927 and !7442.


newtype Backend = Named BackendName
  -- Must be a newtype so that it has no `Eq` instance and
  -- a different `Show` instance.

-- | The Show instance is for messages /only/.  If code depends on
-- what's in the string, you deserve what happens to you.

instance Show Backend where
  show = backendDescription


ncgBackend, llvmBackend, viaCBackend, interpreterBackend, jsBackend, noBackend
    :: Backend

-- | The native code generator.
-- Compiles Cmm code into textual assembler, then relies on
-- an external assembler toolchain to produce machine code.
--
-- Only supports a few platforms (X86, PowerPC, SPARC).
--
-- See "GHC.CmmToAsm".
ncgBackend = Named NCG

-- | The LLVM backend.
--
-- Compiles Cmm code into LLVM textual IR, then relies on
-- LLVM toolchain to produce machine code.
--
-- It relies on LLVM support for the calling convention used
-- by the NCG backend to produce code objects ABI compatible
-- with it (see "cc 10" or "ghccc" calling convention in
-- https://llvm.org/docs/LangRef.html#calling-conventions).
--
-- Supports a few platforms (X86, AArch64, s390x, ARM).
--
-- See "GHC.CmmToLlvm"
llvmBackend = Named LLVM

-- | The JavaScript Backend
--
-- See documentation in GHC.StgToJS
jsBackend = Named JavaScript

-- | Via-C ("unregisterised") backend.
--
-- Compiles Cmm code into C code, then relies on a C compiler
-- to produce machine code.
--
-- It produces code objects that are /not/ ABI compatible
-- with those produced by NCG and LLVM backends.
--
-- Produced code is expected to be less efficient than the
-- one produced by NCG and LLVM backends because STG
-- registers are not pinned into real registers.  On the
-- other hand, it supports more target platforms (those
-- having a valid C toolchain).
--
-- See "GHC.CmmToC"
viaCBackend = Named ViaC

-- | The ByteCode interpreter.
--
-- Produce ByteCode objects (BCO, see "GHC.ByteCode") that
-- can be interpreted. It is used by GHCi.
--
-- Currently some extensions are not supported
-- (foreign primops).
--
-- See "GHC.StgToByteCode"
interpreterBackend = Named Interpreter

-- | A dummy back end that generates no code.
--
-- Use this back end to disable code generation. It is particularly
-- useful when GHC is used as a library for other purpose than
-- generating code (e.g. to generate documentation with Haddock) or
-- when the user requested it (via `-fno-code`) for some reason.
noBackend = Named NoBackend

---------------------------------------------------------------------------------




-- | This enumeration type specifies how the back end wishes GHC's
-- primitives to be implemented.  (Module "GHC.StgToCmm.Prim" provides
-- a generic implementation of every primitive, but some primitives,
-- like `IntQuotRemOp`, can be implemented more efficiently by
-- certain back ends on certain platforms.  For example, by using a
-- machine instruction that simultaneously computes quotient and remainder.)
--
-- For the meaning of each alternative, consult
-- "GHC.StgToCmm.Config".  (In a perfect world, type
-- `PrimitiveImplementation` would be defined there, in the module
-- that determines its meaning.  But I could not figure out how to do
-- it without mutual recursion across module boundaries.)

data PrimitiveImplementation
    = LlvmPrimitives    -- ^ Primitives supported by LLVM
    | NcgPrimitives     -- ^ Primitives supported by the native code generator
    | JSPrimitives      -- ^ Primitives supported by JS backend
    | GenericPrimitives -- ^ Primitives supported by all back ends
  deriving Show


-- | Names a function that generates code and writes the results to a
--  file, of this type:
--
--  >    Logger
--  > -> DynFlags
--  > -> Module -- ^ module being compiled
--  > -> ModLocation
--  > -> FilePath -- ^ Where to write output
--  > -> Set UnitId -- ^ dependencies
--  > -> Stream IO RawCmmGroup a -- results from `StgToCmm`
--  > -> IO a
--
-- The functions so named are defined in "GHC.Driver.CodeOutput".
--
-- We expect one function per back end—or more precisely, one function
-- for each back end that writes code to a file.  (The interpreter
-- does not write to files; its output lives only in memory.)

data DefunctionalizedCodeOutput
  = NcgCodeOutput
  | ViaCCodeOutput
  | LlvmCodeOutput
  | JSCodeOutput


-- | Names a function that tells the driver what should happen after
-- assembly code is written.  This might include running a C compiler,
-- running LLVM, running an assembler, or various similar activities.
-- The function named normally has this type:
--
-- >    TPipelineClass TPhase m
-- > => PipeEnv
-- > -> HscEnv
-- > -> Maybe ModLocation
-- > -> FilePath
-- > -> m (Maybe FilePath)
--
-- The functions so named are defined in "GHC.Driver.Pipeline".

data DefunctionalizedPostHscPipeline
  = NcgPostHscPipeline
  | ViaCPostHscPipeline
  | LlvmPostHscPipeline
  | JSPostHscPipeline
  | NoPostHscPipeline -- ^ After code generation, nothing else need happen.

-- | Names a function that tells the driver what command-line options
-- to include when invoking a C compiler.  It's meant for @-D@ options that
-- define symbols for the C preprocessor.  Because the exact symbols
-- defined might depend on versions of tools located in the file
-- system (/cough/ LLVM /cough/), the function requires an `IO` action.
-- The function named has this type:
--
-- > Logger -> DynFlags -> IO [String]

data DefunctionalizedCDefs
  = NoCDefs   -- ^ No additional command-line options are needed

  | LlvmCDefs -- ^ Return command-line options that tell GHC about the
              -- LLVM version.

---------------------------------------------------------------------------------



-- | An informal description of the back end, for use in
-- issuing warning messages /only/.  If code depends on
-- what's in the string, you deserve what happens to you.
backendDescription :: Backend -> String
backendDescription (Named NCG)         = "native code generator"
backendDescription (Named LLVM)        = "LLVM"
backendDescription (Named ViaC)        = "compiling via C"
backendDescription (Named JavaScript)  = "compiling to JavaScript"
backendDescription (Named Interpreter) = "byte-code interpreter"
backendDescription (Named NoBackend)   = "no code generated"

-- | This flag tells the compiler driver whether the back
-- end will write files: interface files and object files.
-- It is typically true for "real" back ends that generate
-- code into the filesystem.  (That means, not the interpreter.)
backendWritesFiles :: Backend -> Bool
backendWritesFiles (Named NCG)         = True
backendWritesFiles (Named LLVM)        = True
backendWritesFiles (Named ViaC)        = True
backendWritesFiles (Named JavaScript)  = True
backendWritesFiles (Named Interpreter) = False
backendWritesFiles (Named NoBackend)   = False

-- | When the back end does write files, this value tells
-- the compiler in what manner of file the output should go:
-- temporary, persistent, or specific.
backendPipelineOutput :: Backend -> PipelineOutput
backendPipelineOutput (Named NCG)  = Persistent
backendPipelineOutput (Named LLVM) = Persistent
backendPipelineOutput (Named ViaC) = Persistent
backendPipelineOutput (Named JavaScript)  = Persistent
backendPipelineOutput (Named Interpreter) = NoOutputFile
backendPipelineOutput (Named NoBackend)   = NoOutputFile

-- | This flag tells the driver whether the back end can
-- reuse code (bytecode or object code) that has been
-- loaded dynamically.  Likely true only of the interpreter.
backendCanReuseLoadedCode :: Backend -> Bool
backendCanReuseLoadedCode (Named NCG)         = False
backendCanReuseLoadedCode (Named LLVM)        = False
backendCanReuseLoadedCode (Named ViaC)        = False
backendCanReuseLoadedCode (Named JavaScript)  = False
backendCanReuseLoadedCode (Named Interpreter) = True
backendCanReuseLoadedCode (Named NoBackend)   = False

-- | It is is true of every back end except @-fno-code@
-- that it "generates code."  Surprisingly, this property
-- influences the driver in a ton of ways.  Some examples:
--
--   * If the back end does not generate code, then the
--     driver needs to turn on code generation for
--     Template Haskell (because that code needs to be
--     generated and run at compile time).
--
--   * If the back end does not generate code, then the
--     driver does not need to deal with an output file.
--
--   * If the back end /does/ generated code, then the
--     driver supports `HscRecomp`.  If not, recompilation
--     does not need a linkable (and is automatically up
--     to date).
--
backendGeneratesCode :: Backend -> Bool
backendGeneratesCode (Named NCG)         = True
backendGeneratesCode (Named LLVM)        = True
backendGeneratesCode (Named ViaC)        = True
backendGeneratesCode (Named JavaScript)  = True
backendGeneratesCode (Named Interpreter) = True
backendGeneratesCode (Named NoBackend)   = False

backendGeneratesCodeForHsBoot :: Backend -> Bool
backendGeneratesCodeForHsBoot (Named NCG)         = True
backendGeneratesCodeForHsBoot (Named LLVM)        = True
backendGeneratesCodeForHsBoot (Named ViaC)        = True
backendGeneratesCodeForHsBoot (Named JavaScript)  = True
backendGeneratesCodeForHsBoot (Named Interpreter) = False
backendGeneratesCodeForHsBoot (Named NoBackend)   = False

-- | When set, this flag turns on interface writing for
-- Backpack.  It should probably be the same as
-- `backendGeneratesCode`, but it is kept distinct for
-- reasons described in Note [-fno-code mode].
backendSupportsInterfaceWriting :: Backend -> Bool
backendSupportsInterfaceWriting (Named NCG)         = True
backendSupportsInterfaceWriting (Named LLVM)        = True
backendSupportsInterfaceWriting (Named ViaC)        = True
backendSupportsInterfaceWriting (Named JavaScript)  = True
backendSupportsInterfaceWriting (Named Interpreter) = True
backendSupportsInterfaceWriting (Named NoBackend)   = False

-- | When preparing code for this back end, the type
-- checker should pay attention to SPECIALISE pragmas.  If
-- this flag is `False`, then the type checker ignores
-- SPECIALISE pragmas (for imported things?).
backendRespectsSpecialise :: Backend -> Bool
backendRespectsSpecialise (Named NCG)         = True
backendRespectsSpecialise (Named LLVM)        = True
backendRespectsSpecialise (Named ViaC)        = True
backendRespectsSpecialise (Named JavaScript)  = True
backendRespectsSpecialise (Named Interpreter) = False
backendRespectsSpecialise (Named NoBackend)   = False

-- | This back end wants the `mi_top_env` field of a
-- `ModIface` to be populated (with the top-level bindings
-- of the original source).  Only true for the interpreter.
backendWantsGlobalBindings :: Backend -> Bool
backendWantsGlobalBindings (Named NCG)         = False
backendWantsGlobalBindings (Named LLVM)        = False
backendWantsGlobalBindings (Named ViaC)        = False
backendWantsGlobalBindings (Named JavaScript)  = False
backendWantsGlobalBindings (Named NoBackend)   = False
backendWantsGlobalBindings (Named Interpreter) = True

-- | The back end targets a technology that implements
-- `switch` natively.  (For example, LLVM or C.) Therefore
-- it is not necessary for GHC to ccompile a Cmm `Switch`
-- form into a decision tree with jump tables at the
-- leaves.
backendHasNativeSwitch :: Backend -> Bool
backendHasNativeSwitch (Named NCG)         = False
backendHasNativeSwitch (Named LLVM)        = True
backendHasNativeSwitch (Named ViaC)        = True
backendHasNativeSwitch (Named JavaScript)  = True
backendHasNativeSwitch (Named Interpreter) = False
backendHasNativeSwitch (Named NoBackend)   = False

-- | As noted in the documentation for
-- `PrimitiveImplementation`, certain primitives have
-- multiple implementations, depending on the capabilities
-- of the back end.  This field signals to module
-- "GHC.StgToCmm.Prim" what implementations to use with
-- this back end.
backendPrimitiveImplementation :: Backend -> PrimitiveImplementation
backendPrimitiveImplementation (Named NCG)         = NcgPrimitives
backendPrimitiveImplementation (Named LLVM)        = LlvmPrimitives
backendPrimitiveImplementation (Named JavaScript)  = JSPrimitives
backendPrimitiveImplementation (Named ViaC)        = GenericPrimitives
backendPrimitiveImplementation (Named Interpreter) = GenericPrimitives
backendPrimitiveImplementation (Named NoBackend)   = GenericPrimitives

-- | When this value is `IsValid`, the back end is
-- compatible with vector instructions.  When it is
-- `NotValid`, it carries a message that is shown to
-- users.
backendSimdValidity :: Backend -> Validity' String
backendSimdValidity (Named NCG)         = NotValid $ unlines ["SIMD vector instructions require the LLVM back-end.","Please use -fllvm."]
backendSimdValidity (Named LLVM)        = IsValid
backendSimdValidity (Named ViaC)        = NotValid $ unlines ["SIMD vector instructions require the LLVM back-end.","Please use -fllvm."]
backendSimdValidity (Named JavaScript)  = NotValid $ unlines ["SIMD vector instructions require the LLVM back-end.","Please use -fllvm."]
backendSimdValidity (Named Interpreter) = NotValid $ unlines ["SIMD vector instructions require the LLVM back-end.","Please use -fllvm."]
backendSimdValidity (Named NoBackend)   = NotValid $ unlines ["SIMD vector instructions require the LLVM back-end.","Please use -fllvm."]

-- | This flag says whether the back end supports large
-- binary blobs.  See Note [Embedding large binary blobs]
-- in "GHC.CmmToAsm.Ppr".
backendSupportsEmbeddedBlobs :: Backend -> Bool
backendSupportsEmbeddedBlobs (Named NCG)         = True
backendSupportsEmbeddedBlobs (Named LLVM)        = False
backendSupportsEmbeddedBlobs (Named ViaC)        = False
backendSupportsEmbeddedBlobs (Named JavaScript)  = False
backendSupportsEmbeddedBlobs (Named Interpreter) = False
backendSupportsEmbeddedBlobs (Named NoBackend)   = False

-- | This flag tells the compiler driver that the back end
-- does not support every target platform; it supports
-- only platforms that claim NCG support.  (It's set only
-- for the native code generator.)  Crufty.  If the driver
-- tries to use the native code generator /without/
-- platform support, the driver fails over to the LLVM
-- back end.
backendNeedsPlatformNcgSupport :: Backend -> Bool
backendNeedsPlatformNcgSupport (Named NCG)         = True
backendNeedsPlatformNcgSupport (Named LLVM)        = False
backendNeedsPlatformNcgSupport (Named ViaC)        = False
backendNeedsPlatformNcgSupport (Named JavaScript)  = False
backendNeedsPlatformNcgSupport (Named Interpreter) = False
backendNeedsPlatformNcgSupport (Named NoBackend)   = False

-- | This flag is set if the back end can generate code
-- for proc points.  If the flag is not set, then a Cmm
-- pass needs to split proc points (that is, turn each
-- proc point into a standalone procedure).
backendSupportsUnsplitProcPoints :: Backend -> Bool
backendSupportsUnsplitProcPoints (Named NCG)         = True
backendSupportsUnsplitProcPoints (Named LLVM)        = False
backendSupportsUnsplitProcPoints (Named ViaC)        = False
backendSupportsUnsplitProcPoints (Named JavaScript)  = False
backendSupportsUnsplitProcPoints (Named Interpreter) = False
backendSupportsUnsplitProcPoints (Named NoBackend)   = False

-- | This flag guides the driver in resolving issues about
-- API support on the target platform. If the flag is set,
-- then these things are true:
--
--    * When the target platform supports /only/ an unregisterised API,
--      this backend can be replaced with compilation via C.
--
--    * When the target does /not/ support an unregisterised API,
--      this back end can replace compilation via C.
--
backendSwappableWithViaC :: Backend -> Bool
backendSwappableWithViaC (Named NCG)         = True
backendSwappableWithViaC (Named LLVM)        = True
backendSwappableWithViaC (Named ViaC)        = False
backendSwappableWithViaC (Named JavaScript)  = False
backendSwappableWithViaC (Named Interpreter) = False
backendSwappableWithViaC (Named NoBackend)   = False

-- | This flag is true if the back end works *only* with
-- the unregisterised ABI.
backendUnregisterisedAbiOnly :: Backend -> Bool
backendUnregisterisedAbiOnly (Named NCG)         = False
backendUnregisterisedAbiOnly (Named LLVM)        = False
backendUnregisterisedAbiOnly (Named ViaC)        = True
backendUnregisterisedAbiOnly (Named JavaScript)  = False
backendUnregisterisedAbiOnly (Named Interpreter) = False
backendUnregisterisedAbiOnly (Named NoBackend)   = False

-- | This flag is set if the back end generates C code in
-- a @.hc@ file.  The flag lets the compiler driver know
-- if the command-line flag @-C@ is meaningful.
backendGeneratesHc :: Backend -> Bool
backendGeneratesHc (Named NCG)         = False
backendGeneratesHc (Named LLVM)        = False
backendGeneratesHc (Named ViaC)        = True
backendGeneratesHc (Named JavaScript)  = False
backendGeneratesHc (Named Interpreter) = False
backendGeneratesHc (Named NoBackend)   = False

-- | This flag says whether SPT (static pointer table)
-- entries will be inserted dynamically if needed.  If
-- this flag is `False`, then "GHC.Iface.Tidy" should emit C
-- stubs that initialize the SPT entries.
backendSptIsDynamic :: Backend -> Bool
backendSptIsDynamic (Named NCG)         = False
backendSptIsDynamic (Named LLVM)        = False
backendSptIsDynamic (Named ViaC)        = False
backendSptIsDynamic (Named JavaScript)  = False
backendSptIsDynamic (Named Interpreter) = True
backendSptIsDynamic (Named NoBackend)   = False

-- | If this flag is unset, then the driver ignores the flag @-fbreak-points@,
-- since backends other than the interpreter tend to panic on breakpoints.
backendSupportsBreakpoints :: Backend -> Bool
backendSupportsBreakpoints = \case
  Named NCG         -> False
  Named LLVM        -> False
  Named ViaC        -> False
  Named JavaScript  -> False
  Named Interpreter -> True
  Named NoBackend   -> False

-- | If this flag is set, then the driver forces the
-- optimization level to 0, issuing a warning message if
-- the command line requested a higher optimization level.
backendForcesOptimization0 :: Backend -> Bool
backendForcesOptimization0 (Named NCG)         = False
backendForcesOptimization0 (Named LLVM)        = False
backendForcesOptimization0 (Named ViaC)        = False
backendForcesOptimization0 (Named JavaScript)  = False
backendForcesOptimization0 (Named Interpreter) = True
backendForcesOptimization0 (Named NoBackend)   = False

-- | I don't understand exactly how this works.  But if
-- this flag is set *and* another condition is met, then
-- @ghc/Main.hs@ will alter the `DynFlags` so that all the
-- `hostFullWays` are asked for.  It is set only for the interpreter.
backendNeedsFullWays :: Backend -> Bool
backendNeedsFullWays (Named NCG)         = False
backendNeedsFullWays (Named LLVM)        = False
backendNeedsFullWays (Named ViaC)        = False
backendNeedsFullWays (Named JavaScript)  = False
backendNeedsFullWays (Named Interpreter) = True
backendNeedsFullWays (Named NoBackend)   = False

-- | This flag is also special for the interpreter: if a
-- message about a module needs to be shown, do we know
-- anything special about where the module came from?  The
-- Boolean argument is a `recomp` flag.
backendSpecialModuleSource :: Backend -> Bool -> Maybe String
backendSpecialModuleSource (Named NCG)         = const Nothing
backendSpecialModuleSource (Named LLVM)        = const Nothing
backendSpecialModuleSource (Named ViaC)        = const Nothing
backendSpecialModuleSource (Named JavaScript)  = const Nothing
backendSpecialModuleSource (Named Interpreter) = \b -> if b then Just "interpreted" else Nothing
backendSpecialModuleSource (Named NoBackend)   = const (Just "nothing")

-- | This flag says whether the back end supports Haskell
-- Program Coverage (HPC). If not, the compiler driver
-- will ignore the `-fhpc` option (and will issue a
-- warning message if it is used).
backendSupportsHpc :: Backend -> Bool
backendSupportsHpc (Named NCG)         = True
backendSupportsHpc (Named LLVM)        = True
backendSupportsHpc (Named ViaC)        = True
backendSupportsHpc (Named JavaScript)  = False
backendSupportsHpc (Named Interpreter) = False
backendSupportsHpc (Named NoBackend)   = True

-- | This flag says whether the back end supports foreign
-- import of C functions.  ("Supports" means "does not
-- barf on," so @-fno-code@ supports foreign C imports.)
backendSupportsCImport :: Backend -> Bool
backendSupportsCImport (Named NCG)         = True
backendSupportsCImport (Named LLVM)        = True
backendSupportsCImport (Named ViaC)        = True
backendSupportsCImport (Named JavaScript)  = True
backendSupportsCImport (Named Interpreter) = True
backendSupportsCImport (Named NoBackend)   = True

-- | This flag says whether the back end supports foreign
-- export of Haskell functions to C.
backendSupportsCExport :: Backend -> Bool
backendSupportsCExport (Named NCG)         = True
backendSupportsCExport (Named LLVM)        = True
backendSupportsCExport (Named ViaC)        = True
backendSupportsCExport (Named JavaScript)  = True
backendSupportsCExport (Named Interpreter) = False
backendSupportsCExport (Named NoBackend)   = True

-- | When using this back end, it may be necessary or
-- advisable to pass some `-D` options to a C compiler.
-- This (defunctionalized) function produces those
-- options, if any.  An IO action may be necessary in
-- order to interrogate external tools about what version
-- they are, for example.
--
-- The function's type is
-- @
-- Logger -> DynFlags -> IO [String]
-- @
--
-- This field is usually defaulted.
backendCDefs :: Backend -> DefunctionalizedCDefs
backendCDefs (Named NCG)         = NoCDefs
backendCDefs (Named LLVM)        = LlvmCDefs
backendCDefs (Named ViaC)        = NoCDefs
backendCDefs (Named JavaScript)  = NoCDefs
backendCDefs (Named Interpreter) = NoCDefs
backendCDefs (Named NoBackend)   = NoCDefs

-- | This (defunctionalized) function generates code and
-- writes it to a file.  The type of the function is
--
-- >    Logger
-- > -> DynFlags
-- > -> Module -- ^ module being compiled
-- > -> ModLocation
-- > -> FilePath -- ^ Where to write output
-- > -> Set UnitId -- ^ dependencies
-- > -> Stream IO RawCmmGroup a -- results from `StgToCmm`
-- > -> IO a
backendCodeOutput :: Backend -> DefunctionalizedCodeOutput
backendCodeOutput (Named NCG)         = NcgCodeOutput
backendCodeOutput (Named LLVM)        = LlvmCodeOutput
backendCodeOutput (Named ViaC)        = ViaCCodeOutput
backendCodeOutput (Named JavaScript)  = JSCodeOutput
backendCodeOutput (Named Interpreter) = panic "backendCodeOutput: interpreterBackend"
backendCodeOutput (Named NoBackend)   = panic "backendCodeOutput: noBackend"

backendUseJSLinker :: Backend -> Bool
backendUseJSLinker (Named NCG)         = False
backendUseJSLinker (Named LLVM)        = False
backendUseJSLinker (Named ViaC)        = False
backendUseJSLinker (Named JavaScript)  = True
backendUseJSLinker (Named Interpreter) = False
backendUseJSLinker (Named NoBackend)   = False

-- | This (defunctionalized) function tells the compiler
-- driver what else has to be run after code output.
-- The type of the function is
--
-- >
-- >    TPipelineClass TPhase m
-- > => PipeEnv
-- > -> HscEnv
-- > -> Maybe ModLocation
-- > -> FilePath
-- > -> m (Maybe FilePath)
backendPostHscPipeline :: Backend -> DefunctionalizedPostHscPipeline
backendPostHscPipeline (Named NCG)  = NcgPostHscPipeline
backendPostHscPipeline (Named LLVM) = LlvmPostHscPipeline
backendPostHscPipeline (Named ViaC) = ViaCPostHscPipeline
backendPostHscPipeline (Named JavaScript)  = JSPostHscPipeline
backendPostHscPipeline (Named Interpreter) = NoPostHscPipeline
backendPostHscPipeline (Named NoBackend) = NoPostHscPipeline

-- | Somewhere in the compiler driver, when compiling
-- Haskell source (as opposed to a boot file or a sig
-- file), it needs to know what to do with the code that
-- the `backendCodeOutput` writes to a file.  This `Phase`
-- value gives instructions like "run the C compiler",
-- "run the assembler," or "run the LLVM Optimizer."
backendNormalSuccessorPhase :: Backend -> Phase
backendNormalSuccessorPhase (Named NCG)  = As False
backendNormalSuccessorPhase (Named LLVM) = LlvmOpt
backendNormalSuccessorPhase (Named ViaC) = HCc
backendNormalSuccessorPhase (Named JavaScript)  = StopLn
backendNormalSuccessorPhase (Named Interpreter) = StopLn
backendNormalSuccessorPhase (Named NoBackend)   = StopLn

-- | Name of the back end, if any.  Used to migrate legacy
-- clients of the GHC API.  Code within the GHC source
-- tree should not refer to a back end's name.
backendName :: Backend -> BackendName
backendName (Named NCG)  = NCG
backendName (Named LLVM) = LLVM
backendName (Named ViaC) = ViaC
backendName (Named JavaScript)  = JavaScript
backendName (Named Interpreter) = Interpreter
backendName (Named NoBackend)   = NoBackend



-- | A list of all back ends.  They are ordered as we wish them to
-- appear when they are enumerated in error messages.

allBackends :: [Backend]
allBackends = [ ncgBackend
              , llvmBackend
              , viaCBackend
              , jsBackend
              , interpreterBackend
              , noBackend
              ]

-- | When foreign C import or export is invalid, the carried value
-- enumerates the /valid/ back ends.

backendValidityOfCImport, backendValidityOfCExport :: Backend -> Validity' [Backend]

backendValidityOfCImport backend =
    if backendSupportsCImport backend then
        IsValid
    else
        NotValid $ filter backendSupportsCImport allBackends

backendValidityOfCExport backend =
    if backendSupportsCExport backend then
        IsValid
    else
        NotValid $ filter backendSupportsCExport allBackends




{-
Note [Backend Defunctionalization]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
I had hoped to include code-output and post-hsc-pipeline functions
directly in the `Backend` record itself.  But this agenda was derailed
by mutual recursion in the types:

  - A `DynFlags` record contains a back end of type `Backend`.
  - A `Backend` contains a code-output function.
  - A code-output function takes Cmm as input.
  - Cmm can include a `CLabel`.
  - A `CLabel` can have elements that are defined in
    `GHC.Driver.Session`, where `DynFlags` is defined.

There is also a nasty issue in the values: a typical post-backend
pipeline function both depends on and is depended upon by functions in
"GHC.Driver.Pipeline".

I'm cut the Gordian not by removing the function types from the
`Backend` record.  Instead, a function is represented by its /name/.
This representation is an example of an old trick called
/defunctionalization/, which has been used in both compilers and
interpreters for languages with first-class, nested functions.  Here,
a function's name is a value of an algebraic data type.  For example,
a code-output function is represented by a value of this type:

    data DefunctionalizedCodeOutput
      = NcgCodeOutput
      | ViaCCodeOutput
      | LlvmCodeOutput

Such a function may be applied in one of two ways:

  - In this particular example, a `case` expression in module
    "GHC.Driver.CodeOutput" discriminates on the value and calls the
    designated function.

  - In another example, a function of type `DefunctionalizedCDefs` is
    applied by calling function `applyCDefs`, which has this type:

    @
    applyCDefs :: DefunctionalizedCDefs -> Logger -> DynFlags -> IO [String]
    @

    Function `applyCDefs` is defined in module "GHC.SysTools.Cpp".

I don't love this solution, but defunctionalization is a standard
thing, and it makes the meanings of the enumeration values clear.

Anyone defining a new back end will need to extend both the
`DefunctionalizedCodeOutput` type and the corresponding apply
function.
-}
