{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

-------------------------------------------------------------------------------
--
-- | What constitutes a back end for code generation
--
-- This module defines the `Backend` type.  A back end is responsible
-- for generating code, which means taking Cmm (and a compiled module)
-- and writing assembly language out to a file.  A back end also provides
-- a function that enables the compiler driver to run an assembler
-- on the code that is written (the "post-backend pipeline").
-- Finally, a back end has myriad /properties/.  These properties fall
-- into two categories:
--
--  * Properties that tell the compiler driver what `DynFlags` the
--    back end is compatible with, or how to adjust `DynFlags`.
--
--  * Properties (or even functions) that give the compiler driver
--    little jobs to do.  For example, the `backendCDefs` field tells
--    the compiler driver, "if you're using this back end, then these
--    are the command-line flags you should add to any invocation of
--    the C compiler."
--
--
-- This module does not define any back ends; for that you want "GHC.Driver.Backend".
-- You might start with the `prototypeBackend` or with one of the
-- existing back ends.
--
-------------------------------------------------------------------------------



---------------------------------------------------------------------------------
--
--   DESIGN CONSIDERATIONS
--
--
--
-- The abstractions in this module are designed with one goal in mind:
-- make it easy to add new back ends.  In a perfect world, easy would mean:
--
--   * Changes are localized: A new back end needs only the `Backend`
--     record itself and a means of telling the driver to put that record
--     in the `backend` field of the `DynFlags`.
--
--   * The names and documentation of the property fields in the `Backend`
--     record are sufficiently descriptive that the author of a new back
--     end will be able to identify correct values without having to go
--     spelunking throughout the compiler.
--
-- In practice, this ideal is not necessarily achieved:
--
--   * For reasons described in Note [Backend Defunctionalization],
--     code-generation and post-backend pipeline functions, among other
--     functions, cannot be placed in the `Backend` record itself.
--     Instead, the /names/ of those functions are placed.  Each name is
--     a value constructor in one of the algebraic data types defined in
--     this module.  The named function must then be defined in
--     "GHC.Driver.Backend.Refunctionalize" or in "GHC.Driver.Pipeline".
--
--   * When a new back end is defined, it's quite possible that the
--     compiler driver will have to be changed in some way.  Just because
--     it supports five back ends doesn't mean it will support a sixth
--     without changes.
--
--   * We don't yet know how good the names and documentation of the
--     property fields are.
--
-- The abstraction itself is inspired partly by GHC's `Platform` type and
-- partly by the /interface record/ of the [@lcc@
-- compiler](https://github.com/drh/lcc).
-- See /A Retargetable C Compiler/ by Chris Fraser and David R. Hanson.
--
-- This particular collection of properties and functions hasn't
-- really been "designed"; it's what emerged from a refactoring of
-- older code.  The real design criterion was "make it crystal clear
-- what has to be done to add a new back end."
--
--
-- One issue remains unresolved: the error messages and warning
-- messages used in the driver very much assume a "closed world": they
-- think they know all the back ends that exist, and they are not shy
-- about enumerating them.  For example, if a back end does not
-- support the export of functions to C, the message says,
--
-- > requires unregisterised, llvm (-fllvm) or native code generation (-fasm)
--
-- If the next back end added to GHC /does/ support export of
-- functions to C, I'm not sure how the author knows what messages
-- they need to change.

---------------------------------------------------------------------------------


module GHC.Driver.Backend.Types
   ( -- * Representation of a back end
     Backend(..)
     -- * Specialized types of properties
   , PrimitiveImplementation(..)
     -- * Functions that appear in back ends
     -- ** Back-end function for code generation
   , DefunctionalizedCodeOutput(..)
     -- ** Back-end functions for assembly
   , DefunctionalizedPostHscPipeline(..)
   , DefunctionalizedAssemblerProg(..)
   , DefunctionalizedAssemblerInfoGetter(..)
     -- ** Other back-end functions
   , DefunctionalizedCDefs(..)
   )

where

import GHC.Prelude

import GHC.Driver.Phases
import GHC.Driver.Pipeline.Monad
import GHC.Utils.Error


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
    = LlvmPrimitives
    | NcgPrimitives
    | GenericPrimitives
  deriving Show


-- | Names a function that runs the assembler, of this type:
--
-- > Logger -> DynFlags -> Platform -> [Option] -> IO ()

data DefunctionalizedAssemblerProg
  = StandardAssemblerProg
       -- ^ Use the standard system assembler
  | DarwinClangAssemblerProg
       -- ^ If running on Darwin, use the assembler from the @clang@
       -- toolchain.  Otherwise use the standard system assembler.



-- | Names a function that discover from what toolchain the assembler
-- is coming, of this type:
--
-- > Logger -> DynFlags -> Platform -> IO CompilerInfo

data DefunctionalizedAssemblerInfoGetter
  = StandardAssemblerInfoGetter
       -- ^ Interrogate the standard system assembler
  | DarwinClangAssemblerInfoGetter
       -- ^ If running on Darwin, return `Clang`; otherwise
       -- interrogate the standard system assembler.


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
-- There will be one function per back end---or more precisely, one
-- function for each back end that writes code to a file.  (The
-- interpreter does not; its output lives only in memory.)

data DefunctionalizedCodeOutput
  = NcgCodeOutput
  | ViaCCodeOutput
  | LlvmCodeOutput


-- | Names a function that tells the driver what should happen after
-- assembly code is written.  This might include running a C compiler,
-- running LLVM, running an assembler, or various similar activities.
-- The function named has this type:
--
-- >    TPipelineClass TPhase m
-- > => PipeEnv
-- > -> HscEnv
-- > -> Maybe ModLocation
-- > -> FilePath
-- > -> m (Maybe FilePath)
--
-- Unlike the other named functions, which are defined in
-- "GHC.Driver.Backend.Refunctionalize", these functions have to be
-- defined in "GHC.Driver.Pipeline", because they depend on functions
-- defined there and are depended upon by functions defined there.
--
-- There is one function per back end.

data DefunctionalizedPostHscPipeline
  = NcgPostHscPipeline
  | ViaCPostHscPipeline
  | LlvmPostHscPipeline
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


-- | The properties of and functions performed by a back end.


data Backend =
    Backend {
            -- | An informal description of the back end, for use in
            -- issuing warning messages /only/.  If code depends on
            -- what's in the string, you deserve what happens to you.
              backendDescription :: String

            -- | This flag tells the compiler driver whether the back
            -- end will write files: interface files and object files.
            -- It is typically true for "real" back ends that generate
            -- code into the filesystem.  (That means, not the interpreter.)
            , backendWritesFiles :: !Bool

            -- | When the back end does write files, this value tells
            -- the compiler in what manner of file the output should go:
            -- temporary, persistent, or specific.
            , backendPipelineOutput :: PipelineOutput

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
            , backendGeneratesCode :: !Bool

            -- ^ When set, this flag turns on interface writing for
            -- Backpack.  It should probably be the same as
            -- `backendGeneratesCode`, but it is kept distinct for
            -- reasons described in Note [-fno-code mode].
            , backendSupportsInterfaceWriting :: !Bool

            -- | When preparing code for this back end, the type
            -- checker should pay attention to SPECIALISE pragmas.  If
            -- this flag is `False`, then the type checker ignores
            -- SPECIALISE pragmas (for imported things?).
            , backendRespectsSpecialise :: !Bool


            -- | This back end wants the `mi_globals` field of a
            -- `ModIface` to be populated (with the top-level bindings
            -- of the original source).  True for the interpreter, and
            -- also true for "no backend", which is used by Haddock.
            -- (After typechecking a module, Haddock wants access to
            -- the module's `GlobalRdrEnv`.)
            , backendWantsGlobalBindings :: !Bool


            -- | The back end targets a technology that implements
            -- `switch` natively.  (For example, LLVM or C.) Therefore
            -- it is not necessary for GHC to ccompile a Cmm `Switch`
            -- form into a decision tree with jump tables at the
            -- leaves.
            , backendHasNativeSwitch :: !Bool

            -- | As noted in the documentation for
            -- `PrimitiveImplementation`, certain primitives have
            -- multiple implementations, depending on the capabilities
            -- of the back end.  This field signals to module
            -- "GHC.StgToCmm.Prim" what implementations to use with
            -- this back end.
            , backendPrimitiveImplementation :: !PrimitiveImplementation


            -- | When this value is `IsValid`, the back end is
            -- compatible with vector instructions.  When it is
            -- `NotValid`, it carries a message that is shown to
            -- users.
            , backendSimdValidity :: Validity' String

            -- | This flag says whether the back end supports large
            -- binary blobs.  See Note [Embedding large binary blobs]
            -- in "GHC.CmmToAsm.Ppr".
            , backendSupportsEmbeddedBlobs :: !Bool


            -- | This flag tells the compiler driver that the back end
            -- does not support every target platform; it supports
            -- only platforms that claim NCG support.  (It's set only
            -- for the native code generator.)  Crufty.  If the driver
            -- tries to use the native code generator *without*
            -- platform support, the driver fails over to the LLVM
            -- back end.
            , backendNeedsPlatformNcgSupport :: !Bool


            -- | This flag is set if the back end can generate code
            -- for proc points.  If the flag is not set, then a Cmm
            -- pass needs to split proc points (that is, turn each
            -- proc point into a standalone procedure).
            , backendSupportsUnsplitProcPoints :: !Bool


            -- | This flag guides the driver in resolving issues about
            -- API support on the target platform. If the flag is set,
            -- then these things are true:
            --
            --    * When the target platform supports *only* an unregisterised API,
            --      this backend can be replaced with compilation via C.
            --
            --    * When the target does *not* support an unregisterised API,
            --      this back end can replace compilation via C.
            --
            , backendSwappableWithViaC :: !Bool


            -- | This flag is true if the back end works *only* with
            -- the unregisterised ABI.
            , backendUnregisterisedAbiOnly :: !Bool

            -- | This flag is set if the back end generates C code in
            -- a @.hc@ file.  The flag lets the compiler driver know
            -- if the command-line flag @-C@ is meaningful.
            , backendGeneratesHc :: !Bool


            -- The next four flags are True only for the interpreter.

            -- | This flag says whether SPT (static pointer table)
            -- entries will be inserted dynamically if needed.  If
            -- this flag is `False`, then "GHC.Iface.Tidy" should emit C
            -- stubs that initialize the SPT entries.
            , backendSptIsDynamic :: !Bool

            -- | If this flag is set, then "GHC.HsToCore.Coverage"
            -- inserts `Breakpoint` ticks.  Used only for the
            -- interpreter.
            , backendWantsBreakpointTicks :: !Bool

            -- | If this flag is set, then the driver forces the
            -- optimization level to 0, issuing a warning message if
            -- the command line requested a higher optimization level.
            , backendForcesOptimization0 :: !Bool

            -- | I don't understand exactly how this works.  But if
            -- this flag is set *and* another condition is met, then
            -- @ghc/Main.hs@ will alter the `DynFlags` so that all the
            -- `hostFullWays` are asked for.  It is set only for the interpreter.
            , backendNeedsFullWays :: !Bool


            -- | This flag is also special for the interpreter: if a
            -- message about a module needs to be shown, do we know
            -- anything special about where the module came from?  The
            -- Boolean argument is a `recomp` flag.
            , backendSpecialModuleSource :: Bool -> Maybe String



            -- | This flag says whether the back end supports Haskell
            -- Program Coverage (HPC). If not, the compiler driver
            -- will ignore the `-fhpc` option (and will issue a
            -- warning message if it is used).
            , backendSupportsHpc :: !Bool



            -- | This flag says whehther the back end supports foreign
            -- import of C functions.  ("Supports" means "does not
            -- barf on," so @-fno-code@ supports foreign C imports.)
            , backendValidityOfCImport :: !Validity


            -- | This flag says whehther the back end supports foreign
            -- export of Haskell functions to C.
            , backendValidityOfCExport :: !Validity



            ----------------- supporting tooling

            -- | This (defunctionalized) function runs the assembler
            -- used on the code that is written by this back end.  A
            -- program determined by a combination of back end,
            -- `DynFlags`, and `Platform` is run with the given
            -- `Option`s.
            --
            -- This field is usually defaulted.
            , backendAssemblerProg :: DefunctionalizedAssemblerProg
              -- ^ Logger -> DynFlags -> Platform -> [Option] -> IO ()

            -- | This (defunctionalized) function is used to retrieve
            -- an enumeration value that characterizes the C/assembler
            -- part of a toolchain.  The function caches the info in a
            -- mutable variable that is part of the `DynFlags`.
            --
            -- This field is usually defaulted.
            , backendAssemblerInfoGetter :: DefunctionalizedAssemblerInfoGetter
                 -- ^ Logger -> DynFlags -> Platform -> IO CompilerInfo


            -- | When using this back end, it may be necessary or
            -- advisable to pass some `-D` options to a C compiler.
            -- This (defunctionalized) function produces those
            -- options, if any.  An IO action may be necessary in
            -- order to interrogate external tools about what version
            -- they are, for example.
            --
            -- This field is usually defaulted.
            , backendCDefs :: DefunctionalizedCDefs
               -- ^ Logger -> DynFlags -> IO [String]


            ----------------- code generation and compiler driver

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
            , backendCodeOutput :: DefunctionalizedCodeOutput


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
            , backendPostHscPipeline :: DefunctionalizedPostHscPipeline

            -- | Somewhere in the compiler driver, when compiling
            -- Haskell source (as opposed to a boot file or a sig
            -- file), it needs to know what to do with the code that
            -- the `backendCodeOutput` writes to a file.  This `Phase`
            -- value gives instructions like "run the C compiler",
            -- "run the assembler," or "run the LLVM Optimizer."
            , backendNormalSuccessorPhase :: Phase

            }


-- | The Show instance is for messages *only*.  If code depends on
-- what's in the string, you deserve what happens to you.

instance Show Backend where
  show = backendDescription


----------------------------------------------------------------
--
-- Note [Backend Defunctionalization]
--
-- I had hoped to include code-output and post-hsc-pipeline functions
-- directly in the `Backend` record itself.  But this agenda was derailed
-- by mutual recursion in the types:
--
--   - A `DynFlags` record contains a back end of type `Backend`.
--   - A `Backend` contains a code-output function.
--   - A code-output function takes Cmm as input.
--   - Cmm can include a `CLabel`.
--   - A `CLabel` can have elements that are defined in
--     `GHC.Driver.Session`, where `DynFlags` is defined.
--
-- There is also a nasty issue in the values: a typical post-backend
-- pipeline function both depends on and is depended upon by functions in
-- "GHC.Driver.Pipeline".
--
-- I'm cut the Gordian not by removing the function types from the
-- `Backend` record.  Instead, a function is represented by its /name/.
-- This representation is an example of an old trick called
-- /defunctionalization/, which has been used in both compilers and
-- interpreters for languages with first-class, nested functions.  Here,
-- a function's name is a value of an algebraic data type.  For example,
-- a code-output function is represented by a value of this type:
--
--     data DefunctionalizedCodeOutput
--       = NcgCodeOutput
--       | ViaCCodeOutput
--       | LlvmCodeOutput
--
-- To apply the named function, one uses module
-- "Driver.Backend.Refunctionalize", which exports this function:
--
--     applyCodeOutput
--         :: DefunctionalizedCodeOutput
--         -> Logger
--         -> DynFlags
--         -> Module
--         -> ModLocation
--         -> FilePath
--         -> Set UnitId
--         -> Stream IO RawCmmGroup a
--         -> IO a
--
-- I don't love this solution, but defunctionalization is a standard
-- thing, and it makes the meanings of the enumeration values clear.
--
-- Anyone defining a new back end will need to extend both the
-- `DefunctionalizedCodeOutput` type and the corresponding apply
-- function.
--
----------------------------------------------------------------

