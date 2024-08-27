{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}

module GHC.Unit.Module.WholeCoreBindings where

import GHC.Cmm.CLabel
import GHC.Driver.DynFlags (DynFlags (targetPlatform), initSDocContext)
import GHC.ForeignSrcLang (ForeignSrcLang (..))
import GHC.Iface.Syntax
import GHC.Prelude
import GHC.Types.ForeignStubs
import GHC.Unit.Module.Location
import GHC.Unit.Types (Module)
import GHC.Utils.Binary
import GHC.Utils.Error (debugTraceMsg)
import GHC.Utils.Logger (Logger)
import GHC.Utils.Outputable
import GHC.Utils.Panic (panic, pprPanic)
import GHC.Utils.TmpFs

import Control.DeepSeq (NFData (..))
import Data.Traversable (for)
import Data.Word (Word8)
import Data.Maybe (fromMaybe)
import System.FilePath (takeExtension)

{-
Note [Interface Files with Core Definitions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A interface file can optionally contain the definitions of all core bindings, this
is enabled by the flag `-fwrite-if-simplified-core`.
This provides everything needed in addition to the normal ModIface and ModDetails
to restart compilation after typechecking to generate bytecode. The `fi_bindings` field
is stored in the normal interface file and the other fields populated whilst loading
the interface file.

The lifecycle of a WholeCoreBindings typically proceeds as follows:

1. The ModIface which contains mi_extra_decls is loaded from disk. A linkable is
   created (which is headed by the `CoreBindings` constructor). This is an unhydrated set of bindings which
   is currently unsuitable for linking, but at the point it is loaded, the ModIface
   hasn't been hydrated yet (See Note [Hydrating Modules]) either so the CoreBindings constructor allows the delaying of converting
   the WholeCoreBindings into a proper Linkable (if we ever do that). The CoreBindings constructor also
   allows us to convert the WholeCoreBindings into multiple different linkables if we so desired.

2. `initWholeCoreBindings` turns a WholeCoreBindings into a proper BCOs linkable. This step combines together
   all the necessary information from a ModIface, ModDetails and WholeCoreBindings in order to
   create the linkable. The linkable created is a "LazyBCOs" linkable, which
   was introduced just for initWholeCoreBindings, so that the bytecode can be generated lazily.
   Using the `BCOs` constructor directly here leads to the bytecode being forced
   too eagerly.

3. Then when bytecode is needed, the LazyBCOs value is inspected and unpacked and
   the linkable is used as before.

The flag `-fwrite-if-simplified-core` determines whether the extra information is written
to an interface file. The program which is written is the core bindings of the module
after whatever simplification the user requested has been performed. So the simplified core bindings
of the interface file agree with the optimisation level as reported by the interface
file.

Note [Size of Interface Files with Core Definitions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

How much overhead does `-fwrite-if-simplified-core` add to a typical interface file?
As an experiment I compiled the `Cabal` library and `ghc` library (Aug 22) with

| Project | .hi  | .hi (fat) | .o   |
| --------| ---- | --------- | --   |
| ghc     | 32M  | 68M       | 127M |
| Cabal   | 3.2M | 9.8M      | 14M  |

So the interface files gained in size but the end result was still smaller than
the object files.

-}

data WholeCoreBindings = WholeCoreBindings
            { wcb_bindings :: [IfaceBindingX IfaceMaybeRhs IfaceTopBndrInfo] -- ^ serialised tidied core bindings.
            , wcb_module   :: Module  -- ^ The module which the bindings are for
            , wcb_mod_location :: ModLocation -- ^ The location where the sources reside.
              -- | Stubs for foreign declarations and files added via
              -- 'GHC.Internal.TH.Syntax.addForeignFilePath'.
            , wcb_foreign :: IfaceForeign
            }

{-
Note [Foreign stubs and TH bytecode linking]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Foreign declarations may introduce additional build products called "stubs" that
contain wrappers for the exposed functions.
For example, consider a foreign import of a C function named @main_loop@ from
the file @bindings.h@ in the module @CLibrary@:

@
foreign import capi "bindings.h main_loop" mainLoop :: IO Int
@

GHC will generate a snippet of C code containing a wrapper:

@
#include "bindings.h"
HsInt ghczuwrapperZC0ZCmainZCCLibraryZCmainzuloop(void) {return main_loop();}
@

Wrappers like these are generated as 'ForeignStubs' by the desugarer in
'dsForeign' and stored in the various @*Guts@ types; until they are compiled to
temporary object files in 'runHscBackendPhase' during code generation and
ultimately merged into the final object file for the module, @CLibrary.o@.

This creates some problems with @-fprefer-byte-code@, which allows splices to
execute bytecode instead of native code for dependencies that provide it.
Usually, when some TH code depends on @CLibrary@, the linker would look for
@CLibrary.o@ and load that before executing the splice, but with this flag, it
will first attempt to load bytecode from @CLibrary.hi@ and compile it in-memory.

Problem 1:

Code for splices is loaded from interfaces in the shape of Core bindings
(see 'WholeCoreBindings'), rather than from object files.
Those Core bindings are intermediate build products that do not contain the
module's stubs, since those are separated from the Haskell code before Core is
generated and only compiled and linked into the final object when native code is
generated.

Therefore, stubs have to be stored separately in interface files.
Unfortunately, the type 'ForeignStubs' contains 'CLabel', which is a huge type
with several 'Unique's used mainly by C--.
Luckily, the only constructor used for foreign stubs is 'ModuleLabel', which
contains the name of a foreign declaration's initializer, if it has one.
So we convert a 'CLabel' to 'CStubLabel' in 'encodeIfaceForeign' and store only
the simplified data.

Problem 2:

Given module B, which contains a splice that executes code from module A, both
in the home package, consider these different circumstances:

1. In make mode, both modules are recompiled
2. In make mode, only B is recompiled
3. In oneshot mode, B is compiled

In case 1, 'runHscBackendPhase' directly generates bytecode from the 'CgGuts'
that the main pipeline produced and stores it in the 'HomeModLinkable' that is
one of its build products.
The stubs are merged into a single object and added to the 'HomeModLinkable' in
'hscGenBackendPipeline'.

In case 2, 'hscRecompStatus' short-circuits the pipeline while checking A, since
the module is up to date.
Nevertheless, it calls 'checkByteCode', which extracts Core bindings from A's
interface and adds them to the 'HomeModLinkable'.
No stubs are generated in this case, since the desugarer wasn't run!

In both of these cases, 'compileOne'' proceeds to call 'initWholeCoreBindings',
applied to the 'HomeModLinkable', to compile Core bindings (lazily) to bytecode,
which is then written back to the 'HomeModLinkable'.
If the 'HomeModLinkable' already contains bytecode (case 1), this is a no-op.
Otherwise, the stub objects from the interface are compiled to objects in
'generateByteCode' and added to the 'HomeModLinkable' as well.

Case 3 is not implemented yet (!13042).

Problem 3:

In all three cases, the final step before splice execution is linking.

The function 'getLinkDeps' is responsible for assembling all of a splice's
dependencies, looking up imported modules in the HPT and EPS, collecting all
'HomeModLinkable's and object files that it can find.

However, since splices are executed in the interpreter, the 'Way' of the current
build may differ from the interpreter's.
For example, the current GHC invocation might be building a static binary, but
the internal interpreter requires dynamic linking; or profiling might be
enabled.
To adapt to the interpreter's 'Way', 'getLinkDeps' substitutes all object files'
extensions with that corresponding to that 'Way' – e.g. changing @.o@ to
@.dyn_o@, which requires dependencies to be built with @-dynamic[-too]@, which
in turn is enforced after downsweep in 'GHC.Driver.Make.enableCodeGenWhen'.

This doesn't work for stub objects, though – they are compiled to temporary
files with mismatching names, so simply switching out the suffix would refer to
a nonexisting file.
Even if that wasn't an issue, they are compiled for the session's 'Way', not its
associated module's, so the dynamic variant wouldn't be available when building
only static outputs.

To mitigate this, we instead build foreign objects specially for the
interpreter, updating the build flags in 'compile_for_interpreter' to use the
interpreter's way.

Problem 4:

Foreign code may have dependencies on Haskell code.

Both foreign exports and @StaticPointers@ produce stubs that contain @extern@
declarations of values referring to STG closures.
When those stub objects are loaded, the undefined symbols need to be provided to
the linker.

I have no insight into how this works, and whether we could provide the memory
address of a BCO as a ccall symbol while linking, so it's unclear at the moment
what to do about this.

In addition to that, those objects would also have to be loaded _after_
bytecode, and therefore 'DotO' would have to be marked additionally to separate
them from those that are loaded before.
If mutual dependencies between BCOs and foreign code are possible, this will be
much more diffcult though.

Problem 5:

TH allows splices to add arbitrary files as additional linker inputs.

Using the method `qAddForeignFilePath`, a foreign source file or a precompiled
object file can be added to the current modules dependencies.
These files will be processed by the pipeline and linked into the final object.

Since the files may be temporarily created from a string, we have to read their
contents in 'encodeIfaceForeign' and store them in the interface as well, and
write them to temporary files when loading bytecode in 'decodeIfaceForeign'.
-}

-- | Wrapper for avoiding a dependency on 'Binary' and 'NFData' in 'CLabel'.
newtype IfaceCLabel = IfaceCLabel CStubLabel

instance Binary IfaceCLabel where
  get bh = do
    csl_is_initializer <- get bh
    csl_module <- get bh
    csl_name <- get bh
    pure (IfaceCLabel CStubLabel {csl_is_initializer, csl_module, csl_name})

  put_ bh (IfaceCLabel CStubLabel {csl_is_initializer, csl_module, csl_name}) = do
    put_ bh csl_is_initializer
    put_ bh csl_module
    put_ bh csl_name

instance NFData IfaceCLabel where
  rnf (IfaceCLabel CStubLabel {csl_is_initializer, csl_module, csl_name}) =
    rnf csl_is_initializer `seq` rnf csl_module `seq` rnf csl_name

instance Outputable IfaceCLabel where
  ppr (IfaceCLabel l) = ppr l

-- | Simplified encoding of 'GHC.Types.ForeignStubs.ForeignStubs' for interface
-- serialization.
--
-- See Note [Foreign stubs and TH bytecode linking]
data IfaceCStubs =
  IfaceCStubs {
    header :: String,
    source :: String,
    initializers :: [IfaceCLabel],
    finalizers :: [IfaceCLabel]
  }

instance Outputable IfaceCStubs where
  ppr IfaceCStubs {header, source, initializers, finalizers} =
    vcat [
      hang (text "header:") 2 (vcat (text <$> lines header)),
      hang (text "source:") 2 (vcat (text <$> lines source)),
      hang (text "initializers:") 2 (ppr initializers),
      hang (text "finalizers:") 2 (ppr finalizers)
    ]

-- | 'Binary' 'put_' for 'ForeignSrcLang'.
binary_put_ForeignSrcLang :: BinHandle -> ForeignSrcLang -> IO ()
binary_put_ForeignSrcLang bh lang =
  put_ @Word8 bh $ case lang of
    LangC -> 0
    LangCxx -> 1
    LangObjc -> 2
    LangObjcxx -> 3
    LangAsm -> 4
    LangJs -> 5
    RawObject -> 6

-- | 'Binary' 'get' for 'ForeignSrcLang'.
binary_get_ForeignSrcLang :: BinHandle -> IO ForeignSrcLang
binary_get_ForeignSrcLang bh = do
  b <- getByte bh
  pure $ case b of
    0 -> LangC
    1 -> LangCxx
    2 -> LangObjc
    3 -> LangObjcxx
    4 -> LangAsm
    5 -> LangJs
    6 -> RawObject
    _ -> panic "invalid Binary value for ForeignSrcLang"

instance Binary IfaceCStubs where
  get bh = do
    header <- get bh
    source <- get bh
    initializers <- get bh
    finalizers <- get bh
    pure IfaceCStubs {..}

  put_ bh IfaceCStubs {..} = do
    put_ bh header
    put_ bh source
    put_ bh initializers
    put_ bh finalizers

instance NFData IfaceCStubs where
  rnf IfaceCStubs {..} =
    rnf header
    `seq`
    rnf source
    `seq`
    rnf initializers
    `seq`
    rnf finalizers

-- | A source file added from Template Haskell using 'qAddForeignFilePath', for
-- storage in interfaces.
--
-- See Note [Foreign stubs and TH bytecode linking]
data IfaceForeignFile =
  IfaceForeignFile {
    -- | The language is specified by the user.
    lang :: ForeignSrcLang,

    -- | The contents of the file, which will be written to a temporary file
    -- when loaded from an interface.
    source :: String,

    -- | The extension used by the user is preserved, to avoid confusing
    -- external tools with an unexpected @.c@ file or similar.
    extension :: FilePath
  }

instance Outputable IfaceForeignFile where
  ppr IfaceForeignFile {lang, source} =
    hang (text (show lang) <> colon) 2 (vcat (text <$> lines source))

instance Binary IfaceForeignFile where
  get bh = do
    lang <- binary_get_ForeignSrcLang bh
    source <- get bh
    extension <- get bh
    pure IfaceForeignFile {lang, source, extension}

  put_ bh IfaceForeignFile {lang, source, extension} = do
    binary_put_ForeignSrcLang bh lang
    put_ bh source
    put_ bh extension

instance NFData IfaceForeignFile where
  rnf IfaceForeignFile {lang, source, extension} =
    lang `seq` rnf source `seq` rnf extension

data IfaceForeign =
  IfaceForeign {
    stubs :: Maybe IfaceCStubs,
    files :: [IfaceForeignFile]
  }

instance Outputable IfaceForeign where
  ppr IfaceForeign {stubs, files} =
    hang (text "stubs:") 2 (maybe (text "empty") ppr stubs) $$
    vcat (ppr <$> files)

emptyIfaceForeign :: IfaceForeign
emptyIfaceForeign = IfaceForeign {stubs = Nothing, files = []}

-- | Convert foreign stubs and foreign files to a format suitable for writing to
-- interfaces.
--
-- See Note [Foreign stubs and TH bytecode linking]
encodeIfaceForeign ::
  Logger ->
  DynFlags ->
  ForeignStubs ->
  [(ForeignSrcLang, FilePath)] ->
  IO IfaceForeign
encodeIfaceForeign logger dflags foreign_stubs lang_paths = do
  files <- read_foreign_files
  stubs <- encode_stubs foreign_stubs
  let iff = IfaceForeign {stubs, files}
  debugTraceMsg logger 3 $
    hang (text "Encoding foreign data for iface:") 2 (ppr iff)
  pure iff
  where
    -- We can't just store the paths, since files may have been generated with
    -- GHC session lifetime in 'GHC.Internal.TH.Syntax.addForeignSource'.
    read_foreign_files =
      for lang_paths $ \ (lang, path) -> do
        source <- readFile path
        pure IfaceForeignFile {lang, source, extension = takeExtension path}

    encode_stubs = \case
      NoStubs ->
        pure Nothing
      ForeignStubs (CHeader header) (CStub source inits finals) ->
        pure $ Just IfaceCStubs {
          header = render header,
          source = render source,
          initializers = encode_label <$> inits,
          finalizers = encode_label <$> finals
        }

    encode_label clabel =
      fromMaybe (invalid_label clabel) (IfaceCLabel <$> cStubLabel clabel)

    invalid_label clabel =
      pprPanic
      "-fwrite-if-simplified-core is incompatible with this foreign stub:"
      (pprCLabel (targetPlatform dflags) clabel)

    render = renderWithContext (initSDocContext dflags PprCode)

-- | Decode serialized foreign stubs and foreign files.
--
-- See Note [Foreign stubs and TH bytecode linking]
decodeIfaceForeign ::
  Logger ->
  TmpFs ->
  TempDir ->
  IfaceForeign ->
  IO (ForeignStubs, [(ForeignSrcLang, FilePath)])
decodeIfaceForeign logger tmpfs tmp_dir iff@IfaceForeign {stubs, files} = do
  debugTraceMsg logger 3 $
    hang (text "Decoding foreign data from iface:") 2 (ppr iff)
  lang_paths <- for files $ \ IfaceForeignFile {lang, source, extension} -> do
    f <- newTempName logger tmpfs tmp_dir TFL_GhcSession extension
    writeFile f source
    pure (lang, f)
  pure (maybe NoStubs decode_stubs stubs, lang_paths)
  where
    decode_stubs IfaceCStubs {header, source, initializers, finalizers} =
      ForeignStubs
      (CHeader (text header))
      (CStub (text source) (labels initializers) (labels finalizers))

    labels ls = [fromCStubLabel l | IfaceCLabel l <- ls]

instance Binary IfaceForeign where
  get bh = do
    stubs <- get bh
    files <- get bh
    pure IfaceForeign {stubs, files}

  put_ bh IfaceForeign {stubs, files} = do
    put_ bh stubs
    put_ bh files

instance NFData IfaceForeign where
  rnf IfaceForeign {stubs, files} = rnf stubs `seq` rnf files
