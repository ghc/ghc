-----------------------------------------------------------------------------
--
-- Types for the linkers and the loader
--
-- (c) The University of Glasgow 2019
--
-----------------------------------------------------------------------------
module GHC.Linker.Types
   ( Loader (..)
   , LoaderState (..)
   , uninitializedLoader

   -- * Bytecode Loader State
   , BytecodeLoaderState(..)
   , BytecodeState(..)
   , emptyBytecodeLoaderState
   , emptyBytecodeState
   , modifyHomePackageBytecodeState
   , modifyExternalPackageBytecodeState
   , modifyBytecodeLoaderState
   , lookupNameBytecodeState
   , lookupBreakArrayBytecodeState
   , lookupInfoTableBytecodeState
   , lookupAddressBytecodeState
   , lookupCCSBytecodeState
   , BytecodeLoaderStateModifier
   , BytecodeLoaderStateTraverser
   , traverseHomePackageBytecodeState
   , traverseExternalPackageBytecodeState
   , modifyClosureEnv
   , LinkerEnv(..)
   , emptyLinkerEnv
   , ClosureEnv
   , emptyClosureEnv
   , extendClosureEnv
   , LinkedBreaks(..)
   , emptyLinkedBreaks
   , LinkableSet
   , mkLinkableSet
   , unionLinkableSet
   , ObjFile
   , SptEntry(..)
   , LibrarySpec(..)
   , LoadedPkgInfo(..)
   , PkgsLoaded

   -- * Linkable
   , Linkable
   , WholeCoreBindingsLinkable
   , LinkableWith(..)
   , mkModuleByteCodeLinkable
   , LinkablePart(..)
   , LinkableObjectSort (..)
   , linkableIsNativeCodeOnly
   , linkableObjs
   , linkableLibs
   , linkableFiles
   , linkableBCOs
   , linkablePartBCOs
   , linkableModuleByteCodes
   , linkableNativeParts
   , linkablePartitionParts
   , linkablePartPath
   , isNativeCode
   , isNativeLib
   , linkableFilterByteCode
   , linkableFilterNative
   , partitionLinkables

   , ModuleByteCode(..)
   )
where

import GHC.Prelude
import GHC.Unit                ( UnitId, Module )
import GHC.ByteCode.Types
import GHCi.BreakArray
import GHCi.RemoteTypes
import GHCi.Message            ( LoadedDLL )

import GHC.Stack.CCS
import GHC.Types.Name.Env      ( NameEnv, emptyNameEnv, extendNameEnvList, lookupNameEnv )
import GHC.Types.Name          ( Name )
import GHC.Types.SptEntry

import GHC.Utils.Outputable

import Control.Concurrent.MVar
import Data.Array
import Data.Time               ( UTCTime )
import GHC.Unit.Module.Env
import GHC.Types.Unique.DSet
import GHC.Types.Unique.DFM
import GHC.Unit.Module.WholeCoreBindings
import Data.Maybe (mapMaybe)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import qualified Data.List.NonEmpty as NE
import Control.Applicative ((<|>))
import Data.Functor.Identity


{- **********************************************************************

                        The Loader's state

  ********************************************************************* -}

{-
The loader state *must* match the actual state of the C dynamic linker at all
times.

The MVar used to hold the LoaderState contains a Maybe LoaderState. The MVar
serves to ensure mutual exclusion between multiple loaded copies of the GHC
library. The Maybe may be Nothing to indicate that the linker has not yet been
initialised.

The LinkerEnv maps Names to actual closures (for interpreted code only), for
use during linking.

Note [Looking up symbols in the relevant objects]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In #23415, we determined that a lot of time (>10s, or even up to >35s!) was
being spent on dynamically loading symbols before actually interpreting code
when `:main` was run in GHCi. The root cause was that for each symbol we wanted
to lookup, we would traverse the list of loaded objects and try find the symbol
in each of them with dlsym (i.e. looking up a symbol was, worst case, linear in
the amount of loaded objects).

To drastically improve load time (from +-38 seconds down to +-2s), we now:

1. For every of the native objects loaded for a given unit, store the handles returned by `dlopen`.
  - In `pkgs_loaded` of the `LoaderState`, which maps `UnitId`s to
    `LoadedPkgInfo`s, where the handles live in its field `loaded_pkg_hs_dlls`.

2. When looking up a Name (e.g. `lookupHsSymbol`), find that name's `UnitId` in
    the `pkgs_loaded` mapping,

3. And only look for the symbol (with `dlsym`) on the /handles relevant to that
    unit/, rather than in every loaded object.

Note [Symbols may not be found in pkgs_loaded]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Currently the `pkgs_loaded` mapping only contains the dynamic objects
associated with loaded units. Symbols defined in a static object (e.g. from a
statically-linked Haskell library) are found via the generic `lookupSymbol`
function call by `lookupHsSymbol` when the symbol is not found in any of the
dynamic objects of `pkgs_loaded`.

The rationale here is two-fold:

 * we have only observed major link-time issues in dynamic linking; lookups in
 the RTS linker's static symbol table seem to be fast enough

 * allowing symbol lookups restricted to a single ObjectCode would require the
 maintenance of a symbol table per `ObjectCode`, which would introduce time and
 space overhead

This fallback is further needed because we don't look in the haskell objects
loaded for the home units (see the call to `loadModuleLinkables` in
`loadDependencies`, as opposed to the call to `loadPackages'` in the same
function which updates `pkgs_loaded`). We should ultimately keep track of the
objects loaded (probably in `objs_loaded`, for which `LinkableSet` is a bit
unsatisfactory, see a suggestion in 51c5c4eb1f2a33e4dc88e6a37b7b7c135234ce9b)
and be able to lookup symbols specifically in them too (similarly to
`lookupSymbolInDLL`).
-}

newtype Loader = Loader { loader_state :: MVar (Maybe LoaderState) }

data LoaderState = LoaderState
    { bco_loader_state :: !BytecodeLoaderState
        -- ^ Information about bytecode objects we have loaded into the
        -- interpreter.

    , bcos_loaded :: !LinkableSet
        -- ^ The currently loaded interpreted modules (home package)

    , objs_loaded :: !LinkableSet
        -- ^ And the currently-loaded compiled modules (home package)

    , pkgs_loaded :: !PkgsLoaded
        -- ^ The currently-loaded packages;
        -- haskell libraries, system libraries, transitive dependencies

    , temp_sos :: ![(FilePath, String)]
        -- ^ We need to remember the name of previous temporary DLL/.so
        -- libraries so we can link them (see #10322)
    }

data BytecodeState = BytecodeState
        { bco_linker_env :: !LinkerEnv
        -- ^ Current global mapping from Names to their true values
        , bco_linked_breaks :: !LinkedBreaks
        -- ^ Mapping from loaded modules to their breakpoint arrays
        }

-- | The 'BytecodeLoaderState' captures all the information about bytecode loaded
-- into the interpreter.
-- It is separated into two parts. One for bytecode objects loaded by the home package and
-- one for bytecode objects loaded from bytecode libraries for external packages.
-- Much like the HPT/EPS split, the home package state can be unloaded by calling 'unload'.
data BytecodeLoaderState = BytecodeLoaderState
       { homePackage_loaded :: BytecodeState
       -- ^ Information about bytecode objects from the home package we have loaded into the interpreter.
       , externalPackage_loaded :: BytecodeState
       -- ^ Information about bytecode objects from external packages we have loaded into the interpreter.
       }


-- | Find a name loaded from bytecode
lookupNameBytecodeState :: BytecodeLoaderState -> Name -> Maybe (Name, ForeignHValue)
lookupNameBytecodeState (BytecodeLoaderState home_package external_package) name = do
      lookupNameEnv (closure_env (bco_linker_env home_package)) name
  <|> lookupNameEnv (closure_env (bco_linker_env external_package)) name

-- | Look up a break array in the bytecode loader state.
lookupBreakArrayBytecodeState :: BytecodeLoaderState -> Module -> Maybe (ForeignRef BreakArray)
lookupBreakArrayBytecodeState (BytecodeLoaderState home_package external_package) break_mod = do
  lookupModuleEnv (breakarray_env (bco_linked_breaks home_package)) break_mod
  <|> lookupModuleEnv (breakarray_env (bco_linked_breaks external_package)) break_mod

-- | Look up an info table in the bytecode loader state.
lookupInfoTableBytecodeState :: BytecodeLoaderState -> Name -> Maybe (Name, ItblPtr)
lookupInfoTableBytecodeState (BytecodeLoaderState home_package external_package) info_mod = do
  lookupNameEnv (itbl_env (bco_linker_env home_package)) info_mod
  <|> lookupNameEnv (itbl_env (bco_linker_env external_package)) info_mod

-- | Look up an address in the bytecode loader state.
lookupAddressBytecodeState :: BytecodeLoaderState -> Name -> Maybe (Name, AddrPtr)
lookupAddressBytecodeState (BytecodeLoaderState home_package external_package) addr_mod = do
  lookupNameEnv (addr_env (bco_linker_env home_package)) addr_mod
  <|> lookupNameEnv (addr_env (bco_linker_env external_package)) addr_mod

-- | Look up a cost centre stack in the bytecode loader state.
lookupCCSBytecodeState :: BytecodeLoaderState -> Module -> Maybe (Array BreakTickIndex (RemotePtr CostCentre))
lookupCCSBytecodeState (BytecodeLoaderState home_package external_package) ccs_mod = do
  lookupModuleEnv (ccs_env (bco_linked_breaks home_package)) ccs_mod
  <|> lookupModuleEnv (ccs_env (bco_linked_breaks external_package)) ccs_mod

emptyBytecodeLoaderState :: BytecodeLoaderState
emptyBytecodeLoaderState = BytecodeLoaderState
    { homePackage_loaded = emptyBytecodeState
    , externalPackage_loaded = emptyBytecodeState
    }

emptyBytecodeState :: BytecodeState
emptyBytecodeState = BytecodeState
    { bco_linker_env = emptyLinkerEnv
    , bco_linked_breaks = emptyLinkedBreaks
    }


-- Some parts of the compiler can be used to load bytecode into either the home package or
-- external package state. They are parameterised by a 'BytecodeLoaderStateModifier' or
-- 'BytecodeLoaderStateTraverser' so they know which part of the state to update.

type BytecodeLoaderStateModifier = BytecodeLoaderState -> (BytecodeState -> BytecodeState) -> BytecodeLoaderState
type BytecodeLoaderStateTraverser m = BytecodeLoaderState -> (BytecodeState -> m BytecodeState) -> m BytecodeLoaderState

-- | Only update the home package bytecode state.
modifyHomePackageBytecodeState :: BytecodeLoaderState -> (BytecodeState -> BytecodeState) -> BytecodeLoaderState
modifyHomePackageBytecodeState bls f = runIdentity $ traverseHomePackageBytecodeState bls (return . f)

-- | Only update the external package bytecode state.
modifyExternalPackageBytecodeState :: BytecodeLoaderState -> (BytecodeState -> BytecodeState) -> BytecodeLoaderState
modifyExternalPackageBytecodeState bls f = runIdentity $ traverseExternalPackageBytecodeState bls (return . f)

-- | Effectfully update the home package bytecode state.
traverseHomePackageBytecodeState :: Monad m => BytecodeLoaderState -> (BytecodeState -> m BytecodeState) -> m BytecodeLoaderState
traverseHomePackageBytecodeState bls f = do
  home_package <- f (homePackage_loaded bls)
  return bls { homePackage_loaded = home_package }

-- | Effectfully update the external package bytecode state.
traverseExternalPackageBytecodeState :: Monad m => BytecodeLoaderState -> (BytecodeState -> m BytecodeState) -> m BytecodeLoaderState
traverseExternalPackageBytecodeState bls f = do
  external_package <- f (externalPackage_loaded bls)
  return bls { externalPackage_loaded = external_package }


modifyBytecodeLoaderState :: BytecodeLoaderStateModifier -> LoaderState -> (BytecodeState -> BytecodeState) -> LoaderState
modifyBytecodeLoaderState modify_bytecode_loader_state pls f = pls { bco_loader_state = modify_bytecode_loader_state (bco_loader_state pls) f }

uninitializedLoader :: IO Loader
uninitializedLoader = Loader <$> newMVar Nothing

modifyClosureEnv :: BytecodeState -> (ClosureEnv -> ClosureEnv) -> BytecodeState
modifyClosureEnv pls f =
    let le = bco_linker_env pls
        ce = closure_env le
    in pls { bco_linker_env = le { closure_env = f ce } }

-- | Information about loaded bytecode
data LinkerEnv = LinkerEnv
  { closure_env :: !ClosureEnv
      -- ^ Current global mapping from closure Names to their true values

  , itbl_env    :: !ItblEnv
      -- ^ The current global mapping from RdrNames of DataCons to
      -- info table addresses.
      -- When a new LinkablePart is linked into the running image, or an existing
      -- module in the image is replaced, the itbl_env must be updated
      -- appropriately.

  , addr_env    :: !AddrEnv
      -- ^ Like 'closure_env' and 'itbl_env', but for top-level 'Addr#' literals,
      -- see Note [Generating code for top-level string literal bindings] in GHC.StgToByteCode.
  }

emptyLinkerEnv :: LinkerEnv
emptyLinkerEnv = LinkerEnv
  { closure_env = emptyNameEnv
  , itbl_env    = emptyNameEnv
  , addr_env    = emptyNameEnv
  }

type ClosureEnv = NameEnv (Name, ForeignHValue)

emptyClosureEnv :: ClosureEnv
emptyClosureEnv = emptyNameEnv

extendClosureEnv :: ClosureEnv -> [(Name,ForeignHValue)] -> ClosureEnv
extendClosureEnv cl_env pairs
  = extendNameEnvList cl_env [ (n, (n,v)) | (n,v) <- pairs]

-- | 'BreakArray's and CCSs are allocated per-module at link-time.
--
-- Specifically, a module's 'BreakArray' is allocated either:
--  - When a BCO for that module is linked
--  - When :break is used on a given module *before* the BCO has been linked.
--
-- We keep this structure in the 'LoaderState'
data LinkedBreaks
  = LinkedBreaks
  { breakarray_env :: !(ModuleEnv (ForeignRef BreakArray))
      -- ^ Each 'Module's remote pointer of 'BreakArray'.

  , ccs_env :: !(ModuleEnv (Array BreakTickIndex (RemotePtr CostCentre)))
      -- ^ Each 'Module's array of remote pointers of 'CostCentre'.
      -- Untouched when not profiling.
  }

emptyLinkedBreaks :: LinkedBreaks
emptyLinkedBreaks = LinkedBreaks
  { breakarray_env = emptyModuleEnv
  , ccs_env        = emptyModuleEnv
  }

type PkgsLoaded = UniqDFM UnitId LoadedPkgInfo

data LoadedPkgInfo
  = LoadedPkgInfo
  { loaded_pkg_uid         :: !UnitId
  , loaded_pkg_hs_objs     :: ![LibrarySpec]
  , loaded_pkg_non_hs_objs :: ![LibrarySpec]
  , loaded_pkg_hs_dlls     :: ![RemotePtr LoadedDLL]
    -- ^ See Note [Looking up symbols in the relevant objects]
  , loaded_pkg_trans_deps  :: UniqDSet UnitId
  }

instance Outputable LoadedPkgInfo where
  ppr (LoadedPkgInfo uid hs_objs non_hs_objs _ trans_deps) =
    vcat [ppr uid
         , ppr hs_objs
         , ppr non_hs_objs
         , ppr trans_deps ]


-- | Information we can use to dynamically link modules into the compiler
data LinkableWith parts = Linkable
  { linkableTime     :: !UTCTime
      -- ^ Time at which this linkable was built
      -- (i.e. when the bytecodes were produced,
      --       or the mod date on the files)

  , linkableModule   :: !Module
      -- ^ The linkable module itself

  , linkableParts :: parts
    -- ^ Files and chunks of code to link.
 } deriving (Functor, Traversable, Foldable)

type Linkable = LinkableWith (NonEmpty LinkablePart)

type WholeCoreBindingsLinkable = LinkableWith WholeCoreBindings

type LinkableSet = ModuleEnv Linkable

mkLinkableSet :: [Linkable] -> LinkableSet
mkLinkableSet ls = mkModuleEnv [(linkableModule l, l) | l <- ls]

-- | Union of LinkableSets.
--
-- In case of conflict, keep the most recent Linkable (as per linkableTime)
unionLinkableSet :: LinkableSet -> LinkableSet -> LinkableSet
unionLinkableSet = plusModuleEnv_C go
  where
    go l1 l2
      | linkableTime l1 > linkableTime l2 = l1
      | otherwise = l2

instance Outputable a => Outputable (LinkableWith a) where
  ppr (Linkable when_made mod parts)
     = (text "Linkable" <+> parens (text (show when_made)) <+> ppr mod)
       $$ nest 3 (ppr parts)

type ObjFile = FilePath

-- | Classify the provenance of @.o@ products.
data LinkableObjectSort =
  -- | The object is the final product for a module.
  -- When linking splices, its file extension will be adapted to the
  -- interpreter's way if needed.
  ModuleObject
  |
  -- | The object was created from generated code for foreign stubs or foreign
  -- sources added by the user.
  -- Its file extension must be preserved, since there are no objects for
  -- alternative ways available.
  ForeignObject

-- | Objects which have yet to be linked by the compiler
data LinkablePart
  = DotO
      ObjFile
      -- ^ An object file (.o)
      LinkableObjectSort
      -- ^ Whether the object is an internal, intermediate build product that
      -- should not be adapted to the interpreter's way. Used for foreign stubs
      -- loaded from interfaces.

  | DotA FilePath
      -- ^ Static archive file (.a)

  | DotDLL FilePath
      -- ^ Dynamically linked library file (.so, .dll, .dylib)

  | DotGBC ModuleByteCode
    -- ^ A byte-code object, lives only in memory.


-- | The in-memory representation of a bytecode object
-- These are stored on-disk as .gbc files.
data ModuleByteCode = ModuleByteCode { gbc_module :: Module
                                      , gbc_compiled_byte_code :: CompiledByteCode
                                      , gbc_foreign_files :: [FilePath]  -- ^ Path to object files
                                      }

mkModuleByteCodeLinkable :: UTCTime -> ModuleByteCode -> Linkable
mkModuleByteCodeLinkable linkable_time bco =
  Linkable linkable_time (gbc_module bco) (pure (DotGBC bco))

instance Outputable ModuleByteCode where
  ppr (ModuleByteCode mod _cbc _fos) = text "ModuleByteCode" <+> ppr mod

instance Outputable LinkablePart where
  ppr (DotO path sort)   = text "DotO" <+> text path <+> pprSort sort
    where
      pprSort = \case
        ModuleObject -> empty
        ForeignObject -> brackets (text "foreign")
  ppr (DotA path)       = text "DotA" <+> text path
  ppr (DotDLL path)     = text "DotDLL" <+> text path
  ppr (DotGBC bco)      = text "DotGBC" <+> ppr bco

-- | Return true if the linkable only consists of native code (no BCO)
linkableIsNativeCodeOnly :: Linkable -> Bool
linkableIsNativeCodeOnly l = all isNativeCode (NE.toList (linkableParts l))

-- | List the BCOs parts of a linkable.
--
-- This excludes the CoreBindings parts
linkableBCOs :: Linkable -> [CompiledByteCode]
linkableBCOs l = [ gbc_compiled_byte_code gbc | DotGBC gbc <- NE.toList (linkableParts l) ]

linkableModuleByteCodes :: Linkable -> [ModuleByteCode]
linkableModuleByteCodes l = [ mbc | DotGBC mbc <- NE.toList (linkableParts l) ]

-- | List the native linkable parts (.o/.so/.dll) of a linkable
linkableNativeParts :: Linkable -> [LinkablePart]
linkableNativeParts l = NE.filter isNativeCode (linkableParts l)

-- | Split linkable parts into (native code parts, BCOs parts)
linkablePartitionParts :: Linkable -> ([LinkablePart],[LinkablePart])
linkablePartitionParts l = NE.partition isNativeCode (linkableParts l)

-- | List the native objects (.o) of a linkable
linkableObjs :: Linkable -> [FilePath]
linkableObjs l = concatMap linkablePartObjectPaths (linkableParts l)

-- | List the native libraries (.so/.dll) of a linkable
linkableLibs :: Linkable -> [LinkablePart]
linkableLibs l = NE.filter isNativeLib (linkableParts l)

-- | List the paths of the native objects and libraries (.o/.so/.dll)
linkableFiles :: Linkable -> [FilePath]
linkableFiles l = concatMap linkablePartNativePaths (NE.toList (linkableParts l))

-------------------------------------------

-- | Is the part a native object or library? (.o/.so/.dll)
isNativeCode :: LinkablePart -> Bool
isNativeCode = \case
  DotO {}         -> True
  DotA {}         -> True
  DotDLL {}       -> True
  DotGBC {}       -> False

-- | Is the part a native library? (.so/.dll)
isNativeLib :: LinkablePart -> Bool
isNativeLib = \case
  DotO {}         -> False
  DotA {}         -> True
  DotDLL {}       -> True
  DotGBC {}       -> False

-- | Get the FilePath of linkable part (if applicable)
linkablePartPath :: LinkablePart -> Maybe FilePath
linkablePartPath = \case
  DotO fn _       -> Just fn
  DotA fn         -> Just fn
  DotDLL fn       -> Just fn
  DotGBC {}       -> Nothing

-- | Return the paths of all object code files (.o, .a, .so) contained in this
-- 'LinkablePart'.
linkablePartNativePaths :: LinkablePart -> [FilePath]
linkablePartNativePaths = \case
  DotO fn _       -> [fn]
  DotA fn         -> [fn]
  DotDLL fn       -> [fn]
  DotGBC {}       -> []

-- | Return the paths of all object files (.o) contained in this 'LinkablePart'.
linkablePartObjectPaths :: LinkablePart -> [FilePath]
linkablePartObjectPaths = \case
  DotO fn _ -> [fn]
  DotA _ -> []
  DotDLL _ -> []
  DotGBC bco -> gbc_foreign_files bco

-- | Retrieve the compiled byte-code from the linkable part.
--
-- Contrary to linkableBCOs, this includes byte-code from LazyBCOs.
linkablePartBCOs :: LinkablePart -> [CompiledByteCode]
linkablePartBCOs = \case
  DotGBC bco    -> [gbc_compiled_byte_code bco]
  _           -> []

linkableFilter :: (LinkablePart -> [LinkablePart]) -> Linkable -> Maybe Linkable
linkableFilter f linkable = do
  new <- nonEmpty (concatMap f (linkableParts linkable))
  Just linkable {linkableParts = new}

linkablePartNative :: LinkablePart -> [LinkablePart]
linkablePartNative u = case u of
  DotO {}  -> [u]
  DotA {} -> [u]
  DotDLL {} -> [u]
  DotGBC bco -> [DotO f ForeignObject | f <- gbc_foreign_files bco]

linkablePartByteCode :: LinkablePart -> [LinkablePart]
linkablePartByteCode = \case
  u@DotGBC {}  -> [u]
  _ -> []

-- | Transform the 'LinkablePart' list in this 'Linkable' to contain only
-- object code files (.o, .a, .so) without 'BCOs'.
-- If no 'LinkablePart' remains, return 'Nothing'.
linkableFilterNative :: Linkable -> Maybe Linkable
linkableFilterNative = linkableFilter linkablePartNative

-- | Transform the 'LinkablePart' list in this 'Linkable' to contain only byte
-- code
-- If no 'LinkablePart' remains, return 'Nothing'.
linkableFilterByteCode :: Linkable -> Maybe Linkable
linkableFilterByteCode = linkableFilter linkablePartByteCode

-- | Split the 'LinkablePart' lists in each 'Linkable' into only object code
-- files (.o, .a, .so) and only byte code, and return two
-- lists containing the nonempty 'Linkable's for each.
partitionLinkables :: [Linkable] -> ([Linkable], [Linkable])
partitionLinkables linkables =
  (
    mapMaybe linkableFilterNative linkables,
    mapMaybe linkableFilterByteCode linkables
  )

{- **********************************************************************

                Loading packages

  ********************************************************************* -}

data LibrarySpec
   = Objects [FilePath] -- Full path names of set of .o files, including trailing .o
                        -- We allow batched loading to ensure that cyclic symbol
                        -- references can be resolved (see #13786).
                        -- For dynamic objects only, try to find the object
                        -- file in all the directories specified in
                        -- v_Library_paths before giving up.

   | Archive FilePath   -- Full path name of a .a file, including trailing .a

   | DLL String         -- "Unadorned" name of a .DLL/.so
                        --  e.g.    On unix     "qt"  denotes "libqt.so"
                        --          On Windows  "burble"  denotes "burble.DLL" or "libburble.dll"
                        --  loadDLLs is platform-specific and adds the lib/.so/.DLL
                        --  suffixes platform-dependently

   | DLLPath FilePath   -- Absolute or relative pathname to a dynamic library
                        -- (ends with .dll or .so).

   | Framework String   -- Only used for darwin, but does no harm

   | BytecodeLibrary FilePath
      -- ^ A bytecode library file (.bytecodelib)

instance Outputable LibrarySpec where
  ppr (Objects objs) = text "Objects" <+> ppr (map (text @SDoc) objs)
  ppr (Archive a) = text "Archive" <+> text a
  ppr (DLL s) = text "DLL" <+> text s
  ppr (DLLPath f) = text "DLLPath" <+> text f
  ppr (Framework s) = text "Framework" <+> text s
  ppr (BytecodeLibrary f) = text "BytecodeLibrary" <+> text f
