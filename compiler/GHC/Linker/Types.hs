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
   , Linkable(..)
   , LinkableSet
   , mkLinkableSet
   , unionLinkableSet
   , ObjFile
   , Unlinked(..)
   , SptEntry(..)
   , isObjectLinkable
   , linkableObjs
   , isObject
   , nameOfObject
   , nameOfObject_maybe
   , isInterpretable
   , byteCodeOfObject
   , LibrarySpec(..)
   , LoadedPkgInfo(..)
   , PkgsLoaded
   )
where

import GHC.Prelude
import GHC.Unit                ( UnitId, Module )
import GHC.ByteCode.Types      ( ItblEnv, CompiledByteCode )
import GHC.Fingerprint.Type    ( Fingerprint )
import GHCi.RemoteTypes        ( ForeignHValue )

import GHC.Types.Var           ( Id )
import GHC.Types.Name.Env      ( NameEnv )
import GHC.Types.Name          ( Name )

import GHC.Utils.Outputable
import GHC.Utils.Panic

import Control.Concurrent.MVar
import Data.Time               ( UTCTime )
import Data.Maybe
import GHC.Unit.Module.Env
import GHC.Types.Unique.DSet
import GHC.Types.Unique.DFM


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

The LoaderState maps Names to actual closures (for interpreted code only), for
use during linking.
-}

newtype Loader = Loader { loader_state :: MVar (Maybe LoaderState) }

data LoaderState = LoaderState
    { closure_env :: ClosureEnv
        -- ^ Current global mapping from Names to their true values

    , itbl_env    :: !ItblEnv
        -- ^ The current global mapping from RdrNames of DataCons to
        -- info table addresses.
        -- When a new Unlinked is linked into the running image, or an existing
        -- module in the image is replaced, the itbl_env must be updated
        -- appropriately.

    , bcos_loaded :: !LinkableSet
        -- ^ The currently loaded interpreted modules (home package)

    , objs_loaded :: !LinkableSet
        -- ^ And the currently-loaded compiled modules (home package)

    , pkgs_loaded :: !PkgsLoaded
        -- ^ The currently-loaded packages; always object code
        -- haskell libraries, system libraries, transitive dependencies

    , temp_sos :: ![(FilePath, String)]
        -- ^ We need to remember the name of previous temporary DLL/.so
        -- libraries so we can link them (see #10322)
    }

uninitializedLoader :: IO Loader
uninitializedLoader = Loader <$> newMVar Nothing

type ClosureEnv = NameEnv (Name, ForeignHValue)
type PkgsLoaded = UniqDFM UnitId LoadedPkgInfo

data LoadedPkgInfo
  = LoadedPkgInfo
  { loaded_pkg_uid         :: !UnitId
  , loaded_pkg_hs_objs     :: ![LibrarySpec]
  , loaded_pkg_non_hs_objs :: ![LibrarySpec]
  , loaded_pkg_trans_deps  :: UniqDSet UnitId
  }

instance Outputable LoadedPkgInfo where
  ppr (LoadedPkgInfo uid hs_objs non_hs_objs trans_deps) =
    vcat [ppr uid
         , ppr hs_objs
         , ppr non_hs_objs
         , ppr trans_deps ]


-- | Information we can use to dynamically link modules into the compiler
data Linkable = LM {
  linkableTime     :: !UTCTime,          -- ^ Time at which this linkable was built
                                        -- (i.e. when the bytecodes were produced,
                                        --       or the mod date on the files)
  linkableModule   :: !Module,           -- ^ The linkable module itself
  linkableUnlinked :: [Unlinked]
    -- ^ Those files and chunks of code we have yet to link.
    --
    -- INVARIANT: A valid linkable always has at least one 'Unlinked' item.
 }

type LinkableSet = ModuleEnv Linkable

mkLinkableSet :: [Linkable] -> LinkableSet
mkLinkableSet ls = mkModuleEnv [(linkableModule l, l) | l <- ls]

unionLinkableSet :: LinkableSet -> LinkableSet -> LinkableSet
unionLinkableSet = plusModuleEnv_C go
  where
    go l1 l2
      | linkableTime l1 > linkableTime l2 = l1
      | otherwise = l2

instance Outputable Linkable where
  ppr (LM when_made mod unlinkeds)
     = (text "LinkableM" <+> parens (text (show when_made)) <+> ppr mod)
       $$ nest 3 (ppr unlinkeds)

type ObjFile = FilePath

-- | Objects which have yet to be linked by the compiler
data Unlinked
  = DotO ObjFile       -- ^ An object file (.o)
  | DotA FilePath      -- ^ Static archive file (.a)
  | DotDLL FilePath    -- ^ Dynamically linked library file (.so, .dll, .dylib)
  | BCOs CompiledByteCode
         [SptEntry]    -- ^ A byte-code object, lives only in memory. Also
                       -- carries some static pointer table entries which
                       -- should be loaded along with the BCOs.
                       -- See Note [Grand plan for static forms] in
                       -- "GHC.Iface.Tidy.StaticPtrTable".

instance Outputable Unlinked where
  ppr (DotO path)   = text "DotO" <+> text path
  ppr (DotA path)   = text "DotA" <+> text path
  ppr (DotDLL path) = text "DotDLL" <+> text path
  ppr (BCOs bcos spt) = text "BCOs" <+> ppr bcos <+> ppr spt

-- | An entry to be inserted into a module's static pointer table.
-- See Note [Grand plan for static forms] in "GHC.Iface.Tidy.StaticPtrTable".
data SptEntry = SptEntry Id Fingerprint

instance Outputable SptEntry where
  ppr (SptEntry id fpr) = ppr id <> colon <+> ppr fpr


isObjectLinkable :: Linkable -> Bool
isObjectLinkable l = not (null unlinked) && all isObject unlinked
  where unlinked = linkableUnlinked l
        -- A linkable with no Unlinked's is treated as a BCO.  We can
        -- generate a linkable with no Unlinked's as a result of
        -- compiling a module in NoBackend mode, and this choice
        -- happens to work well with checkStability in module GHC.

linkableObjs :: Linkable -> [FilePath]
linkableObjs l = [ f | DotO f <- linkableUnlinked l ]

-------------------------------------------

-- | Is this an actual file on disk we can link in somehow?
isObject :: Unlinked -> Bool
isObject (DotO _)   = True
isObject (DotA _)   = True
isObject (DotDLL _) = True
isObject _          = False

-- | Is this a bytecode linkable with no file on disk?
isInterpretable :: Unlinked -> Bool
isInterpretable = not . isObject

nameOfObject_maybe :: Unlinked -> Maybe FilePath
nameOfObject_maybe (DotO fn)   = Just fn
nameOfObject_maybe (DotA fn)   = Just fn
nameOfObject_maybe (DotDLL fn) = Just fn
nameOfObject_maybe (BCOs {})   = Nothing

-- | Retrieve the filename of the linkable if possible. Panic if it is a byte-code object
nameOfObject :: Unlinked -> FilePath
nameOfObject o = fromMaybe (pprPanic "nameOfObject" (ppr o)) (nameOfObject_maybe o)

-- | Retrieve the compiled byte-code if possible. Panic if it is a file-based linkable
byteCodeOfObject :: Unlinked -> CompiledByteCode
byteCodeOfObject (BCOs bc _) = bc
byteCodeOfObject other       = pprPanic "byteCodeOfObject" (ppr other)

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
                        --  loadDLL is platform-specific and adds the lib/.so/.DLL
                        --  suffixes platform-dependently

   | DLLPath FilePath   -- Absolute or relative pathname to a dynamic library
                        -- (ends with .dll or .so).

   | Framework String   -- Only used for darwin, but does no harm

instance Outputable LibrarySpec where
  ppr (Objects objs) = text "Objects" <+> ppr objs
  ppr (Archive a) = text "Archive" <+> text a
  ppr (DLL s) = text "DLL" <+> text s
  ppr (DLLPath f) = text "DLLPath" <+> text f
  ppr (Framework s) = text "Framework" <+> text s
