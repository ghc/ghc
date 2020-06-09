-----------------------------------------------------------------------------
--
-- Types for the Dynamic Linker
--
-- (c) The University of Glasgow 2019
--
-----------------------------------------------------------------------------

module GHC.Runtime.Linker.Types (
      DynLinker(..),
      PersistentLinkerState(..),
      Linkable(..),
      Unlinked(..),
      SptEntry(..)
    ) where

import GHC.Prelude             ( FilePath, String, show )
import Data.Time               ( UTCTime )
import Data.Maybe              ( Maybe )
import Control.Concurrent.MVar ( MVar )
import GHC.Unit                ( UnitId, Module )
import GHC.ByteCode.Types      ( ItblEnv, CompiledByteCode )
import GHC.Utils.Outputable
import GHC.Types.Var           ( Id )
import GHC.Fingerprint.Type    ( Fingerprint )
import GHC.Types.Name.Env      ( NameEnv )
import GHC.Types.Name          ( Name )
import GHCi.RemoteTypes        ( ForeignHValue )

type ClosureEnv = NameEnv (Name, ForeignHValue)

newtype DynLinker =
  DynLinker { dl_mpls :: MVar (Maybe PersistentLinkerState) }

data PersistentLinkerState
  = PersistentLinkerState {

       -- Current global mapping from Names to their true values
       closure_env :: ClosureEnv,

       -- The current global mapping from RdrNames of DataCons to
       -- info table addresses.
       -- When a new Unlinked is linked into the running image, or an existing
       -- module in the image is replaced, the itbl_env must be updated
       -- appropriately.
       itbl_env    :: !ItblEnv,

       -- The currently loaded interpreted modules (home package)
       bcos_loaded :: ![Linkable],

       -- And the currently-loaded compiled modules (home package)
       objs_loaded :: ![Linkable],

       -- The currently-loaded packages; always object code
       -- Held, as usual, in dependency order; though I am not sure if
       -- that is really important
       pkgs_loaded :: ![UnitId],

       -- we need to remember the name of previous temporary DLL/.so
       -- libraries so we can link them (see #10322)
       temp_sos :: ![(FilePath, String)] }

-- | Information we can use to dynamically link modules into the compiler
data Linkable = LM {
  linkableTime     :: UTCTime,          -- ^ Time at which this linkable was built
                                        -- (i.e. when the bytecodes were produced,
                                        --       or the mod date on the files)
  linkableModule   :: Module,           -- ^ The linkable module itself
  linkableUnlinked :: [Unlinked]
    -- ^ Those files and chunks of code we have yet to link.
    --
    -- INVARIANT: A valid linkable always has at least one 'Unlinked' item.
    -- If this list is empty, the Linkable represents a fake linkable, which
    -- is generated in HscNothing mode to avoid recompiling modules.
    --
    -- ToDo: Do items get removed from this list when they get linked?
 }

instance Outputable Linkable where
  ppr (LM when_made mod unlinkeds)
     = (text "LinkableM" <+> parens (text (show when_made)) <+> ppr mod)
       $$ nest 3 (ppr unlinkeds)

-- | Objects which have yet to be linked by the compiler
data Unlinked
  = DotO FilePath      -- ^ An object file (.o)
  | DotA FilePath      -- ^ Static archive file (.a)
  | DotDLL FilePath    -- ^ Dynamically linked library file (.so, .dll, .dylib)
  | BCOs CompiledByteCode
         [SptEntry]    -- ^ A byte-code object, lives only in memory. Also
                       -- carries some static pointer table entries which
                       -- should be loaded along with the BCOs.
                       -- See Note [Grant plan for static forms] in
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

