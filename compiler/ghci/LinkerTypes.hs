module LinkerTypes (
      DynLinker(..),
      PersistentLinkerState(..),
      LinkerUnitId(..)
    ) where

import GhcPrelude (FilePath, String)
import Data.Maybe (Maybe)
import Data.IORef (IORef)
import Control.Concurrent.MVar (MVar)
import BasicTypes (InstalledUnitId)
import ByteCodeTypes (ItblEnv)
import ByteCodeLink (ClosureEnv)

newtype DynLinker =
  DynLinker { dl_mpls :: IORef (MVar (Maybe PersistentLinkerState)) }

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
       pkgs_loaded :: ![LinkerUnitId],

       -- we need to remember the name of previous temporary DLL/.so
       -- libraries so we can link them (see #10322)
       temp_sos :: ![(FilePath, String)] }

-- TODO: Make this type more precise
type LinkerUnitId = InstalledUnitId
