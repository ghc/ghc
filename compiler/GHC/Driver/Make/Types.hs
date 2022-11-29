{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
module GHC.Driver.Make.Types where


import GHC.Prelude
import GHC.Driver.Monad
import GHC.Data.Maybe      ( expectJust )

import GHC.Utils.Outputable
import GHC.Utils.Misc
import GHC.Utils.Error

import GHC.Unit
import GHC.Unit.Env
import GHC.Unit.Module.Graph
import GHC.Unit.Home.ModInfo

import Control.Concurrent.MVar
import Data.IORef

import qualified Data.Map.Strict as M
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Maybe
import qualified Data.IntSet as I
import GHC.Types.Unique
import Debug.Trace
import GHC.Utils.Json

-- | Simple wrapper around MVar which allows a functor instance.
data ResultVar b = forall a . ResultVar (a -> b) (MVar (Maybe a))

deriving instance Functor ResultVar

mkResultVar :: MVar (Maybe a) -> ResultVar a
mkResultVar = ResultVar id

-- | Block until the result is ready.
waitResult :: ResultVar a -> MaybeT IO a
waitResult (ResultVar f var) = MaybeT (fmap f <$> readMVar var)

newtype MakeActionId = MakeActionId { getMakeActionId :: Int } deriving (Eq, Ord, Show)

instance Outputable MakeActionId where
  ppr (MakeActionId n) = ppr n

data BuildResult = BuildResult { _resultOrigin :: ResultOrigin
                               , resultMakeId :: MakeActionId -- ^ The corresponding Make action which are going to fill in
                               , resultVar    :: ResultVar (Maybe HomeModInfo, ModuleNameSet)
                               }

-- The origin of this result var, useful for debugging
data ResultOrigin = NoLoop | Loop ResultLoopOrigin

data ResultLoopOrigin = Initialise | Rehydrated | Finalised

instance Outputable ResultLoopOrigin where
  ppr Initialise = text "Initialise"
  ppr Rehydrated = text "Rehydrated"
  ppr Finalised = text "Finalised"

instance Outputable ResultOrigin where
  ppr (NoLoop) = text "NL"
  ppr (Loop ro) = text "L(" <> ppr ro <> text ")"

mkBuildResult :: ResultOrigin -> MakeActionId -> ResultVar (Maybe HomeModInfo, ModuleNameSet) -> BuildResult
mkBuildResult = BuildResult

data MakeActionT m = forall a b . MakeAction { make_deps   :: [BuildResult] -- Dependencies of this action
                                          , make_wait   :: m b    -- The action to run to get the result of these deps
                                          , make_action :: (b -> m a) -- How to build the action once the depenencies are ready
                                          , make_res_var :: (MVar (Maybe a)) -- Where to put the result of running the action
                                          , make_action_meta :: MakeActionMeta -- Meta information about the action
                                          }

makeAction :: MakeActionOrigin -> [BuildResult] -> ([BuildResult] -> m b) -> (b -> m a) -> (MVar (Maybe a)) -> BuildM (MakeActionT m)
makeAction make_action_meta_origin make_deps make_wait_deps make_action make_res_var = do
  make_action_meta_id <- makeId
  make_action_meta_timing <- IORefMaybe <$> liftIO (newIORef Nothing)
  let make_wait = make_wait_deps make_deps
      !make_action_meta_dep_ids = strictMap resultMakeId make_deps
      make_action_meta = MakeActionMeta{..}
      ma = MakeAction{..}
  reportNode ma
  return ma

-- | Record a new edge from the build graph
reportNode :: MakeActionT m -> BuildM ()
reportNode ma = do
  -- TODO: here we emit to eventlog and also store in memory?
    let mk_int = JSInt . getMakeActionId
    liftIO $ traceEventIO
      (showSDocUnsafe $ text "node:" <> renderJSON (JSObject [("node_id", mk_int (make_action_id ma))
                                             , ("node_deps", JSArray (map (mk_int . resultMakeId) (make_deps ma)))
                                             , ("node_desc", JSString (showSDocUnsafe (ppr (make_action_origin ma))))
                                             ]))

data MakeActionOrigin = MakeModule NodeKey | LoopSync

instance Outputable MakeActionOrigin where
  ppr (MakeModule nk) = text "M:" <+> ppr nk
  ppr LoopSync = text "LoopSync"

type MakeActionMeta = MakeActionMetaX IORefMaybe

-- | Separate data type as want to avoid retaining MVars pointing to the results.
data MakeActionMetaX f = MakeActionMeta { make_action_meta_origin :: !MakeActionOrigin -- Where the action originated from
                                       , make_action_meta_dep_ids :: ![MakeActionId]   -- Ids of the dependencies
                                       , make_action_meta_id :: !MakeActionId          -- Id of the current action
                                       , make_action_meta_timing :: !(f TimingInfo)      -- Information about how long the action took
                                       }

instance Outputable (f TimingInfo) => Outputable (MakeActionMetaX f) where
  ppr (MakeActionMeta o deps id timing) =
    text "ActionMeta:" <+> vcat [ text "id:" <+> ppr id
                                , text "deps:" <+> ppr deps
                                , text "origin:" <+> ppr o
                                , text "timing:" <+> ppr timing ]


traverseMakeActionMetaX :: Monad m => (forall a . f a -> m (g a)) -> MakeActionMetaX f -> m (MakeActionMetaX g)
traverseMakeActionMetaX nat (MakeActionMeta{..}) =
  nat make_action_meta_timing >>= \new -> return $ MakeActionMeta{make_action_meta_timing = new, ..}

newtype IORefMaybe a = IORefMaybe { getIORefMaybe :: IORef (Maybe a)}

make_action_id :: MakeActionT m -> MakeActionId
make_action_id = make_action_meta_id . make_action_meta
make_action_origin :: MakeActionT m -> MakeActionOrigin
make_action_origin = make_action_meta_origin . make_action_meta

data BuildLoopState = BuildLoopState { buildDep :: M.Map NodeKey BuildResult
                                          -- The current way to build a specific TNodeKey, without cycles this just points to
                                          -- the appropriate result of compiling a module  but with
                                          -- cycles there can be additional indirection and can point to the result of typechecking a loop
                                     , nNODE :: Int
                                     , nMAKE :: Int
                                     , hug_var :: MVar HomeUnitGraph
                                     -- A global variable which is incrementally updated with the result
                                     -- of compiling modules.
                                     }

nodeId :: BuildM Int
nodeId = do
  n <- gets nNODE
  modify (\m -> m { nNODE = n + 1 })
  return n

makeId :: BuildM MakeActionId
makeId = do
  n <- gets nMAKE
  modify (\m -> m { nMAKE = n + 1 })
  return (MakeActionId n)


setModulePipeline :: NodeKey -> BuildResult -> BuildM ()
setModulePipeline mgn build_result = do
  modify (\m -> m { buildDep = M.insert mgn build_result (buildDep m) })

type BuildMap = M.Map NodeKey BuildResult

getBuildMap :: BuildM BuildMap
getBuildMap = gets buildDep

getDependencies :: [NodeKey] -> BuildMap -> [BuildResult]
getDependencies direct_deps build_map =
  strictMap (expectJust "dep_map" . flip M.lookup build_map) direct_deps

type BuildM a = StateT BuildLoopState IO a

{-
Note [ModuleNameSet, efficiency and space leaks]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

During upsweep, the results of compiling modules are placed into a MVar. When we need
to compute the right compilation environment for a module, we consult this MVar and
set the HomeUnitGraph accordingly. This is done to avoid having to precisely track
module dependencies and recreating the HUG from scratch each time, which is very expensive.

In serial mode (-j1), this all works out fine: a module can only be compiled
after its dependencies have finished compiling, and compilation can't be
interleaved with the compilation of other module loops. This ensures that
the HUG only ever contains finalised interfaces.

In parallel mode, we have to be more careful: the HUG variable can contain non-finalised
interfaces, which have been started by another thread. In order to avoid a space leak
in which a finalised interface is compiled against a HPT which contains a non-finalised
interface, we have to restrict the HUG to only contain the visible modules.

The collection of visible modules explains which transitive modules are visible
from a certain point. It is recorded in the ModuleNameSet.
Before a module is compiled, we use this set to restrict the HUG to the visible
modules only, avoiding this tricky space leak.

Efficiency of the ModuleNameSet is of utmost importance, because a union occurs for
each edge in the module graph. To achieve this, the set is represented directly as an IntSet,
which provides suitable performance – even using a UniqSet (which is backed by an IntMap) is
too slow. The crucial test of performance here is the time taken to a do a no-op build in --make mode.

See test "jspace" for an example which used to trigger this problem.

-}

-- See Note [ModuleNameSet, efficiency and space leaks]
type ModuleNameSet = M.Map UnitId I.IntSet

addToModuleNameSet :: UnitId -> ModuleName -> ModuleNameSet -> ModuleNameSet
addToModuleNameSet uid mn s =
  let k = (getKey $ getUnique $ mn)
  in M.insertWith (I.union) uid (I.singleton k) s