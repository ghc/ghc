{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GHC.Core.Opt.DmdAnal.Framework
  ( -- * Applying demand annotations
    DmdAnnotations, annotateProgram
    -- * Evaluating a demand analysis framework
  , DmdFramework, evalFramework
    -- * Evaluation monad
  , EvalM, (:~>), writeIdDemand, writeBndrsDemands, writeIdDmdSig
    -- * Building a demand analysis framework
  , Builder, runBuilder, readLazyFVs, writeLazyFVs, trackCall, registerTransformer
  ) where

import GHC.Prelude

import GHC.Types.Demand
import GHC.Core
import GHC.Core.Utils
import GHC.Utils.Outputable
import GHC.Utils.Misc
import GHC.Types.Id
import GHC.Types.Id.Make
import GHC.Types.Var
import GHC.Types.Var.Env
import GHC.Types.Unique.FM
import GHC.Utils.Panic
import GHC.Data.Maybe
import GHC.Data.STuple
import GHC.Data.IOEnv
import GHC.Data.Bag

import Data.Array
import Data.Array.IO
import Data.Array.Unboxed (UArray)
import qualified Data.Array.Unboxed as UArr
import System.IO.Unsafe

import Control.Monad
import Control.Monad.Trans.State.Strict
import Control.Monad.ST

import Data.Coerce
import Data.Functor.Identity
import Data.IORef
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import qualified Data.Graph as Graph

import GHC.Utils.Trace
_ = pprTrace -- Tired of commenting out the import all the time

---------------------------------
-- Applying demand annotations --
---------------------------------

data DmdAnnotations = DA
  { da_demands :: !(IdEnv Demand)
  , da_sigs    :: !(IdEnv DmdSig)
  }

annotateProgram :: DmdAnnotations -> CoreProgram -> CoreProgram
annotateProgram anns = runIdentity . traverseBinders (Identity . annotate)
  where
    annotate bndr | isTyVar bndr = bndr
                  | otherwise    = annotate_sig $ annotate_demand bndr
    annotate_sig bndr
      | Just sig <- lookupVarEnv (da_sigs anns) bndr
      , sig /= idDmdSig bndr
      = bndr `setIdDmdSig` sig
      | otherwise
      = bndr
    annotate_demand bndr
      | Just dmd <- lookupVarEnv (da_demands anns) bndr
      , dmd /= idDemandInfo bndr
      = bndr `setIdDemandInfo` dmd
      | otherwise
      = bndr

-------------------------------
-- Evaluating a DmdFramework --
-------------------------------

-- | A monad in which fixed-point iteration takes place.
-- Internally stores the monotonically increasing approximations
-- for all 'Id's registered through 'registerTransformer'.
newtype EvalM a = EvalM (IOEnv EvalEnv a)
  deriving (Functor, Applicative, Monad)

runEvalM :: EvalEnv -> EvalM a -> IO a
runEvalM env (EvalM m) = runIOEnv env m

-- | Convenience synonym for a demand transformer
type eval_sd :~> dmd_ty = eval_sd -> EvalM dmd_ty

-- | A node for a function 'Id' in the dataflow framework, registered with
-- 'registerTransformer'.
data TransNode = TN
  { tn_id    :: !Id
  , tn_trans :: !(SubDemand :~> DmdType)
  -- ^ Demand transformer registered for 'tn_id'
  , tn_prio  :: !Prio
  -- ^ TopoSort priority wrt. the call graph, established by 'depAnal'.
  }

-- | A single-point approximation to a demand transformer
-- @SubDemand :~> DmdType@. The 'Int' tells us how often the approximated
-- demand transformer 'tn_trans' has been evaluated already.
data TransApprox = TransApprox !Int !SubDemand !DmdType

botTransApprox :: TransApprox
botTransApprox = TransApprox 0 botSubDmd botDmdType

instance Outputable TransApprox where
  ppr (TransApprox n inp outp) = ppr inp <> text "|-" <> ppr n <> text "->" <> ppr outp

-- | Write out a demand annotation. Delayed 'setIdDemand'.
writeIdDemand :: Id -> Demand -> EvalM ()
writeIdDemand id dmd = EvalM $ getEnv >>= \env ->
  liftIO $ modifyIORef' (ee_demands env) (\anns -> extendVarEnv anns id dmd)

-- | Write out a bunch of demand annotations. There might be 'TyVar's among the
-- `[Var]` which will be filtered out.
writeBndrsDemands :: HasCallStack => [Var] -> [Demand] -> EvalM ()
writeBndrsDemands vars dmds = EvalM $ getEnv >>= \env -> do
  let prs = strictZipWith (,) (filter isRuntimeVar vars) dmds
  liftIO $ modifyIORef' (ee_demands env) (`extendVarEnvList` prs)

-- | Write out a demand signature. Delayed 'setIdDmdSig'.
writeIdDmdSig :: Id -> DmdSig -> EvalM ()
writeIdDmdSig id sig = EvalM $ getEnv >>= \env ->
  liftIO $ modifyIORef' (ee_sigs env) (\anns -> extendVarEnv anns id sig)

-- | Write to the ref cell for the lazy free variables of this binding group.
-- See Note [Lazy and unleashable free variables].
writeLazyFVs :: [Id] -> DmdEnv -> EvalM ()
writeLazyFVs bind_ids lazy_fvs = EvalM $ getEnv >>= \env -> liftIO $ do
  lazy_fvs_env <- readIORef (ee_lazy_fvs env)
  case lookupVarEnv lazy_fvs_env (lazyFVRep bind_ids) of
    Nothing -> writeIORef (ee_changed env) True
    Just old_lazy_fvs -> do
      modifyIORef (ee_changed env) (|| old_lazy_fvs /= lazy_fvs)
      -- when (old_lazy_fvs /= lazy_fvs) $ do
      --   pprTraceM "Lazy FV change" (ppr (lazyFVRep bind_ids) $$ ppr (diffUFM old_lazy_fvs lazy_fvs))
  changed <- readIORef (ee_changed env)
  when changed $
    writeIORef (ee_lazy_fvs env) (extendVarEnv lazy_fvs_env (lazyFVRep bind_ids) lazy_fvs)

-- | Read out the lazy free variables of this binding group.
-- See Note [Lazy and unleashable free variables].
readLazyFVs :: [Id] -> EvalM DmdEnv
readLazyFVs bind_ids = EvalM $ getEnv >>= \env -> do
  lazy_fv_env <- liftIO $ readIORef (ee_lazy_fvs env)
  return $! fromMaybe emptyDmdEnv $ lookupVarEnv lazy_fv_env (lazyFVRep bind_ids)

-- | Pick one of the incoming Ids as the representative for the lazy FVs of that
-- binding group
lazyFVRep :: [Id] -> Id
lazyFVRep = head

-- | Internal function that evaluates a call to the given 'Id', which must have
-- been registered through 'registerTransformer' before.
-- Doesn't to any dependency tracking by itself, just implements the intricacies
-- of the call site. Clients will go through 'trackCall'.
evalCall :: Id -> (SubDemand :~> DmdType)
evalCall callee eval_sd = EvalM $ getEnv >>= \env -> liftIO $ do
  let prio = node2prio env callee
  TransApprox n inp outp <- readTransApprox env prio
  let inp' = inp `lubSubDmd` eval_sd
  when (inp /= inp') $ do -- change in input. callee might be unstable
    let approx' = TransApprox n inp' outp
    writeTransApprox env prio approx'
    enqueueUnstable env prio
  -- Now, if id really is unstable, evaluate it directly instead
  -- of finishing this transformer and returning to it afterwards in the next
  -- iteration of the fixed-point loop in 'evalFramework'.
  -- But only do so if we haven't already visited as part of this
  -- iteration of the fixed-point loop!
  callee_unstable <- isUnstable env prio
  first_visit <- not <$> hasBeenVisited env prio
  -- pprTraceM "do_call" (ppr callee <+> ppr inp' <+> ppr callee_unstable <+> ppr first_visit)
  let do_call = callee_unstable && first_visit
  if not do_call
    then return outp
    else do
      TransApprox _ _ outp' <- evalNode env prio
      return outp'

-----------------------------
-- Building a DmdFramework --
-----------------------------

newtype Prio = Prio { unPrio :: Int } deriving (Eq, Ord, Ix, Outputable)

newtype PrioSet = PrioSet IntSet

-- | A data-flow framework for computing the demand properties of a
-- 'CoreProgram'. Constructed with 'runBuilder', evaluated with 'evalFramework'.
data DmdFramework
  = DF
  { df_trans_nodes    :: !(IdEnv TransNode)
  , df_referrer_prios :: !(Prio -> PrioSet)
  , df_prio2node      :: !(Prio -> Id)
  }

data BuilderState = BS
  { bs_cur_id         :: !Id
  , bs_deps           :: !(IdEnv (Bag Id))
  , bs_trans_nodes    :: !(IdEnv TransNode)
  }

newtype Builder a = Builder (State BuilderState a)
  deriving (Functor, Applicative, Monad, MonadFix)

-- | Register a call dependency on the callee 'Id' and return a demand
-- transformer that properly denotes a call to said callee.
-- The callee must be registered with 'registerTransformer', before or after
-- the call to 'trackCall'.
trackCall :: Id -> Builder (SubDemand :~> DmdType)
trackCall callee = do
  Builder $ modify' $ \bs ->
    bs{bs_deps = extendVarEnv_Acc (flip snocBag) unitBag (bs_deps bs) (bs_cur_id bs) callee}
  pure $ evalCall callee

registerTransformer :: Id -> Builder (SPair (SubDemand :~> DmdType) a) -> Builder a
registerTransformer id (Builder build_id) = Builder $ do
  old_id <- bs_cur_id <$> get
  modify' (\bs -> bs{bs_cur_id=id})
  S2 trans a <- build_id
  let !node = TN id trans (Prio $ -1) -- we'll replace the -1 in depAnal
  modify' $ \bs@BS{bs_trans_nodes = nodes} ->
    bs {bs_trans_nodes = extendVarEnv nodes id node, bs_cur_id = old_id}
  return a

-- | Build a 'DmdFramework' from a denotation of the 'CoreProgram' in 'EvalM'.
runBuilder :: Builder (() :~> DmdType) -> DmdFramework
runBuilder build = framework
  where
    Builder m = registerTransformer rootId $ do
      denot <- build
      return $! S2 (\_ -> denot ()) ()
    ((), BS _ deps trans_nodes) = runState m (BS rootId emptyVarEnv emptyVarEnv )
    framework = depAnal deps trans_nodes

rootId :: Id
rootId = voidPrimId -- because it's convenient. Any other wired-in Id that we
                    -- never actually encounter would work, too

-- | `depAnal deps nodes` performs dependency analysis on the call graph `deps`
-- so that we know for each 'TransNode' in `nodes`
--
--  1. Its topo-sort number, stored as a 'Prio' in the 'TransNode'
--  2. A set of callers for each node
--
-- Since 'evalFramework' indexes nodes by priority rather than 'Id' unique,
-- we store (2) as a mapping from 'Prio' to 'PrioSet' in 'df_referrer_prios',
-- which is backed by an unboxed array. The reverse mapping of (1) is stored in
-- 'df_prio2node', likewise backed by an unboxed array.
depAnal :: IdEnv (Bag Id) -> IdEnv TransNode -> DmdFramework
depAnal deps nodes = runST $ {-# SCC "depAnal" #-} do
  let bounds = (0, sizeUFM nodes - 1)
  let vtx2node_arr = listArray bounds $ map tn_id $ nonDetEltsUFM nodes
  let node2vtx_env = mkVarEnv $ map (\(a,b) -> (b,a)) $ assocs vtx2node_arr
  let vtx2node vtx = vtx2node_arr ! vtx
  let node2vtx id = expectJust "node2vtx" $ lookupVarEnv node2vtx_env id
  let deps_graph = map node2vtx . maybe [] bagToList . lookupVarEnv deps <$> vtx2node_arr
  let priorities = reverse $ Graph.topSort deps_graph -- Urgh, we have to wait for reverseTopSort
  let prio2vtx_arr = UArr.listArray bounds priorities :: UArray Int Int
  let vtx2prio_arr = UArr.array     bounds $ zipWith (\p n -> (n, p)) [0..] priorities :: UArray Int Int
  let prio2vtx prio = prio2vtx_arr UArr.! unPrio prio
  let vtx2prio vtx  = Prio $ vtx2prio_arr UArr.! vtx
  let nodes_with_prios = mapVarEnv (\node -> node{tn_prio = vtx2prio $ node2vtx $ tn_id node}) nodes
      -- referrers is the transposed graph of deps
  let referrers = Graph.transposeG deps_graph :: Array Int [Int]
      -- ... but we should rather cache the *priorities* of those referrers,
      -- because that's the set that we will keep on adding to our priority queue.
      vtx2prio_assoc (vtx,refs) = (vtx2prio vtx, coerce IntSet.fromList $ map vtx2prio refs)
      referrer_prios = array (coerce bounds) $ map vtx2prio_assoc $ assocs referrers
      prio2node = vtx2node . prio2vtx
  -- let pprArr arr = ppr (assocs arr)
  -- let pprUArr arr = ppr (UArr.assocs arr)
  -- pprTraceM "depAnal" (vcat [pprArr deps, ppr priorities, pprUArr node2prio_arr, pprUArr prio2node_arr, pprArr referrers, pprArr referrer_prios])
  return $! DF { df_trans_nodes = nodes_with_prios
               , df_prio2node = prio2node
               , df_referrer_prios = (referrer_prios UArr.!) }

-------------------------------
-- Evaluating a DmdFramework --
-------------------------------

data EvalEnv
  = EE
  { ee_framework :: !DmdFramework
  , ee_approxs   :: !(IOArray Prio TransApprox)
  , ee_unstable  :: !(IORef PrioSet)
  , ee_visited   :: !(IORef PrioSet)
  , ee_changed   :: !(IORef Bool)
  , ee_lazy_fvs  :: !(IORef (IdEnv DmdEnv))
  , ee_demands   :: !(IORef (IdEnv Demand))
  , ee_sigs      :: !(IORef (IdEnv DmdSig))
  }

initEvalEnv :: DmdFramework -> IO EvalEnv
initEvalEnv fw = do
  let !n = sizeUFM (df_trans_nodes fw)
  approxs  <- newArray (Prio 0, Prio $ n-1) botTransApprox
  unstable <- newIORef $ PrioSet $ IntSet.fromDistinctAscList [0..n-1]
  visited  <- newIORef $ PrioSet IntSet.empty
  changed  <- newIORef False
  lazy_fvs <- newIORef emptyVarEnv
  demands  <- newIORef emptyVarEnv
  sigs     <- newIORef emptyVarEnv
  return $! EE fw approxs unstable visited changed lazy_fvs demands sigs

node2prio :: EvalEnv -> Id -> Prio
node2prio env id = expectJust "node2prio" $ tn_prio <$> lookupVarEnv (df_trans_nodes (ee_framework env)) id

prio2node :: EvalEnv -> Prio -> Id
prio2node env = df_prio2node (ee_framework env)

getTransNode :: EvalEnv -> Id -> TransNode
getTransNode env id =
  expectJust "getTransNode" $ lookupVarEnv (df_trans_nodes (ee_framework env)) id

referrers :: EvalEnv -> Prio -> [Id]
referrers env n = map (prio2node env) $ coerce IntSet.toList prios
  where
    prios = referrerPrios env n

referrerPrios :: EvalEnv -> Prio -> PrioSet
referrerPrios env n = referrers `seq` -- keep alive referrers for trace messages
  df_referrer_prios (ee_framework env) n

dequeueUnstable :: EvalEnv -> IO (Maybe Prio)
dequeueUnstable env = do
  queue <- readIORef (ee_unstable env)
  case coerce IntSet.minView queue of
    Nothing -> return Nothing
    Just (!next, !queue') -> do
      writeIORef (ee_unstable env) queue'
      return $! Just $! next

deleteUnstable :: EvalEnv -> Prio -> IO ()
deleteUnstable env n = modifyIORef' (ee_unstable env) (coerce IntSet.delete n)

isUnstable :: EvalEnv -> Prio -> IO Bool
isUnstable env n = -- pprTrace "isUnstable" (ppr n) $
  coerce IntSet.member n <$> readIORef (ee_unstable env)

enqueueUnstable :: EvalEnv -> Prio -> IO ()
enqueueUnstable env n = -- pprTrace "enqueueUnstable" (ppr n) $
  modifyIORef' (ee_unstable env) $ coerce IntSet.insert n

enqueueUnstableReferrers :: EvalEnv -> Prio -> IO ()
enqueueUnstableReferrers env changed_node_prio = do
  -- pprTraceM "enqueueUnstableReferrers" (ppr (referrers env changed_node_id) $$ ppr (referrerPrios env changed_node_id))
  modifyIORef' (ee_unstable env) $ \queue ->
    coerce IntSet.union queue $ referrerPrios env changed_node_prio

readTransApprox :: EvalEnv -> Prio -> IO TransApprox
readTransApprox env = readArray (ee_approxs env)

writeTransApprox :: EvalEnv -> Prio -> TransApprox -> IO ()
writeTransApprox env = writeArray (ee_approxs env)

clearVisited :: EvalEnv -> IO ()
clearVisited env = writeIORef (ee_visited env) (coerce IntSet.empty)

addVisited :: EvalEnv -> Prio -> IO ()
addVisited env prio = modifyIORef' (ee_visited env) (coerce IntSet.insert prio)

hasBeenVisited :: EvalEnv -> Prio -> IO Bool
hasBeenVisited env n = coerce IntSet.member n <$> readIORef (ee_visited env)

finaliseEvalEnv :: EvalEnv -> IO DmdAnnotations
finaliseEvalEnv env = DA <$> readIORef (ee_demands env) <*> readIORef (ee_sigs env)

{-# INLINE whileJust_ #-}
whileJust_ :: Monad m => m (Maybe a) -> (a -> m ()) -> m ()
whileJust_ cond body = loop
  where
    loop = do
      mb <- cond
      case mb of
        Just mb -> body mb >> loop
        Nothing -> return ()

evalFramework :: DmdFramework -> DmdAnnotations
evalFramework fw = {-# SCC "evalFramework" #-} unsafePerformIO $ do
  !env <- initEvalEnv fw
  let root_prio = node2prio env rootId
  writeTransApprox env root_prio (TransApprox 0 topSubDmd nopDmdType)
  _ <- evalNode env root_prio
  whileJust_ (dequeueUnstable env) $ \node_prio -> do
    _new_approx <- evalNode env node_prio
    clearVisited env
  finaliseEvalEnv env

withFreshChangedFlag :: EvalEnv -> IO a -> IO a
withFreshChangedFlag env m = do
  old_changed <- readIORef (ee_changed env)
  a <- m
  writeIORef (ee_changed env) old_changed
  return a

evalNode :: EvalEnv -> Prio -> IO TransApprox
evalNode env node_prio = withFreshChangedFlag env $ do
  deleteUnstable env node_prio
  addVisited env node_prio
  let node_id = prio2node env node_prio
  let node = getTransNode env node_id
  _approx@(TransApprox n inp _outp) <- readTransApprox env node_prio
  -- pprTraceM "evalNode {" (ppr node_id <+> parens (ppr node_prio) <> colon <+> ppr _approx)
  outp' <- if n < 10
             then runEvalM env (tn_trans node inp)
             else pprPanic "TODO: think hard about abortion" (ppr (tn_id node) $$ ppr _approx)
  -- re-read, because there might have been a self-loop updating the input
  approx@(TransApprox n inp' outp) <- readTransApprox env node_prio
  let !approx' = TransApprox (n+1) inp' outp'
  -- but the output shouldn't have changed yet
  massertPpr (_outp == outp) (text "output changed" $$ ppr (tn_id node) $$ ppr _approx $$ ppr approx $$ ppr approx')
  -- Unfortunately, the following is not True due to the fact that we split off lazy FVs:
  -- massertPpr (stripBoxityDmdType (outp `lubDmdType` outp') == stripBoxityDmdType outp') (text "non-monotone" $$ ppr id $$ ppr _approx $$ ppr approx $$ ppr approx' $$ ppr (outp `lubDmdType` outp'))
  when (outp /= outp') $
    writeIORef (ee_changed env) True
  changed <- readIORef (ee_changed env)
  let _pp_chg = (if changed then text "CHANGED" else text "no change") <+> ppr (n+1)
  -- pprTraceM "evalNode }" (ppr node_id <+> parens _pp_chg <> colon <+> ppr outp' <+> if changed then ppr (referrers env node_prio) else empty)
  writeTransApprox env node_prio approx'
  when changed $ do
    enqueueUnstableReferrers env node_prio
  return approx'
