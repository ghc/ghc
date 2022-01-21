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
import GHC.Data.Graph.UnVar

import Data.Array
import Data.Array.IO
import Data.Array.Unboxed (UArray)
import qualified Data.Array.Unboxed as UArr
import System.IO.Unsafe

import Control.Monad
import Control.Monad.Trans.State.Strict
import Control.Monad.ST

import Data.Functor.Identity
import Data.IORef
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
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
--
-- This monad can be thought of as a model of the type
--
-- > type EvalM a = ReaderT (IdEnv TransApprox) (Writer Usg a)
-- > data TransApprox = ... -- see below
-- > type Usg = (DmdAnnotations, IdEnv SubDmd) -- annotations and how functions had been called
--
-- But the actual implementation is in terms of mutable state
-- and does a bunch of optimisations to speed up fixed-point
-- iteration.
newtype EvalM a = EvalM { unEvalM :: IterM a }
  deriving (Functor, Applicative, Monad)

-- | Convenience synonym for a demand transformer
type eval_sd :~> dmd_ty = eval_sd -> EvalM dmd_ty

-- | A node for a function 'Id' in the dataflow framework, registered with
-- 'registerTransformer'.
data TransNode = TN
  { tn_id    :: !Id
  , tn_trans :: !(SubDemand :~> DmdType)
  -- ^ Demand transformer registered for 'tn_id'
  , tn_parent_id :: !Id
  -- ^ The 'Id' in the RHS of which 'tn_id' is defined. Will be marked unstable
  -- when lazy_fvs of 'tn_id' change.
  , tn_rec_group :: !UnVarSet
  -- ^ The 'Id's in the same rec group as this function.
  , tn_prio  :: !Prio
  -- ^ TopoSort priority wrt. the call graph, established by 'depAnal'.
  -- Roughly corresponds to lexical nesting depth; a nested function will
  -- have a higher priority than the containing function.
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
writeIdDemand id dmd = EvalM $
  modifyIterRef' ie_demands (\anns -> extendVarEnv anns id dmd)

-- | Write out a bunch of demand annotations. There might be 'TyVar's among the
-- `[Var]` which will be filtered out.
writeBndrsDemands :: [Var] -> [Demand] -> EvalM ()
writeBndrsDemands vars dmds = EvalM $ do
  let prs = strictZipWith (,) (filter isRuntimeVar vars) dmds
  modifyIterRef' ie_demands (`extendVarEnvList` prs)

-- | Write out a demand signature. Delayed 'setIdDmdSig'.
writeIdDmdSig :: Id -> DmdSig -> EvalM ()
writeIdDmdSig id sig = EvalM $
  modifyIterRef' ie_sigs (\anns -> extendVarEnv anns id sig)

-- | Write to the ref cell for the lazy free variables of this binding group.
-- See Note [Lazy and unleashable free variables].
writeLazyFVs :: [Id] -> DmdEnv -> EvalM ()
writeLazyFVs bind_ids new_lazy_fvs = EvalM $ do
  lazy_fvs_env <- readIterRef ie_lazy_fvs
  let !old_lazy_fvs = fromMaybe emptyVarEnv $ lookupVarEnv lazy_fvs_env (lazyFVRep bind_ids)
  when (old_lazy_fvs /= new_lazy_fvs) $ do
    writeIterRef ie_lazy_fvs (extendVarEnv lazy_fvs_env (lazyFVRep bind_ids) new_lazy_fvs)
    -- pprTraceM "Lazy FV change" (ppr (lazyFVRep bind_ids) $$ ppr (diffUFM old_lazy_fvs new_lazy_fvs))
    fw <- getFramework
    let node = getTransNode fw (lazyFVRep bind_ids)
    enqueueUnstableOut (node2prio fw (tn_parent_id node))

-- | Read out the lazy free variables of this binding group.
-- See Note [Lazy and unleashable free variables].
readLazyFVs :: [Id] -> EvalM DmdEnv
readLazyFVs bind_ids = EvalM $ do
  lazy_fv_env <- readIterRef ie_lazy_fvs
  return $! fromMaybe emptyDmdEnv $ lookupVarEnv lazy_fv_env (lazyFVRep bind_ids)

-- | Pick one of the incoming Ids as the representative for the lazy FVs of that
-- binding group
lazyFVRep :: [Id] -> Id
lazyFVRep = head

-----------------------------
-- Building a DmdFramework --
-----------------------------

type Prio = Int
type PrioSet = IntSet
type PrioMap a = IntMap a

-- | A data-flow framework for computing the demand properties of a
-- 'CoreProgram'. Constructed with 'runBuilder', evaluated with 'evalFramework'.
data DmdFramework
  = DF
  { df_trans_nodes    :: !(IdEnv TransNode)
  , df_referrer_prios :: !(Prio -> PrioSet)
  , df_prio2node      :: !(Prio -> Id)
  }

data BuilderState = BS
  { bs_defining_id    :: !Id
  -- ^ 'Id's which currently register stuff, including the 'bs_defining_id'
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
trackCall callee = Builder $ do
  caller <- gets bs_defining_id
  modify' $ \bs ->
    bs{bs_deps = extendVarEnv_Acc (flip snocBag) unitBag (bs_deps bs) caller callee}
  pure (EvalM . evalCall caller callee)

registerTransformer :: Id -> UnVarSet -> Builder (SPair (SubDemand :~> DmdType) a) -> Builder a
registerTransformer id bind_group (Builder build_id) = Builder $ do
  defining_id  <- gets bs_defining_id
  modify' (\bs -> bs{bs_defining_id=id})
  S2 trans a <- build_id
  let !node = TN id trans defining_id bind_group (-1) -- we'll replace the -1 in depAnal
  modify' $ \bs@BS{bs_trans_nodes = nodes} ->
    bs {bs_trans_nodes = extendVarEnv nodes id node, bs_defining_id = defining_id}
  return a

-- | Build a 'DmdFramework' from a denotation of the 'CoreProgram' in 'EvalM'.
runBuilder :: Builder (() :~> DmdType) -> DmdFramework
runBuilder build = framework
  where
    Builder m = registerTransformer rootId (mkUnVarSet [rootId]) $ do
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
  let priorities = Graph.topSort deps_graph
  let prio2vtx_arr = UArr.listArray bounds priorities :: UArray Prio Graph.Vertex
  let vtx2prio_arr = UArr.array     bounds $ zipWith (\p n -> (n, p)) [0..] priorities :: UArray Graph.Vertex Prio
  let prio2vtx prio = prio2vtx_arr UArr.! prio
  let vtx2prio vtx  = vtx2prio_arr UArr.! vtx
  let nodes_with_prios = mapVarEnv (\node -> node{tn_prio = vtx2prio $ node2vtx $ tn_id node}) nodes
      -- referrers is the transposed graph of deps
  let referrers = Graph.transposeG deps_graph :: Array Graph.Vertex [Graph.Vertex]
      -- ... but we should rather cache the *priorities* of those referrers,
      -- because that's the set that we will keep on adding to our priority queue.
      vtx2prio_assoc (vtx,refs) = (vtx2prio vtx, IntSet.fromList $ map vtx2prio refs)
      referrer_prios = array bounds $ map vtx2prio_assoc $ assocs referrers
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

data IterEnv
  = EE
  { ie_framework    :: !DmdFramework
  -- Fields related to current approximations:
  , ie_approxs      :: !(IOArray Prio TransApprox)
  , ie_lazy_fvs     :: !(IORef (IdEnv DmdEnv))
  -- Fields related to fixed-point iteration:
  , ie_unstable_in  :: !(IORef PrioSet)
  , ie_unstable_out :: !(IORef PrioSet)
  , ie_visited      :: !(IORef PrioSet)
  , ie_active_evals :: !(IORef (PrioMap PrioSet))
  -- ^ Domain is priorities of function 'Id's currently under evaluation.
  -- In an interpreter, we'd say these functions are on the stack.
  -- Due to our invariants around 'ie_visited', every function may be on
  -- this stack at most once.
  -- We don't care for the order of the stack, only for efficient mapping.
  -- What it maps to are priorities of other functions that the corresponding
  -- function has finished calling during the current evaluation.
  -- These must be a subset of all the functions the function calls, e.g.,
  -- what is statically approximated in 'df_referrer_prios' of 'ie_framework'.

  -- Fields related to demand annotations:
  , ie_demands      :: !(IORef (IdEnv Demand))
  , ie_sigs         :: !(IORef (IdEnv DmdSig))
  }

newtype IterM a = IterM (IOEnv IterEnv a)
  deriving (Functor, Applicative, Monad)

liftIOEnv :: (IterEnv -> IO a) -> IterM a
liftIOEnv f = IterM $ getEnv >>= \env -> liftIO (f env)
{-# INLINE liftIOEnv #-}

readIterRef :: (IterEnv -> IORef a) -> IterM a
readIterRef ref = liftIOEnv $ \env ->
  liftIO $ readIORef (ref env)
{-# INLINE readIterRef #-}

writeIterRef :: (IterEnv -> IORef a) -> a -> IterM ()
writeIterRef ref a = liftIOEnv $ \env ->
  liftIO $ writeIORef (ref env) a
{-# INLINE writeIterRef #-}

modifyIterRef' :: (IterEnv -> IORef a) -> (a -> a) -> IterM ()
modifyIterRef' ref f = liftIOEnv $ \env ->
  liftIO $ modifyIORef' (ref env) f
{-# INLINE modifyIterRef' #-}

getFramework :: IterM DmdFramework
getFramework = ie_framework <$> IterM getEnv
{-# INLINE getFramework #-}

initIterEnv :: DmdFramework -> IO IterEnv
initIterEnv fw = do
  let !n = sizeUFM (df_trans_nodes fw)
  approxs      <- newArray (0, n-1) botTransApprox
  lazy_fvs     <- newIORef emptyVarEnv
  unstable_in  <- newIORef IntSet.empty
  unstable_out <- newIORef IntSet.empty
  visited      <- newIORef IntSet.empty
  active_evals <- newIORef IntMap.empty
  demands      <- newIORef emptyVarEnv
  sigs         <- newIORef emptyVarEnv
  return $! EE fw approxs lazy_fvs unstable_in unstable_out visited active_evals demands sigs

node2prio :: DmdFramework -> Id -> Prio
node2prio fw id = tn_prio $ getTransNode fw id

prio2node :: DmdFramework -> Prio -> Id
prio2node = df_prio2node

getTransNode :: DmdFramework -> Id -> TransNode
getTransNode fw id =
  expectJust "getTransNode" $ lookupVarEnv (df_trans_nodes fw) id

referrers :: DmdFramework -> Prio -> [Id]
referrers fw n = map (df_prio2node fw) $ IntSet.toList $ referrerPrios fw n

referrerPrios :: DmdFramework -> Prio -> PrioSet
referrerPrios fw n = referrers `seq` -- keep alive referrers for trace messages
  df_referrer_prios fw n

dequeueUnstableIn :: IterM (Maybe Prio)
dequeueUnstableIn = do
  queue <- readIterRef ie_unstable_in
  case IntSet.minView queue of
    Nothing -> return Nothing
    Just (!next, !queue') -> do
      writeIterRef ie_unstable_in queue'
      return $! Just $! next

dequeueUnstableOut :: IterM (Maybe Prio)
dequeueUnstableOut = do
  queue <- readIterRef ie_unstable_out
  case IntSet.maxView queue of
    Nothing -> return Nothing
    Just (!next, !queue') -> do
      writeIterRef ie_unstable_out queue'
      return $! Just $! next

deleteUnstable :: Prio -> IterM ()
deleteUnstable n = do
  modifyIterRef' ie_unstable_in  (IntSet.delete n)
  modifyIterRef' ie_unstable_out (IntSet.delete n)

enqueueUnstableIn :: Prio -> IterM ()
enqueueUnstableIn n =
  modifyIterRef' ie_unstable_in $ IntSet.insert n

enqueueUnstableOut :: Prio -> IterM ()
enqueueUnstableOut n =
  modifyIterRef' ie_unstable_out $ IntSet.insert n

enqueueUnstableReferrers :: Prio -> IterM ()
enqueueUnstableReferrers changed_node_prio = do
  active_evals <- readIterRef ie_active_evals
  fw <- getFramework
  let static_referrers = referrerPrios fw changed_node_prio
  -- Some of the referrers might be actively eval'd at the moment.
  -- We do finer-grained dependency tracking for those, in the PrioSets
  -- carried in ie_active_evals.
      !active_referrers = IntMap.restrictKeys active_evals static_referrers
      !inactive_referrers = IntSet.difference static_referrers (IntMap.keysSet active_referrers)

      is_affected calls = IntSet.member changed_node_prio calls
      !affected_active_referrers = IntMap.keysSet $ IntMap.filter is_affected active_referrers
      !dynamic_referrers = inactive_referrers `IntSet.union` affected_active_referrers
  massertPpr (inactive_referrers `IntSet.isSubsetOf` static_referrers) (ppr inactive_referrers $$ ppr static_referrers)
  massertPpr (affected_active_referrers `IntSet.isSubsetOf` static_referrers) (ppr active_referrers $$ ppr static_referrers)
  -- pprTraceM "enqueueUnstableReferrers" (ppr changed_node_prio $$ ppr (referrers env changed_node_prio) $$ ppr (referrerPrios env changed_node_prio) $$ ppr active_referrers $$ ppr affected_active_referrers)
  modifyIterRef' ie_unstable_out $ \queue ->
    -- The following is conservative and simple:
    -- IntSet.union queue static_referrers
    -- The following is conservative and more precise (a subset of the former), but a little more complex:
    IntSet.union queue dynamic_referrers

readTransApprox :: Prio -> IterM TransApprox
readTransApprox n = liftIOEnv $ \env -> readArray (ie_approxs env) n

writeTransApprox :: Prio -> TransApprox -> IterM ()
writeTransApprox n approx = liftIOEnv $ \env -> writeArray (ie_approxs env) n approx

clearVisited :: IterM ()
clearVisited = writeIterRef ie_visited IntSet.empty

addVisited :: Prio -> IterM ()
addVisited prio = modifyIterRef' ie_visited (IntSet.insert prio)

hasBeenVisited :: Prio -> IterM Bool
hasBeenVisited n = IntSet.member n <$> readIterRef ie_visited

trackDynamicCall :: Prio -> Prio -> IterM ()
trackDynamicCall caller_prio callee_prio = do
  modifyIterRef' ie_active_evals (IntMap.adjust (IntSet.insert callee_prio) caller_prio)

-- | Internal function that evaluates a call to the given 'Id', which must have
-- been registered through 'registerTransformer' before.
-- Doesn't to any dependency tracking by itself, just implements the intricacies
-- of the call site. Clients will go through 'trackCall'.
evalCall :: Id -> Id -> SubDemand -> IterM DmdType
evalCall caller callee eval_sd = do
  fw <- getFramework
  let caller_node = getTransNode fw caller
  let callee_node = getTransNode fw callee
  let caller_prio = tn_prio caller_node
  let callee_prio = tn_prio callee_node
  TransApprox n_callee inp outp <- readTransApprox callee_prio
  TransApprox n_caller _ _ <- readTransApprox caller_prio
  let inp' = inp `lubSubDmd` eval_sd
  when (inp /= inp') $ do -- change in input. callee might be unstable
    let approx' = TransApprox n_callee inp' outp
    writeTransApprox callee_prio approx'
    enqueueUnstableIn callee_prio
  -- Now, if id really is unstable, evaluate it directly instead
  -- of finishing this transformer and returning to it afterwards in the next
  -- iteration of the fixed-point loop in 'evalFramework'.
  -- But only do so if we haven't already visited as part of this
  -- iteration of the fixed-point loop!
  callee_unstable_in  <- IntSet.member callee_prio <$> readIterRef ie_unstable_in
  callee_unstable_out <- IntSet.member callee_prio <$> readIterRef ie_unstable_out
  let higher_prio      = callee_prio > caller_prio
  let lower_iter_count = n_callee < n_caller -- >= as opposed to > helps in haddock's Parser
  let same_rec_group   = caller `elemUnVarSet` tn_rec_group callee_node
  first_visit <- not <$> hasBeenVisited callee_prio
  -- pprTraceM "do_call" (ppr callee <+> ppr inp' <+> ppr callee_unstable <+> ppr first_visit <+> ppr higher_prio)
  let do_call = (callee_unstable_in || callee_unstable_out && (higher_prio || same_rec_group || lower_iter_count)) && first_visit
  -- let do_call = (callee_unstable_in || callee_unstable_out && (higher_prio || same_rec_group || True)) && first_visit
  outp' <- if not do_call
    then return outp
    else do
      TransApprox _ _ outp' <- evalNode callee_node
      return outp'
  trackDynamicCall caller_prio callee_prio
  return outp'

execIterM :: DmdFramework -> IterM () -> DmdAnnotations
execIterM fw (IterM m) = unsafePerformIO $ do
  !env <- initIterEnv fw
  runIOEnv env m
  DA <$> readIORef (ie_demands env) <*> readIORef (ie_sigs env)

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
evalFramework fw = {-# SCC "evalFramework" #-} execIterM fw $ do
  let root_prio = tn_prio $ getTransNode fw rootId
  writeTransApprox root_prio (TransApprox 0 topSubDmd nopDmdType)
  enqueueUnstableIn root_prio
  let ma <|> mb = ma >>= \a -> case a of Nothing -> mb; _ -> pure a
  whileJust_ (dequeueUnstableIn <|> dequeueUnstableOut) $ \node_prio -> do
    clearVisited
    -- unstable <- IntSet.union <$> readIterRef ie_unstable_in <*> readIterRef ie_unstable_out
    -- pprTraceM "unstable" (ppr (take 10 (IntSet.toDescList unstable)))
    void $ evalNode (getTransNode fw (prio2node fw node_prio))

prepareNextEval :: TransNode -> IterM a -> IterM a
prepareNextEval node m = do
  -- The following assert is in place to guarantee we don't re-evaluate
  -- the same node as part of the same iteration. If for some reason we
  -- decide to do that in the future, we have to think of a different
  -- data structure for ie_active_evals.
  let node_prio = tn_prio node
  assertPprM (not <$> hasBeenVisited node_prio) (ppr (tn_id node) <+> ppr node_prio)
  addVisited node_prio
  modifyIterRef' ie_active_evals (IntMap.insert node_prio IntSet.empty)
  deleteUnstable node_prio -- eval is imminent, so this node is now considered stable
  a <- m
  modifyIterRef' ie_active_evals (IntMap.delete node_prio)
  return a

evalNode :: TransNode -> IterM TransApprox
evalNode node = prepareNextEval node $ do
  _fw <- getFramework
  let _node_id   = tn_id node
  let node_prio = tn_prio node
  _approx@(TransApprox n inp _outp) <- readTransApprox node_prio
  -- pprTraceM "evalNode {" (ppr _node_id <+> parens (ppr node_prio) <> colon <+> ppr _approx)
  outp' <- if n < 11
             then unEvalM (tn_trans node inp)
             else pprPanic "TODO: think hard about abortion" (ppr (tn_id node) $$ ppr _approx)
  -- re-read, because there might have been a self-loop updating the input
  approx@(TransApprox n inp' outp) <- readTransApprox node_prio
  let !approx' = TransApprox (n+1) inp' outp'
  -- but the stored output shouldn't have changed yet
  massertPpr (_outp == outp) (text "output changed" $$ ppr (tn_id node) $$ ppr _approx $$ ppr approx $$ ppr approx')
  let !changed = outp /= outp'
  -- Unfortunately, the following is not True due to the fact that we split off lazy FVs:
  -- massertPpr (stripBoxityDmdType (outp `lubDmdType` outp') == stripBoxityDmdType outp') (text "non-monotone" $$ ppr id $$ ppr _approx $$ ppr approx $$ ppr approx' $$ ppr (outp `lubDmdType` outp'))
  writeTransApprox node_prio approx'

  -- when changed $ do
  --   pprTraceM "diff of fvs:" $ ppr $ diffUFM (dt_env outp) (dt_env outp')

  let _pp_chg = (if changed then text "CHANGED" else text "no change") <+> ppr (n+1)
  -- pprTraceM "evalNode }" (ppr _node_id <+> parens _pp_chg <> colon <+> ppr outp' <+> if changed then ppr (referrers _fw node_prio) else empty)

  when changed $
    enqueueUnstableReferrers node_prio

  return approx'
