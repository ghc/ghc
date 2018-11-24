{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- | Hides away distracting bookkeeping while lambda lifting into a 'LiftM'
-- monad.
module StgLiftLams.LiftM (
    decomposeStgBinding, mkStgBinding,
    Env (..),
    -- * #floats# Handling floats
    -- $floats
    FloatLang (..), collectFloats, -- Exported just for the docs
    -- * Transformation monad
    LiftM, runLiftM, withCaffyness,
    -- ** Adding bindings
    startBindingGroup, endBindingGroup, addTopStringLit, addLiftedBinding,
    -- ** Substitution and binders
    withSubstBndr, withSubstBndrs, withLiftedBndr, withLiftedBndrs,
    -- ** Occurrences
    substOcc, isLifted, formerFreeVars, liftedIdsExpander
  ) where

#include "HsVersions.h"

import GhcPrelude

import BasicTypes
import CostCentre ( isCurrentCCS, dontCareCCS )
import DynFlags
import FastString
import Id
import IdInfo
import Name
import Outputable
import OrdList
import StgSubst
import StgSyn
import Type
import UniqSupply
import Util
import VarEnv
import VarSet

import Control.Arrow ( second )
import Control.Monad.Trans.Class
import Control.Monad.Trans.RWS.Strict ( RWST, runRWST )
import qualified Control.Monad.Trans.RWS.Strict as RWS
import Control.Monad.Trans.Cont ( ContT (..) )
import Data.ByteString ( ByteString )

-- | @uncurry 'mkStgBinding' . 'decomposeStgBinding' = id@
decomposeStgBinding :: GenStgBinding pass -> (RecFlag, [(BinderP pass, GenStgRhs pass)])
decomposeStgBinding (StgRec pairs) = (Recursive, pairs)
decomposeStgBinding (StgNonRec bndr rhs) = (NonRecursive, [(bndr, rhs)])

mkStgBinding :: RecFlag -> [(BinderP pass, GenStgRhs pass)] -> GenStgBinding pass
mkStgBinding Recursive = StgRec
mkStgBinding NonRecursive = uncurry StgNonRec . head

-- | Environment threaded around in a scoped, @Reader@-like fashion.
data Env
  = Env
  { e_dflags     :: !DynFlags
  -- ^ Read-only.
  , e_subst      :: !Subst
  -- ^ We need to track the renamings of local 'InId's to their lifted 'OutId',
  -- because shadowing might make a closure's free variables unavailable at its
  -- call sites. Consider:
  -- @
  --    let f y = x + y in let x = 4 in f x
  -- @
  -- Here, @f@ can't be lifted to top-level, because its free variable @x@ isn't
  -- available at its call site.
  , e_expansions :: !(IdEnv DIdSet)
  -- ^ Lifted 'Id's don't occur as free variables in any closure anymore, because
  -- they are bound at the top-level. Every occurrence must supply the formerly
  -- free variables of the lifted 'Id', so they in turn become free variables of
  -- the call sites. This environment tracks this expansion from lifted 'Id's to
  -- their free variables.
  --
  -- 'InId's to 'OutId's.
  --
  -- Invariant: 'Id's not present in this map won't be substituted.
  , e_in_caffy_context :: !Bool
  -- ^ Are we currently analysing within a caffy context (e.g. the containing
  -- top-level binder's 'idCafInfo' is 'MayHaveCafRefs')? If not, we can safely
  -- assume that functions we lift out aren't caffy either.
  }

emptyEnv :: DynFlags -> Env
emptyEnv dflags = Env dflags emptySubst emptyVarEnv False


-- Note [Handling floats]
-- ~~~~~~~~~~~~~~~~~~~~~~
-- $floats
-- Consider the following expression:
--
-- @
--     f x =
--       let g y = ... f y ...
--       in g x
-- @
--
-- What happens when we want to lift @g@? Normally, we'd put the lifted @l_g@
-- binding above the binding for @f@:
--
-- @
--     g f y = ... f y ...
--     f x = g f x
-- @
--
-- But this very unnecessarily turns a known call to @f@ into an unknown one, in
-- addition to complicating matters for the analysis.
-- Instead, we'd really like to put both functions in the same recursive group,
-- thereby preserving the known call:
--
-- @
--     Rec {
--       g y = ... f y ...
--       f x = g x
--     }
-- @
--
-- But we don't want this to happen for just /any/ binding. That would create
-- possibly huge recursive groups in the process, calling for an occurrence
-- analyser on STG.
-- So, we need to track when we lift a binding out of a recursive RHS and add
-- the binding to the same recursive group as the enclosing recursive binding
-- (which must have either already been at the top-level or decided to be
-- lifted itself in order to preserve the known call).
--
-- This is done by expressing this kind of nesting structure as a 'Writer' over
-- @['FloatLang']@ and flattening this expression in 'runLiftM' by a call to
-- 'collectFloats'.
-- API-wise, the analysis will not need to know about the whole 'FloatLang'
-- business and will just manipulate it indirectly through actions in 'LiftM'.

-- | We need to detect when we are lifting something out of the RHS of a
-- recursive binding (c.f. "StgLiftLams.LiftM#floats"), in which case that
-- binding needs to be added to the same top-level recursive group. This
-- requires we detect a certain nesting structure, which is encoded by
-- 'StartBindingGroup' and 'EndBindingGroup'.
--
-- Although 'collectFloats' will only ever care if the current binding to be
-- lifted (through 'LiftedBinding') will occur inside such a binding group or
-- not, e.g. doesn't care about the nesting level as long as its greater than 0.
data FloatLang
  = StartBindingGroup
  | EndBindingGroup
  | PlainTopBinding OutStgTopBinding
  | LiftedBinding OutStgBinding

instance Outputable FloatLang where
  ppr StartBindingGroup = char '('
  ppr EndBindingGroup = char ')'
  ppr (PlainTopBinding StgTopStringLit{}) = text "<str>"
  ppr (PlainTopBinding (StgTopLifted b)) = ppr (LiftedBinding b)
  ppr (LiftedBinding bind) = (if isRec rec then char 'r' else char 'n') <+> ppr (map fst pairs)
    where
      (rec, pairs) = decomposeStgBinding bind

-- | Flattens an expression in @['FloatLang']@ into an STG program, see #floats.
-- Important pre-conditions: The nesting of opening 'StartBindinGroup's and
-- closing 'EndBindinGroup's is balanced. Also, it is crucial that every binding
-- group has at least one recursive binding inside. Otherwise there's no point
-- in announcing the binding group in the first place and an @ASSERT@ will
-- trigger.
collectFloats :: [FloatLang] -> [OutStgTopBinding]
collectFloats = go (0 :: Int) []
  where
    go 0 [] [] = []
    go _ _ [] = pprPanic "collectFloats" (text "unterminated group")
    go n binds (f:rest) = case f of
      StartBindingGroup -> go (n+1) binds rest
      EndBindingGroup
        | n == 0 -> pprPanic "collectFloats" (text "no group to end")
        | n == 1 -> StgTopLifted (merge_binds binds) : go 0 [] rest
        | otherwise -> go (n-1) binds rest
      PlainTopBinding top_bind
        | n == 0 -> top_bind : go n binds rest
        | otherwise -> pprPanic "collectFloats" (text "plain top binding inside group")
      LiftedBinding bind
        | n == 0 -> StgTopLifted (rm_cccs bind) : go n binds rest
        | otherwise -> go n (bind:binds) rest

    map_rhss f = uncurry mkStgBinding . second (map (second f)) . decomposeStgBinding
    rm_cccs = map_rhss removeRhsCCCS
    merge_binds binds = ASSERT( any is_rec binds )
                        StgRec (concatMap (snd . decomposeStgBinding . rm_cccs) binds)
    is_rec StgRec{} = True
    is_rec _ = False

-- | Omitting this makes for strange closure allocation schemes that crash the
-- GC.
removeRhsCCCS :: GenStgRhs pass -> GenStgRhs pass
removeRhsCCCS (StgRhsClosure ext ccs upd bndrs body)
  | isCurrentCCS ccs
  = StgRhsClosure ext dontCareCCS upd bndrs body
removeRhsCCCS (StgRhsCon ccs con args)
  | isCurrentCCS ccs
  = StgRhsCon dontCareCCS con args
removeRhsCCCS rhs = rhs

-- | The analysis monad consists of the following 'RWST' components:
--
--     * 'Env': Reader-like context. Contains a substitution, info about how
--       how lifted identifiers are to be expanded into applications and details
--       such as 'DynFlags' and a flag helping with determining if a lifted
--       binding is caffy.
--
--     * @'OrdList' 'FloatLang'@: Writer output for the resulting STG program.
--
--     * No pure state component
--
--     * But wrapping around 'UniqSM' for generating fresh lifted binders.
--       (The @uniqAway@ approach could give the same name to two different
--       lifted binders, so this is necessary.)
newtype LiftM a
  = LiftM { unwrapLiftM :: RWST Env (OrdList FloatLang) () UniqSM a }
  deriving (Functor, Applicative, Monad)

instance HasDynFlags LiftM where
  getDynFlags = LiftM (RWS.asks e_dflags)

instance MonadUnique LiftM where
  getUniqueSupplyM = LiftM (lift getUniqueSupplyM)
  getUniqueM = LiftM (lift getUniqueM)
  getUniquesM = LiftM (lift getUniquesM)

runLiftM :: DynFlags -> UniqSupply -> LiftM () -> [OutStgTopBinding]
runLiftM dflags us (LiftM m) = collectFloats (fromOL floats)
  where
    (_, _, floats) = initUs_ us (runRWST m (emptyEnv dflags) ())

-- | Assumes a given caffyness for the execution of the passed action, which
-- influences the 'cafInfo' of lifted bindings.
withCaffyness :: Bool -> LiftM a -> LiftM a
withCaffyness caffy action
  = LiftM (RWS.local (\e -> e { e_in_caffy_context = caffy }) (unwrapLiftM action))

-- | Writes a plain 'StgTopStringLit' to the output.
addTopStringLit :: OutId -> ByteString -> LiftM ()
addTopStringLit id = LiftM . RWS.tell . unitOL . PlainTopBinding . StgTopStringLit id

-- | Starts a recursive binding group. See #floats# and 'collectFloats'.
startBindingGroup :: LiftM ()
startBindingGroup = LiftM $ RWS.tell $ unitOL $ StartBindingGroup

-- | Ends a recursive binding group. See #floats# and 'collectFloats'.
endBindingGroup :: LiftM ()
endBindingGroup = LiftM $ RWS.tell $ unitOL $ EndBindingGroup

-- | Lifts a binding to top-level. Depending on whether it's declared inside
-- a recursive RHS (see #floats# and 'collectFloats'), this might be added to
-- an existing recursive top-level binding group.
addLiftedBinding :: OutStgBinding -> LiftM ()
addLiftedBinding = LiftM . RWS.tell . unitOL . LiftedBinding

-- | Takes a binder and a continuation which is called with the substituted
-- binder. The continuation will be evaluated in a 'LiftM' context in which that
-- binder is deemed in scope. Think of it as a 'RWS.local' computation: After
-- the continuation finishes, the new binding won't be in scope anymore.
withSubstBndr :: Id -> (Id -> LiftM a) -> LiftM a
withSubstBndr bndr inner = LiftM $ do
  subst <- RWS.asks e_subst
  let (bndr', subst') = substBndr bndr subst
  RWS.local (\e -> e { e_subst = subst' }) (unwrapLiftM (inner bndr'))

-- | See 'withSubstBndr'.
withSubstBndrs :: Traversable f => f Id -> (f Id -> LiftM a) -> LiftM a
withSubstBndrs = runContT . traverse (ContT . withSubstBndr)

-- | Similarly to 'withSubstBndr', this function takes a set of variables to
-- abstract over, the binder to lift (and generate a fresh, substituted name
-- for) and a continuation in which that fresh, lifted binder is in scope.
--
-- It takes care of all the details involved with copying and adjusting the
-- binder, fresh name generation and caffyness.
withLiftedBndr :: DIdSet -> Id -> (Id -> LiftM a) -> LiftM a
withLiftedBndr abs_ids bndr inner = do
  uniq <- getUniqueM
  let str = "$l" ++ occNameString (getOccName bndr)
  let ty = mkLamTypes (dVarSetElems abs_ids) (idType bndr)
  -- When the enclosing top-level binding is not caffy, then the lifted
  -- binding will not be caffy either. If we don't recognize this, non-caffy
  -- things call caffy things and then codegen screws up.
  in_caffy_ctxt <- LiftM (RWS.asks e_in_caffy_context)
  let caf_info = if in_caffy_ctxt then MayHaveCafRefs else NoCafRefs
  let bndr'
        -- See Note [transferPolyIdInfo] in Id.hs. We need to do this at least
        -- for arity information.
        = transferPolyIdInfo bndr (dVarSetElems abs_ids)
        -- Otherwise we confuse code gen if bndr was not caffy: the new bndr is
        -- assumed to be caffy and will need an SRT. Transitive call sites might
        -- not be caffy themselves and subsequently will miss a static link
        -- field in their closure. Chaos ensues.
        . flip setIdCafInfo caf_info
        . mkSysLocalOrCoVar (mkFastString str) uniq
        $ ty
  LiftM $ RWS.local
    (\e -> e
      { e_subst = extendSubst bndr bndr' $ extendInScope bndr' $ e_subst e
      , e_expansions = extendVarEnv (e_expansions e) bndr abs_ids
      })
    (unwrapLiftM (inner bndr'))

-- | See 'withLiftedBndr'.
withLiftedBndrs :: Traversable f => DIdSet -> f Id -> (f Id -> LiftM a) -> LiftM a
withLiftedBndrs abs_ids = runContT . traverse (ContT . withLiftedBndr abs_ids)

-- | Substitutes a binder /occurrence/, which was brought in scope earlier by
-- 'withSubstBndr'\/'withLiftedBndr'.
substOcc :: Id -> LiftM Id
substOcc id = LiftM (RWS.asks (lookupIdSubst id . e_subst))

-- | Whether the given binding was decided to be lambda lifted.
isLifted :: InId -> LiftM Bool
isLifted bndr = LiftM (RWS.asks (elemVarEnv bndr . e_expansions))

-- | Returns an empty list for a binding that was not lifted and the list of all
-- local variables the binding abstracts over (so, exactly the additional
-- arguments at adjusted call sites) otherwise.
formerFreeVars :: InId -> LiftM [OutId]
formerFreeVars f = LiftM $ do
  expansions <- RWS.asks e_expansions
  pure $ case lookupVarEnv expansions f of
    Nothing -> []
    Just fvs -> dVarSetElems fvs

-- | Creates an /expander function/ for the current set of lifted binders.
-- This expander function will replace any 'InId' by their corresponding 'OutId'
-- and, in addition, will expand any lifted binders by the former free variables
-- it abstracts over.
liftedIdsExpander :: LiftM (DIdSet -> DIdSet)
liftedIdsExpander = LiftM $ do
  expansions <- RWS.asks e_expansions
  subst <- RWS.asks e_subst
  -- We use @noWarnLookupIdSubst@ here in order to suppress "not in scope"
  -- warnings generated by 'lookupIdSubst' due to local bindings within RHS.
  -- These are not in the InScopeSet of @subst@ and extending the InScopeSet in
  -- @goodToLift@/@closureGrowth@ before passing it on to @expander@ is too much
  -- trouble.
  let go set fv = case lookupVarEnv expansions fv of
        Nothing -> extendDVarSet set (noWarnLookupIdSubst fv subst) -- Not lifted
        Just fvs' -> unionDVarSet set fvs'
  let expander fvs = foldl' go emptyDVarSet (dVarSetElems fvs)
  pure expander
