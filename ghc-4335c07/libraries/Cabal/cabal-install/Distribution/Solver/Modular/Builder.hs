{-# LANGUAGE ScopedTypeVariables #-}
module Distribution.Solver.Modular.Builder (
    buildTree
  , splits -- for testing
  ) where

-- Building the search tree.
--
-- In this phase, we build a search tree that is too large, i.e, it contains
-- invalid solutions. We keep track of the open goals at each point. We
-- nondeterministically pick an open goal (via a goal choice node), create
-- subtrees according to the index and the available solutions, and extend the
-- set of open goals by superficially looking at the dependencies recorded in
-- the index.
--
-- For each goal, we keep track of all the *reasons* why it is being
-- introduced. These are for debugging and error messages, mainly. A little bit
-- of care has to be taken due to the way we treat flags. If a package has
-- flag-guarded dependencies, we cannot introduce them immediately. Instead, we
-- store the entire dependency.

import Data.List as L
import Data.Map as M
import Prelude hiding (sequence, mapM)

import Distribution.Solver.Modular.Dependency
import Distribution.Solver.Modular.Flag
import Distribution.Solver.Modular.Index
import Distribution.Solver.Modular.Package
import qualified Distribution.Solver.Modular.PSQ as P
import Distribution.Solver.Modular.Tree
import qualified Distribution.Solver.Modular.WeightedPSQ as W

import Distribution.Solver.Types.PackagePath
import Distribution.Solver.Types.Settings

-- | All state needed to build and link the search tree. It has a type variable
-- because the linking phase doesn't need to know about the state used to build
-- the tree.
data Linker a = Linker {
  buildState   :: a,
  linkingState :: LinkingState
}

-- | The state needed to build the search tree without creating any linked nodes.
data BuildState = BS {
  index :: Index,                   -- ^ information about packages and their dependencies
  rdeps :: RevDepMap,               -- ^ set of all package goals, completed and open, with reverse dependencies
  open  :: [OpenGoal],              -- ^ set of still open goals (flag and package goals)
  next  :: BuildType,               -- ^ kind of node to generate next
  qualifyOptions :: QualifyOptions  -- ^ qualification options
}

-- | Map of available linking targets.
type LinkingState = Map (PN, I) [PackagePath]

-- | Extend the set of open goals with the new goals listed.
--
-- We also adjust the map of overall goals, and keep track of the
-- reverse dependencies of each of the goals.
extendOpen :: QPN -> [FlaggedDep QPN] -> BuildState -> BuildState
extendOpen qpn' gs s@(BS { rdeps = gs', open = o' }) = go gs' o' gs
  where
    go :: RevDepMap -> [OpenGoal] -> [FlaggedDep QPN] -> BuildState
    go g o []                                             = s { rdeps = g, open = o }
    go g o ((Flagged fn@(FN qpn _) fInfo t f)  : ngs) =
        go g (FlagGoal fn fInfo t f (flagGR qpn) : o) ngs
      -- Note: for 'Flagged' goals, we always insert, so later additions win.
      -- This is important, because in general, if a goal is inserted twice,
      -- the later addition will have better dependency information.
    go g o ((Stanza sn@(SN qpn _) t)           : ngs) =
        go g (StanzaGoal sn t (flagGR qpn) : o) ngs
    go g o ((Simple (LDep dr (Dep _ qpn _)) c) : ngs)
      | qpn == qpn'       = go                            g               o  ngs
          -- we ignore self-dependencies at this point; TODO: more care may be needed
      | qpn `M.member` g  = go (M.adjust (addIfAbsent (c, qpn')) qpn g)   o  ngs
      | otherwise         = go (M.insert qpn [(c, qpn')]  g) (PkgGoal qpn (DependencyGoal dr) : o) ngs
          -- code above is correct; insert/adjust have different arg order
    go g o ((Simple (LDep _dr (Ext _ext )) _)  : ngs) = go g o ngs
    go g o ((Simple (LDep _dr (Lang _lang))_)  : ngs) = go g o ngs
    go g o ((Simple (LDep _dr (Pkg _pn _vr))_) : ngs) = go g o ngs

    addIfAbsent :: Eq a => a -> [a] -> [a]
    addIfAbsent x xs = if x `elem` xs then xs else x : xs

    -- GoalReason for a flag or stanza. Each flag/stanza is introduced only by
    -- its containing package.
    flagGR :: qpn -> GoalReason qpn
    flagGR qpn = DependencyGoal (DependencyReason qpn [] [])

-- | Given the current scope, qualify all the package names in the given set of
-- dependencies and then extend the set of open goals accordingly.
scopedExtendOpen :: QPN -> FlaggedDeps PN -> FlagInfo ->
                    BuildState -> BuildState
scopedExtendOpen qpn fdeps fdefs s = extendOpen qpn gs s
  where
    -- Qualify all package names
    qfdeps = qualifyDeps (qualifyOptions s) qpn fdeps
    -- Introduce all package flags
    qfdefs = L.map (\ (fn, b) -> Flagged (FN qpn fn) b [] []) $ M.toList fdefs
    -- Combine new package and flag goals
    gs     = qfdefs ++ qfdeps
    -- NOTE:
    --
    -- In the expression @qfdefs ++ qfdeps@ above, flags occur potentially
    -- multiple times, both via the flag declaration and via dependencies.

-- | Datatype that encodes what to build next
data BuildType =
    Goals              -- ^ build a goal choice node
  | OneGoal OpenGoal   -- ^ build a node for this goal
  | Instance QPN PInfo -- ^ build a tree for a concrete instance

build :: Linker BuildState -> Tree () QGoalReason
build = ana go
  where
    go :: Linker BuildState -> TreeF () QGoalReason (Linker BuildState)
    go s = addLinking (linkingState s) $ addChildren (buildState s)

addChildren :: BuildState -> TreeF () QGoalReason BuildState

-- If we have a choice between many goals, we just record the choice in
-- the tree. We select each open goal in turn, and before we descend, remove
-- it from the queue of open goals.
addChildren bs@(BS { rdeps = rdm, open = gs, next = Goals })
  | L.null gs = DoneF rdm ()
  | otherwise = GoalChoiceF rdm $ P.fromList
                                $ L.map (\ (g, gs') -> (close g, bs { next = OneGoal g, open = gs' }))
                                $ splits gs

-- If we have already picked a goal, then the choice depends on the kind
-- of goal.
--
-- For a package, we look up the instances available in the global info,
-- and then handle each instance in turn.
addChildren bs@(BS { rdeps = rdm, index = idx, next = OneGoal (PkgGoal qpn@(Q _ pn) gr) }) =
  -- If the package does not exist in the index, we construct an emty PChoiceF node for it
  -- After all, we have no choices here. Alternatively, we could immediately construct
  -- a Fail node here, but that would complicate the construction of conflict sets.
  -- We will probably want to give this case special treatment when generating error
  -- messages though.
  case M.lookup pn idx of
    Nothing  -> PChoiceF qpn rdm gr (W.fromList [])
    Just pis -> PChoiceF qpn rdm gr (W.fromList (L.map (\ (i, info) ->
                                                       ([], POption i Nothing, bs { next = Instance qpn info }))
                                                     (M.toList pis)))
      -- TODO: data structure conversion is rather ugly here

-- For a flag, we create only two subtrees, and we create them in the order
-- that is indicated by the flag default.
addChildren bs@(BS { rdeps = rdm, next = OneGoal (FlagGoal qfn@(FN qpn _) (FInfo b m w) t f gr) }) =
  FChoiceF qfn rdm gr weak m b (W.fromList
    [([if b then 0 else 1], True,  (extendOpen qpn t bs) { next = Goals }),
     ([if b then 1 else 0], False, (extendOpen qpn f bs) { next = Goals })])
  where
    trivial = L.null t && L.null f
    weak = WeakOrTrivial $ unWeakOrTrivial w || trivial

-- For a stanza, we also create only two subtrees. The order is initially
-- False, True. This can be changed later by constraints (force enabling
-- the stanza by replacing the False branch with failure) or preferences
-- (try enabling the stanza if possible by moving the True branch first).

addChildren bs@(BS { rdeps = rdm, next = OneGoal (StanzaGoal qsn@(SN qpn _) t gr) }) =
  SChoiceF qsn rdm gr trivial (W.fromList
    [([0], False,                                                                  bs  { next = Goals }),
     ([1], True,  (extendOpen qpn t bs) { next = Goals })])
  where
    trivial = WeakOrTrivial (L.null t)

-- For a particular instance, we change the state: we update the scope,
-- and furthermore we update the set of goals.
--
-- TODO: We could inline this above.
addChildren bs@(BS { next = Instance qpn (PInfo fdeps fdefs _) }) =
  addChildren ((scopedExtendOpen qpn fdeps fdefs bs)
         { next = Goals })

{-------------------------------------------------------------------------------
  Add linking
-------------------------------------------------------------------------------}

-- | Introduce link nodes into the tree
--
-- Linking is a phase that adapts package choice nodes and adds the option to
-- link wherever appropriate: Package goals are called "related" if they are for
-- the same instance of the same package (but have different prefixes). A link
-- option is available in a package choice node whenever we can choose an
-- instance that has already been chosen for a related goal at a higher position
-- in the tree. We only create link options for related goals that are not
-- themselves linked, because the choice to link to a linked goal is the same as
-- the choice to link to the target of that goal's linking.
--
-- The code here proceeds by maintaining a finite map recording choices that
-- have been made at higher positions in the tree. For each pair of package name
-- and instance, it stores the prefixes at which we have made a choice for this
-- package instance. Whenever we make an unlinked choice, we extend the map.
-- Whenever we find a choice, we look into the map in order to find out what
-- link options we have to add.
--
-- A separate tree traversal would be simpler. However, 'addLinking' creates
-- linked nodes from existing unlinked nodes, which leads to sharing between the
-- nodes. If we copied the nodes when they were full trees of type
-- 'Tree () QGoalReason', then the sharing would cause a space leak during
-- exploration of the tree. Instead, we only copy the 'BuildState', which is
-- relatively small, while the tree is being constructed. See
-- https://github.com/haskell/cabal/issues/2899
addLinking :: LinkingState -> TreeF () c a -> TreeF () c (Linker a)
-- The only nodes of interest are package nodes
addLinking ls (PChoiceF qpn@(Q pp pn) rdm gr cs) =
  let linkedCs = fmap (\bs -> Linker bs ls) $
                 W.fromList $ concatMap (linkChoices ls qpn) (W.toList cs)
      unlinkedCs = W.mapWithKey goP cs
      allCs = unlinkedCs `W.union` linkedCs

      -- Recurse underneath package choices. Here we just need to make sure
      -- that we record the package choice so that it is available below
      goP :: POption -> a -> Linker a
      goP (POption i Nothing) bs = Linker bs $ M.insertWith (++) (pn, i) [pp] ls
      goP _                   _  = alreadyLinked
  in PChoiceF qpn rdm gr allCs
addLinking ls t = fmap (\bs -> Linker bs ls) t

linkChoices :: forall a w . LinkingState
            -> QPN
            -> (w, POption, a)
            -> [(w, POption, a)]
linkChoices related (Q _pp pn) (weight, POption i Nothing, subtree) =
    L.map aux (M.findWithDefault [] (pn, i) related)
  where
    aux :: PackagePath -> (w, POption, a)
    aux pp = (weight, POption i (Just pp), subtree)
linkChoices _ _ (_, POption _ (Just _), _) =
    alreadyLinked

alreadyLinked :: a
alreadyLinked = error "addLinking called on tree that already contains linked nodes"

-------------------------------------------------------------------------------

-- | Interface to the tree builder. Just takes an index and a list of package names,
-- and computes the initial state and then the tree from there.
buildTree :: Index -> IndependentGoals -> [PN] -> Tree () QGoalReason
buildTree idx (IndependentGoals ind) igs =
    build Linker {
        buildState = BS {
            index = idx
          , rdeps = M.fromList (L.map (\ qpn -> (qpn, []))              qpns)
          , open  = L.map topLevelGoal qpns
          , next  = Goals
          , qualifyOptions = defaultQualifyOptions idx
          }
      , linkingState = M.empty
      }
  where
    topLevelGoal qpn = PkgGoal qpn UserGoal

    qpns | ind       = L.map makeIndependent igs
         | otherwise = L.map (Q (PackagePath DefaultNamespace QualToplevel)) igs

{-------------------------------------------------------------------------------
  Goals
-------------------------------------------------------------------------------}

-- | Information needed about a dependency before it is converted into a Goal.
data OpenGoal =
    FlagGoal   (FN QPN) FInfo (FlaggedDeps QPN) (FlaggedDeps QPN) QGoalReason
  | StanzaGoal (SN QPN)       (FlaggedDeps QPN)                   QGoalReason
  | PkgGoal    QPN                                                QGoalReason

-- | Closes a goal, i.e., removes all the extraneous information that we
-- need only during the build phase.
close :: OpenGoal -> Goal QPN
close (FlagGoal   qfn _ _ _ gr) = Goal (F qfn) gr
close (StanzaGoal qsn _     gr) = Goal (S qsn) gr
close (PkgGoal    qpn       gr) = Goal (P qpn) gr

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- | Pairs each element of a list with the list resulting from removal of that
-- element from the original list.
splits :: [a] -> [(a, [a])]
splits = go id
  where
    go :: ([a] -> [a]) -> [a] -> [(a, [a])]
    go _ [] = []
    go f (x : xs) = (x, f xs) : go (f . (x :)) xs
