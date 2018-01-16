{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Distribution.Solver.Modular.Linking (
    validateLinking
  ) where

import Prelude ()
import Distribution.Solver.Compat.Prelude hiding (get,put)

import Control.Exception (assert)
import Control.Monad.Reader
import Control.Monad.State
import Data.Function (on)
import Data.Map ((!))
import Data.Set (Set)
import qualified Data.Map         as M
import qualified Data.Set         as S
import qualified Data.Traversable as T

import Distribution.Client.Utils.Assertion
import Distribution.Solver.Modular.Assignment
import Distribution.Solver.Modular.Dependency
import Distribution.Solver.Modular.Flag
import Distribution.Solver.Modular.Index
import Distribution.Solver.Modular.Package
import Distribution.Solver.Modular.Tree
import qualified Distribution.Solver.Modular.ConflictSet as CS
import qualified Distribution.Solver.Modular.WeightedPSQ as W

import Distribution.Solver.Types.OptionalStanza
import Distribution.Solver.Types.PackagePath
import Distribution.Types.GenericPackageDescription (unFlagName)

{-------------------------------------------------------------------------------
  Validation

  Validation of links is a separate pass that's performed after normal
  validation. Validation of links checks that if the tree indicates that a
  package is linked, then everything underneath that choice really matches the
  package we have linked to.

  This is interesting because it isn't unidirectional. Consider that we've
  chosen a.foo to be version 1 and later decide that b.foo should link to a.foo.
  Now foo depends on bar. Because a.foo and b.foo are linked, it's required that
  a.bar and b.bar are also linked. However, it's not required that we actually
  choose a.bar before b.bar. Goal choice order is relatively free. It's possible
  that we choose a.bar first, but also possible that we choose b.bar first. In
  both cases, we have to recognize that we have freedom of choice for the first
  of the two, but no freedom of choice for the second.

  This is what LinkGroups are all about. Using LinkGroup, we can record (in the
  situation above) that a.bar and b.bar need to be linked even if we haven't
  chosen either of them yet.
-------------------------------------------------------------------------------}

data ValidateState = VS {
      vsIndex    :: Index
    , vsLinks    :: Map QPN LinkGroup
    , vsFlags    :: FAssignment
    , vsStanzas  :: SAssignment
    , vsQualifyOptions :: QualifyOptions

    -- Saved qualified dependencies. Every time 'validateLinking' makes a
    -- package choice, it qualifies the package's dependencies and saves them in
    -- this map. Then the qualified dependencies are available for subsequent
    -- flag and stanza choices for the same package.
    , vsSaved    :: Map QPN (FlaggedDeps QPN)
    }

type Validate = Reader ValidateState

-- | Validate linked packages
--
-- Verify that linked packages have
--
-- * Linked dependencies,
-- * Equal flag assignments
-- * Equal stanza assignments
validateLinking :: Index -> Tree d c -> Tree d c
validateLinking index = (`runReader` initVS) . cata go
  where
    go :: TreeF d c (Validate (Tree d c)) -> Validate (Tree d c)

    go (PChoiceF qpn rdm gr       cs) =
      PChoice qpn rdm gr       <$> T.sequence (W.mapWithKey (goP qpn) cs)
    go (FChoiceF qfn rdm gr t m d cs) =
      FChoice qfn rdm gr t m d <$> T.sequence (W.mapWithKey (goF qfn) cs)
    go (SChoiceF qsn rdm gr t     cs) =
      SChoice qsn rdm gr t     <$> T.sequence (W.mapWithKey (goS qsn) cs)

    -- For the other nodes we just recurse
    go (GoalChoiceF rdm           cs) = GoalChoice rdm <$> T.sequence cs
    go (DoneF revDepMap s)            = return $ Done revDepMap s
    go (FailF conflictSet failReason) = return $ Fail conflictSet failReason

    -- Package choices
    goP :: QPN -> POption -> Validate (Tree d c) -> Validate (Tree d c)
    goP qpn@(Q _pp pn) opt@(POption i _) r = do
      vs <- ask
      let PInfo deps _ _ = vsIndex vs ! pn ! i
          qdeps          = qualifyDeps (vsQualifyOptions vs) qpn deps
          newSaved       = M.insert qpn qdeps (vsSaved vs)
      case execUpdateState (pickPOption qpn opt qdeps) vs of
        Left  (cs, err) -> return $ Fail cs (DependenciesNotLinked err)
        Right vs'       -> local (const vs' { vsSaved = newSaved }) r

    -- Flag choices
    goF :: QFN -> Bool -> Validate (Tree d c) -> Validate (Tree d c)
    goF qfn b r = do
      vs <- ask
      case execUpdateState (pickFlag qfn b) vs of
        Left  (cs, err) -> return $ Fail cs (DependenciesNotLinked err)
        Right vs'       -> local (const vs') r

    -- Stanza choices (much the same as flag choices)
    goS :: QSN -> Bool -> Validate (Tree d c) -> Validate (Tree d c)
    goS qsn b r = do
      vs <- ask
      case execUpdateState (pickStanza qsn b) vs of
        Left  (cs, err) -> return $ Fail cs (DependenciesNotLinked err)
        Right vs'       -> local (const vs') r

    initVS :: ValidateState
    initVS = VS {
        vsIndex   = index
      , vsLinks   = M.empty
      , vsFlags   = M.empty
      , vsStanzas = M.empty
      , vsQualifyOptions = defaultQualifyOptions index
      , vsSaved   = M.empty
      }

{-------------------------------------------------------------------------------
  Updating the validation state
-------------------------------------------------------------------------------}

type Conflict = (ConflictSet, String)

newtype UpdateState a = UpdateState {
    unUpdateState :: StateT ValidateState (Either Conflict) a
  }
  deriving (Functor, Applicative, Monad)

instance MonadState ValidateState UpdateState where
  get    = UpdateState $ get
  put st = UpdateState $ do
             expensiveAssert (lgInvariant $ vsLinks st) $ return ()
             put st

lift' :: Either Conflict a -> UpdateState a
lift' = UpdateState . lift

conflict :: Conflict -> UpdateState a
conflict = lift' . Left

execUpdateState :: UpdateState () -> ValidateState -> Either Conflict ValidateState
execUpdateState = execStateT . unUpdateState

pickPOption :: QPN -> POption -> FlaggedDeps QPN -> UpdateState ()
pickPOption qpn (POption i Nothing)    _deps = pickConcrete qpn i
pickPOption qpn (POption i (Just pp'))  deps = pickLink     qpn i pp' deps

pickConcrete :: QPN -> I -> UpdateState ()
pickConcrete qpn@(Q pp _) i = do
    vs <- get
    case M.lookup qpn (vsLinks vs) of
      -- Package is not yet in a LinkGroup. Create a new singleton link group.
      Nothing -> do
        let lg = lgSingleton qpn (Just $ PI pp i)
        updateLinkGroup lg

      -- Package is already in a link group. Since we are picking a concrete
      -- instance here, it must by definition be the canonical package.
      Just lg ->
        makeCanonical lg qpn i

pickLink :: QPN -> I -> PackagePath -> FlaggedDeps QPN -> UpdateState ()
pickLink qpn@(Q _pp pn) i pp' deps = do
    vs <- get

    -- The package might already be in a link group
    -- (because one of its reverse dependencies is)
    let lgSource = case M.lookup qpn (vsLinks vs) of
                     Nothing -> lgSingleton qpn Nothing
                     Just lg -> lg

    -- Find the link group for the package we are linking to
    --
    -- Since the builder never links to a package without having first picked a
    -- concrete instance for that package, and since we create singleton link
    -- groups for concrete instances, this link group must exist (and must
    -- in fact already have a canonical member).
    let target   = Q pp' pn
        lgTarget = vsLinks vs ! target

    -- Verify here that the member we add is in fact for the same package and
    -- matches the version of the canonical instance. However, violations of
    -- these checks would indicate a bug in the linker, not a true conflict.
    let sanityCheck :: Maybe (PI PackagePath) -> Bool
        sanityCheck Nothing              = False
        sanityCheck (Just (PI _ canonI)) = pn == lgPackage lgTarget && i == canonI
    assert (sanityCheck (lgCanon lgTarget)) $ return ()

    -- Merge the two link groups (updateLinkGroup will propagate the change)
    lgTarget' <- lift' $ lgMerge CS.empty lgSource lgTarget
    updateLinkGroup lgTarget'

    -- Make sure all dependencies are linked as well
    linkDeps target deps

makeCanonical :: LinkGroup -> QPN -> I -> UpdateState ()
makeCanonical lg qpn@(Q pp _) i =
    case lgCanon lg of
      -- There is already a canonical member. Fail.
      Just _ ->
        conflict ( CS.insert (P qpn) (lgConflictSet lg)
                 ,    "cannot make " ++ showQPN qpn
                   ++ " canonical member of " ++ showLinkGroup lg
                 )
      Nothing -> do
        let lg' = lg { lgCanon = Just (PI pp i) }
        updateLinkGroup lg'

-- | Link the dependencies of linked parents.
--
-- When we decide to link one package against another we walk through the
-- package's direct depedencies and make sure that they're all linked to each
-- other by merging their link groups (or creating new singleton link groups if
-- they don't have link groups yet). We do not need to do this recursively,
-- because having the direct dependencies in a link group means that we must
-- have already made or will make sooner or later a link choice for one of these
-- as well, and cover their dependencies at that point.
linkDeps :: QPN -> FlaggedDeps QPN -> UpdateState ()
linkDeps target = \deps -> do
    -- linkDeps is called in two places: when we first link one package to
    -- another, and when we discover more dependencies of an already linked
    -- package after doing some flag assignment. It is therefore important that
    -- flag assignments cannot influence _how_ dependencies are qualified;
    -- fortunately this is a documented property of 'qualifyDeps'.
    rdeps <- requalify deps
    go deps rdeps
  where
    go :: FlaggedDeps QPN -> FlaggedDeps QPN -> UpdateState ()
    go = zipWithM_ go1

    go1 :: FlaggedDep QPN -> FlaggedDep QPN -> UpdateState ()
    go1 dep rdep = case (dep, rdep) of
      (Simple (LDep dr1 (Dep _ qpn _)) _, ~(Simple (LDep dr2 (Dep _ qpn' _)) _)) -> do
        vs <- get
        let lg   = M.findWithDefault (lgSingleton qpn  Nothing) qpn  $ vsLinks vs
            lg'  = M.findWithDefault (lgSingleton qpn' Nothing) qpn' $ vsLinks vs
        lg'' <- lift' $ lgMerge ((CS.union `on` dependencyReasonToCS) dr1 dr2) lg lg'
        updateLinkGroup lg''
      (Flagged fn _ t f, ~(Flagged _ _ t' f')) -> do
        vs <- get
        case M.lookup fn (vsFlags vs) of
          Nothing    -> return () -- flag assignment not yet known
          Just True  -> go t t'
          Just False -> go f f'
      (Stanza sn t, ~(Stanza _ t')) -> do
        vs <- get
        case M.lookup sn (vsStanzas vs) of
          Nothing    -> return () -- stanza assignment not yet known
          Just True  -> go t t'
          Just False -> return () -- stanza not enabled; no new deps
    -- For extensions and language dependencies, there is nothing to do.
    -- No choice is involved, just checking, so there is nothing to link.
    -- The same goes for for pkg-config constraints.
      (Simple (LDep _ (Ext  _))   _, _) -> return ()
      (Simple (LDep _ (Lang _))   _, _) -> return ()
      (Simple (LDep _ (Pkg  _ _)) _, _) -> return ()

    requalify :: FlaggedDeps QPN -> UpdateState (FlaggedDeps QPN)
    requalify deps = do
      vs <- get
      return $ qualifyDeps (vsQualifyOptions vs) target (unqualifyDeps deps)

pickFlag :: QFN -> Bool -> UpdateState ()
pickFlag qfn b = do
    modify $ \vs -> vs { vsFlags = M.insert qfn b (vsFlags vs) }
    verifyFlag qfn
    linkNewDeps (F qfn) b

pickStanza :: QSN -> Bool -> UpdateState ()
pickStanza qsn b = do
    modify $ \vs -> vs { vsStanzas = M.insert qsn b (vsStanzas vs) }
    verifyStanza qsn
    linkNewDeps (S qsn) b

-- | Link dependencies that we discover after making a flag or stanza choice.
--
-- When we make a flag choice for a package, then new dependencies for that
-- package might become available. If the package under consideration is in a
-- non-trivial link group, then these new dependencies have to be linked as
-- well. In linkNewDeps, we compute such new dependencies and make sure they are
-- linked.
linkNewDeps :: Var QPN -> Bool -> UpdateState ()
linkNewDeps var b = do
    vs <- get
    let qpn@(Q pp pn)           = varPN var
        qdeps                   = vsSaved vs ! qpn
        lg                      = vsLinks vs ! qpn
        newDeps                 = findNewDeps vs qdeps
        linkedTo                = S.delete pp (lgMembers lg)
    forM_ (S.toList linkedTo) $ \pp' -> linkDeps (Q pp' pn) newDeps
  where
    findNewDeps :: ValidateState -> FlaggedDeps QPN -> FlaggedDeps QPN
    findNewDeps vs = concatMap (findNewDeps' vs)

    findNewDeps' :: ValidateState -> FlaggedDep QPN -> FlaggedDeps QPN
    findNewDeps' _  (Simple _ _)        = []
    findNewDeps' vs (Flagged qfn _ t f) =
      case (F qfn == var, M.lookup qfn (vsFlags vs)) of
        (True, _)    -> if b then t else f
        (_, Nothing) -> [] -- not yet known
        (_, Just b') -> findNewDeps vs (if b' then t else f)
    findNewDeps' vs (Stanza qsn t) =
      case (S qsn == var, M.lookup qsn (vsStanzas vs)) of
        (True, _)    -> if b then t else []
        (_, Nothing) -> [] -- not yet known
        (_, Just b') -> findNewDeps vs (if b' then t else [])

updateLinkGroup :: LinkGroup -> UpdateState ()
updateLinkGroup lg = do
    verifyLinkGroup lg
    modify $ \vs -> vs {
        vsLinks =           M.fromList (map aux (S.toList (lgMembers lg)))
                  `M.union` vsLinks vs
      }
  where
    aux pp = (Q pp (lgPackage lg), lg)

{-------------------------------------------------------------------------------
  Verification
-------------------------------------------------------------------------------}

verifyLinkGroup :: LinkGroup -> UpdateState ()
verifyLinkGroup lg =
    case lgInstance lg of
      -- No instance picked yet. Nothing to verify
      Nothing ->
        return ()

      -- We picked an instance. Verify flags and stanzas
      -- TODO: The enumeration of OptionalStanza names is very brittle;
      -- if a constructor is added to the datatype we won't notice it here
      Just i -> do
        vs <- get
        let PInfo _deps finfo _ = vsIndex vs ! lgPackage lg ! i
            flags   = M.keys finfo
            stanzas = [TestStanzas, BenchStanzas]
        forM_ flags $ \fn -> do
          let flag = FN (lgPackage lg) fn
          verifyFlag' flag lg
        forM_ stanzas $ \sn -> do
          let stanza = SN (lgPackage lg) sn
          verifyStanza' stanza lg

verifyFlag :: QFN -> UpdateState ()
verifyFlag (FN qpn@(Q _pp pn) fn) = do
    vs <- get
    -- We can only pick a flag after picking an instance; link group must exist
    verifyFlag' (FN pn fn) (vsLinks vs ! qpn)

verifyStanza :: QSN -> UpdateState ()
verifyStanza (SN qpn@(Q _pp pn) sn) = do
    vs <- get
    -- We can only pick a stanza after picking an instance; link group must exist
    verifyStanza' (SN pn sn) (vsLinks vs ! qpn)

-- | Verify that all packages in the link group agree on flag assignments
--
-- For the given flag and the link group, obtain all assignments for the flag
-- that have already been made for link group members, and check that they are
-- equal.
verifyFlag' :: FN PN -> LinkGroup -> UpdateState ()
verifyFlag' (FN pn fn) lg = do
    vs <- get
    let flags = map (\pp' -> FN (Q pp' pn) fn) (S.toList (lgMembers lg))
        vals  = map (`M.lookup` vsFlags vs) flags
    if allEqual (catMaybes vals) -- We ignore not-yet assigned flags
      then return ()
      else conflict ( CS.fromList (map F flags) `CS.union` lgConflictSet lg
                    , "flag \"" ++ unFlagName fn ++ "\" incompatible"
                    )

-- | Verify that all packages in the link group agree on stanza assignments
--
-- For the given stanza and the link group, obtain all assignments for the
-- stanza that have already been made for link group members, and check that
-- they are equal.
--
-- This function closely mirrors 'verifyFlag''.
verifyStanza' :: SN PN -> LinkGroup -> UpdateState ()
verifyStanza' (SN pn sn) lg = do
    vs <- get
    let stanzas = map (\pp' -> SN (Q pp' pn) sn) (S.toList (lgMembers lg))
        vals    = map (`M.lookup` vsStanzas vs) stanzas
    if allEqual (catMaybes vals) -- We ignore not-yet assigned stanzas
      then return ()
      else conflict ( CS.fromList (map S stanzas) `CS.union` lgConflictSet lg
                    , "stanza \"" ++ showStanza sn ++ "\" incompatible"
                    )

{-------------------------------------------------------------------------------
  Link groups
-------------------------------------------------------------------------------}

-- | Set of packages that must be linked together
--
-- A LinkGroup is between several qualified package names. In the validation
-- state, we maintain a map vsLinks from qualified package names to link groups.
-- There is an invariant that for all members of a link group, vsLinks must map
-- to the same link group. The function updateLinkGroup can be used to
-- re-establish this invariant after creating or expanding a LinkGroup.
data LinkGroup = LinkGroup {
      -- | The name of the package of this link group
      lgPackage :: PN

      -- | The canonical member of this link group (the one where we picked
      -- a concrete instance). Once we have picked a canonical member, all
      -- other packages must link to this one.
      --
      -- We may not know this yet (if we are constructing link groups
      -- for dependencies)
    , lgCanon :: Maybe (PI PackagePath)

      -- | The members of the link group
    , lgMembers :: Set PackagePath

      -- | The set of variables that should be added to the conflict set if
      -- something goes wrong with this link set (in addition to the members
      -- of the link group itself)
    , lgBlame :: ConflictSet
    }
    deriving (Show, Eq)

-- | Invariant for the set of link groups: every element in the link group
-- must be pointing to the /same/ link group
lgInvariant :: Map QPN LinkGroup -> Bool
lgInvariant links = all invGroup (M.elems links)
  where
    invGroup :: LinkGroup -> Bool
    invGroup lg = allEqual $ map (`M.lookup` links) members
      where
        members :: [QPN]
        members = map (`Q` lgPackage lg) $ S.toList (lgMembers lg)

-- | Package version of this group
--
-- This is only known once we have picked a canonical element.
lgInstance :: LinkGroup -> Maybe I
lgInstance = fmap (\(PI _ i) -> i) . lgCanon

showLinkGroup :: LinkGroup -> String
showLinkGroup lg =
    "{" ++ intercalate "," (map showMember (S.toList (lgMembers lg))) ++ "}"
  where
    showMember :: PackagePath -> String
    showMember pp = case lgCanon lg of
                      Just (PI pp' _i) | pp == pp' -> "*"
                      _otherwise                   -> ""
                 ++ case lgInstance lg of
                      Nothing -> showQPN (qpn pp)
                      Just i  -> showPI (PI (qpn pp) i)

    qpn :: PackagePath -> QPN
    qpn pp = Q pp (lgPackage lg)

-- | Creates a link group that contains a single member.
lgSingleton :: QPN -> Maybe (PI PackagePath) -> LinkGroup
lgSingleton (Q pp pn) canon = LinkGroup {
      lgPackage = pn
    , lgCanon   = canon
    , lgMembers = S.singleton pp
    , lgBlame   = CS.empty
    }

lgMerge :: ConflictSet -> LinkGroup -> LinkGroup -> Either Conflict LinkGroup
lgMerge blame lg lg' = do
    canon <- pick (lgCanon lg) (lgCanon lg')
    return LinkGroup {
        lgPackage = lgPackage lg
      , lgCanon   = canon
      , lgMembers = lgMembers lg `S.union` lgMembers lg'
      , lgBlame   = CS.unions [blame, lgBlame lg, lgBlame lg']
      }
  where
    pick :: Eq a => Maybe a -> Maybe a -> Either Conflict (Maybe a)
    pick Nothing  Nothing  = Right Nothing
    pick (Just x) Nothing  = Right $ Just x
    pick Nothing  (Just y) = Right $ Just y
    pick (Just x) (Just y) =
      if x == y then Right $ Just x
                else Left ( CS.unions [
                               blame
                             , lgConflictSet lg
                             , lgConflictSet lg'
                             ]
                          ,    "cannot merge " ++ showLinkGroup lg
                            ++ " and " ++ showLinkGroup lg'
                          )

lgConflictSet :: LinkGroup -> ConflictSet
lgConflictSet lg =
               CS.fromList (map aux (S.toList (lgMembers lg)))
    `CS.union` lgBlame lg
  where
    aux pp = P (Q pp (lgPackage lg))

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

allEqual :: Eq a => [a] -> Bool
allEqual []       = True
allEqual [_]      = True
allEqual (x:y:ys) = x == y && allEqual (y:ys)
