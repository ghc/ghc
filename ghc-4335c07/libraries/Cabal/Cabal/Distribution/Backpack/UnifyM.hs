{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | See <https://github.com/ezyang/ghc-proposals/blob/backpack/proposals/0000-backpack.rst>
module Distribution.Backpack.UnifyM (
    -- * Unification monad
    UnifyM,
    runUnifyM,
    failWith,
    addErr,
    failIfErrs,
    tryM,
    addErrContext,
    addErrContextM,
    liftST,

    UnifEnv(..),
    getUnifEnv,

    -- * Modules and unit IDs
    ModuleU,
    ModuleU'(..),
    convertModule,
    convertModuleU,

    UnitIdU,
    UnitIdU'(..),
    convertUnitId,
    convertUnitIdU,

    ModuleSubstU,
    convertModuleSubstU,
    convertModuleSubst,

    ModuleScopeU,
    emptyModuleScopeU,
    convertModuleScopeU,

    ModuleWithSourceU,

    convertInclude,
    convertModuleProvides,
    convertModuleProvidesU,

) where

import Prelude ()
import Distribution.Compat.Prelude hiding (mod)

import Distribution.Backpack.ModuleShape
import Distribution.Backpack.ModuleScope
import Distribution.Backpack.ModSubst
import Distribution.Backpack.FullUnitId
import Distribution.Backpack

import qualified Distribution.Utils.UnionFind as UnionFind
import Distribution.ModuleName
import Distribution.Package
import Distribution.PackageDescription
import Distribution.Text
import Distribution.Types.IncludeRenaming
import Distribution.Types.ComponentInclude
import Distribution.Types.AnnotatedId
import Distribution.Types.ComponentName
import Distribution.Verbosity

import Data.STRef
import Data.Traversable
import Control.Monad.ST
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.Traversable as T
import Text.PrettyPrint

-- TODO: more detailed trace output on high verbosity would probably
-- be appreciated by users debugging unification errors.  Collect
-- some good examples!

data ErrMsg = ErrMsg {
        err_msg :: Doc,
        err_ctx :: [Doc]
    }
type MsgDoc = Doc

renderErrMsg :: ErrMsg -> MsgDoc
renderErrMsg ErrMsg { err_msg = msg, err_ctx = ctx } =
    msg $$ vcat ctx

-- | The unification monad, this monad encapsulates imperative
-- unification.
newtype UnifyM s a = UnifyM { unUnifyM :: UnifEnv s -> ST s (Maybe a) }

-- | Run a computation in the unification monad.
runUnifyM :: Verbosity -> ComponentId -> FullDb -> (forall s. UnifyM s a) -> Either [MsgDoc] a
runUnifyM verbosity self_cid db m
    = runST $ do i    <- newSTRef 0
                 hmap <- newSTRef Map.empty
                 errs <- newSTRef []
                 mb_r <- unUnifyM m UnifEnv {
                            unify_uniq = i,
                            unify_reqs = hmap,
                            unify_self_cid = self_cid,
                            unify_verbosity = verbosity,
                            unify_ctx = [],
                            unify_db = db,
                            unify_errs = errs }
                 final_errs <- readSTRef errs
                 case mb_r of
                    Just x | null final_errs -> return (Right x)
                    _ -> return (Left (map renderErrMsg (reverse final_errs)))
-- NB: GHC 7.6 throws a hissy fit if you pattern match on 'm'.

type ErrCtx s = MsgDoc

-- | The unification environment.
data UnifEnv s = UnifEnv {
        -- | A supply of unique integers to label 'UnitIdU'
        -- cells.  This is used to determine loops in unit
        -- identifiers (which can happen with mutual recursion.)
        unify_uniq :: UnifRef s UnitIdUnique,
        -- | The set of requirements in scope.  When
        -- a provision is brought into scope, we unify with
        -- the requirement at the same module name to fill it.
        -- This mapping grows monotonically.
        unify_reqs :: UnifRef s (Map ModuleName (ModuleU s)),
        -- | Component id of the unit we're linking.  We use this
        -- to detect if we fill a requirement with a local module,
        -- which in principle should be OK but is not currently
        -- supported by GHC.
        unify_self_cid :: ComponentId,
        -- | How verbose the error message should be
        unify_verbosity :: Verbosity,
        -- | The error reporting context
        unify_ctx :: [ErrCtx s],
        -- | The package index for expanding unit identifiers
        unify_db :: FullDb,
        -- | Accumulated errors
        unify_errs :: UnifRef s [ErrMsg]
    }

instance Functor (UnifyM s) where
    fmap f (UnifyM m) = UnifyM (fmap (fmap (fmap f)) m)

instance Applicative (UnifyM s) where
    pure = UnifyM . pure . pure . pure
    UnifyM f <*> UnifyM x = UnifyM $ \r -> do
        f' <- f r
        case f' of
          Nothing -> return Nothing
          Just f'' -> do
              x' <- x r
              case x' of
                  Nothing -> return Nothing
                  Just x'' -> return (Just (f'' x''))

instance Monad (UnifyM s) where
    return = pure
    UnifyM m >>= f = UnifyM $ \r -> do
        x <- m r
        case x of
            Nothing -> return Nothing
            Just x' -> unUnifyM (f x') r

-- | Lift a computation from 'ST' monad to 'UnifyM' monad.
-- Internal use only.
liftST :: ST s a -> UnifyM s a
liftST m = UnifyM $ \_ -> fmap Just m

addErr :: MsgDoc -> UnifyM s ()
addErr msg = do
    env <- getUnifEnv
    let err = ErrMsg {
                err_msg = msg,
                err_ctx = unify_ctx env
              }
    liftST $ modifySTRef (unify_errs env) (\errs -> err:errs)

failWith :: MsgDoc -> UnifyM s a
failWith msg = do
    addErr msg
    failM

failM :: UnifyM s a
failM = UnifyM $ \_ -> return Nothing

failIfErrs :: UnifyM s ()
failIfErrs = do
    env <- getUnifEnv
    errs <- liftST $ readSTRef (unify_errs env)
    when (not (null errs)) failM

tryM :: UnifyM s a -> UnifyM s (Maybe a)
tryM m =
    UnifyM (\env -> do
        mb_r <- unUnifyM m env
        return (Just mb_r))

{-
otherFail :: ErrMsg -> UnifyM s a
otherFail s = UnifyM $ \_ -> return (Left s)

unifyFail :: ErrMsg -> UnifyM s a
unifyFail err = do
    env <- getUnifEnv
    msg <- case unify_ctx env of
        Nothing -> return (text "Unspecified unification error:" <+> err)
        Just (ctx, mod1, mod2)
            | unify_verbosity env > normal
            -> do mod1' <- convertModuleU mod1
                  mod2' <- convertModuleU mod2
                  let extra = " (was unifying " ++ display mod1'
                                     ++ " and " ++ display mod2' ++ ")"
                  return (ctx ++ err ++ extra)
            | otherwise
            -> return (ctx ++ err ++ " (for more information, pass -v flag)")
    UnifyM $ \_ -> return (Left msg)
-}

-- | A convenient alias for mutable references in the unification monad.
type UnifRef s a = STRef s a

-- | Imperatively read a 'UnifRef'.
readUnifRef :: UnifRef s a -> UnifyM s a
readUnifRef = liftST . readSTRef

-- | Imperatively write a 'UnifRef'.
writeUnifRef :: UnifRef s a -> a -> UnifyM s ()
writeUnifRef x = liftST . writeSTRef x

-- | Get the current unification environment.
getUnifEnv :: UnifyM s (UnifEnv s)
getUnifEnv = UnifyM $ \r -> return (return r)

-- | Add a fixed message to the error context.
addErrContext :: Doc -> UnifyM s a -> UnifyM s a
addErrContext ctx m = addErrContextM ctx m

-- | Add a message to the error context.  It may make monadic queries.
addErrContextM :: ErrCtx s -> UnifyM s a -> UnifyM s a
addErrContextM ctx m =
    UnifyM $ \r -> unUnifyM m r { unify_ctx = ctx : unify_ctx r }


-----------------------------------------------------------------------
-- The "unifiable" variants of the data types
--
-- In order to properly do unification over infinite trees, we
-- need to union find over 'Module's and 'UnitId's.  The pure
-- representation is ill-equipped to do this, so we convert
-- from the pure representation into one which is indirected
-- through union-find.  'ModuleU' handles hole variables;
-- 'UnitIdU' handles mu-binders.

-- | Contents of a mutable 'ModuleU' reference.
data ModuleU' s
    = ModuleU (UnitIdU s) ModuleName
    | ModuleVarU ModuleName

-- | Contents of a mutable 'UnitIdU' reference.
data UnitIdU' s
    = UnitIdU UnitIdUnique ComponentId (Map ModuleName (ModuleU s))
    | UnitIdThunkU DefUnitId

-- | A mutable version of 'Module' which can be imperatively unified.
type ModuleU s = UnionFind.Point s (ModuleU' s)

-- | A mutable version of 'UnitId' which can be imperatively unified.
type UnitIdU s = UnionFind.Point s (UnitIdU' s)

-- | An integer for uniquely labeling 'UnitIdU' nodes.  We need
-- these labels in order to efficiently serialize 'UnitIdU's into
-- 'UnitId's (we use the label to check if any parent is the
-- node in question, and if so insert a deBruijn index instead.)
-- These labels must be unique across all 'UnitId's/'Module's which
-- participate in unification!
type UnitIdUnique = Int


-----------------------------------------------------------------------
-- Conversion to the unifiable data types

-- An environment for tracking the mu-bindings in scope.
-- The invariant for a state @(m, i)@ is that [0..i] are
-- keys of @m@; in fact, the @i-k@th entry is the @k@th
-- de Bruijn index (this saves us from having to shift as
-- we enter mu-binders.)
type MuEnv s = (IntMap (UnitIdU s), Int)

extendMuEnv :: MuEnv s -> UnitIdU s -> MuEnv s
extendMuEnv (m, i) x =
    (IntMap.insert (i + 1) x m, i + 1)

{-
lookupMuEnv :: MuEnv s -> Int {- de Bruijn index -} -> UnitIdU s
lookupMuEnv (m, i) k =
    case IntMap.lookup (i - k) m of
        -- Technically a user can trigger this by giving us a
        -- bad 'UnitId', so handle this better.
        Nothing -> error "lookupMuEnv: out of bounds (malformed de Bruijn index)"
        Just v -> v
-}

emptyMuEnv :: MuEnv s
emptyMuEnv = (IntMap.empty, -1)

-- The workhorse functions.  These share an environment:
--   * @UnifRef s UnitIdUnique@ - the unique label supply for 'UnitIdU' nodes
--   * @UnifRef s (Map ModuleName moduleU)@ - the (lazily initialized)
--     environment containing the implicitly universally quantified
--     @hole:A@ binders.
--   * @MuEnv@ - the environment for mu-binders.

convertUnitId' :: MuEnv s
               -> OpenUnitId
               -> UnifyM s (UnitIdU s)
-- TODO: this could be more lazy if we know there are no internal
-- references
convertUnitId' _ (DefiniteUnitId uid) =
    liftST $ UnionFind.fresh (UnitIdThunkU uid)
convertUnitId' stk (IndefFullUnitId cid insts) = do
    fs <- fmap unify_uniq getUnifEnv
    x <- liftST $ UnionFind.fresh (error "convertUnitId") -- tie the knot later
    insts_u <- T.forM insts $ convertModule' (extendMuEnv stk x)
    u <- readUnifRef fs
    writeUnifRef fs (u+1)
    y <- liftST $ UnionFind.fresh (UnitIdU u cid insts_u)
    liftST $ UnionFind.union x y
    return y
-- convertUnitId' stk (UnitIdVar i) = return (lookupMuEnv stk i)

convertModule' :: MuEnv s
               -> OpenModule -> UnifyM s (ModuleU s)
convertModule' _stk (OpenModuleVar mod_name) = do
    hmap <- fmap unify_reqs getUnifEnv
    hm <- readUnifRef hmap
    case Map.lookup mod_name hm of
        Nothing -> do mod <- liftST $ UnionFind.fresh (ModuleVarU mod_name)
                      writeUnifRef hmap (Map.insert mod_name mod hm)
                      return mod
        Just mod -> return mod
convertModule' stk (OpenModule uid mod_name) = do
    uid_u <- convertUnitId' stk uid
    liftST $ UnionFind.fresh (ModuleU uid_u mod_name)

convertUnitId :: OpenUnitId -> UnifyM s (UnitIdU s)
convertUnitId = convertUnitId' emptyMuEnv

convertModule :: OpenModule -> UnifyM s (ModuleU s)
convertModule = convertModule' emptyMuEnv



-----------------------------------------------------------------------
-- Substitutions

-- | The mutable counterpart of a 'ModuleSubst' (not defined here).
type ModuleSubstU s = Map ModuleName (ModuleU s)

-- | Conversion of 'ModuleSubst' to 'ModuleSubstU'
convertModuleSubst :: Map ModuleName OpenModule -> UnifyM s (Map ModuleName (ModuleU s))
convertModuleSubst = T.mapM convertModule

-- | Conversion of 'ModuleSubstU' to 'ModuleSubst'
convertModuleSubstU :: ModuleSubstU s -> UnifyM s OpenModuleSubst
convertModuleSubstU = T.mapM convertModuleU

-----------------------------------------------------------------------
-- Conversion from the unifiable data types

-- An environment for tracking candidates for adding a mu-binding.
-- The invariant for a state @(m, i)@, is that if we encounter a node
-- labeled @k@ such that @m[k -> v]@, then we can replace this
-- node with the de Bruijn index @i-v@ referring to an enclosing
-- mu-binder; furthermore, @range(m) = [0..i]@.
type MooEnv = (IntMap Int, Int)

emptyMooEnv :: MooEnv
emptyMooEnv = (IntMap.empty, -1)

extendMooEnv :: MooEnv -> UnitIdUnique -> MooEnv
extendMooEnv (m, i) k = (IntMap.insert k (i + 1) m, i + 1)

lookupMooEnv :: MooEnv -> UnitIdUnique -> Maybe Int
lookupMooEnv (m, i) k =
    case IntMap.lookup k m of
        Nothing -> Nothing
        Just v -> Just (i-v) -- de Bruijn indexize

-- The workhorse functions

convertUnitIdU' :: MooEnv -> UnitIdU s -> UnifyM s OpenUnitId
convertUnitIdU' stk uid_u = do
    x <- liftST $ UnionFind.find uid_u
    case x of
        UnitIdThunkU uid -> return (DefiniteUnitId uid)
        UnitIdU u cid insts_u ->
            case lookupMooEnv stk u of
                Just _i ->
                    failWith (text "Unsupported mutually recursive unit identifier")
                    -- return (UnitIdVar i)
                Nothing -> do
                    insts <- T.forM insts_u $ convertModuleU' (extendMooEnv stk u)
                    return (IndefFullUnitId cid insts)

convertModuleU' :: MooEnv -> ModuleU s -> UnifyM s OpenModule
convertModuleU' stk mod_u = do
    mod <- liftST $ UnionFind.find mod_u
    case mod of
        ModuleVarU mod_name -> return (OpenModuleVar mod_name)
        ModuleU uid_u mod_name -> do
            uid <- convertUnitIdU' stk uid_u
            return (OpenModule uid mod_name)

-- Helper functions

convertUnitIdU :: UnitIdU s -> UnifyM s OpenUnitId
convertUnitIdU = convertUnitIdU' emptyMooEnv

convertModuleU :: ModuleU s -> UnifyM s OpenModule
convertModuleU = convertModuleU' emptyMooEnv

-- | An empty 'ModuleScopeU'.
emptyModuleScopeU :: ModuleScopeU s
emptyModuleScopeU = (Map.empty, Map.empty)


-- | The mutable counterpart of 'ModuleScope'.
type ModuleScopeU s = (ModuleProvidesU s, ModuleRequiresU s)
-- | The mutable counterpart of 'ModuleProvides'
type ModuleProvidesU s = Map ModuleName [ModuleWithSourceU s]
type ModuleRequiresU s = ModuleProvidesU s
type ModuleWithSourceU s = WithSource (ModuleU s)

-- TODO: Deduplicate this with Distribution.Backpack.MixLink.dispSource
ci_msg :: ComponentInclude (OpenUnitId, ModuleShape) IncludeRenaming -> Doc
ci_msg ci
  | ci_implicit ci = text "build-depends:" <+> pp_pn
  | otherwise = text "mixins:" <+> pp_pn <+> disp (ci_renaming ci)
  where
    pn = pkgName (ci_pkgid ci)
    pp_pn =
        case ci_cname ci of
            CLibName -> disp pn
            CSubLibName cn -> disp pn <<>> colon <<>> disp cn
            -- Shouldn't happen
            cn -> disp pn <+> parens (disp cn)

-- | Convert a 'ModuleShape' into a 'ModuleScopeU', so we can do
-- unification on it.
convertInclude
    :: ComponentInclude (OpenUnitId, ModuleShape) IncludeRenaming
    -> UnifyM s (ModuleScopeU s,
                 Either (ComponentInclude (UnitIdU s) ModuleRenaming) {- normal -}
                        (ComponentInclude (UnitIdU s) ModuleRenaming) {- sig -})
convertInclude ci@(ComponentInclude {
                    ci_ann_id = AnnotatedId {
                            ann_id = (uid, ModuleShape provs reqs),
                            ann_pid = pid,
                            ann_cname = compname
                        },
                    ci_renaming = incl@(IncludeRenaming prov_rns req_rns),
                    ci_implicit = implicit
               }) = addErrContext (text "In" <+> ci_msg ci) $ do
    let pn = packageName pid
        the_source | implicit
                   = FromBuildDepends pn compname
                   | otherwise
                   = FromMixins pn compname incl
        source = WithSource the_source

    -- Suppose our package has two requirements A and B, and
    -- we include it with @requires (A as X)@
    -- There are three closely related things we compute based
    -- off of @reqs@ and @reqs_rns@:
    --
    --      1. The requirement renaming (A -> X)
    --      2. The requirement substitution (A -> <X>, B -> <B>)

    -- Requirement renaming.  This is read straight off the syntax:
    --
    --      [nothing]          ==>  [empty]
    --      requires (B as Y)  ==>  B -> Y
    --
    -- Requirement renamings are NOT injective: if two requirements
    -- are mapped to the same name, the intent is to merge them
    -- together.  But they are *functions*, so @B as X, B as Y@ is
    -- illegal.

    req_rename_list <-
      case req_rns of
        DefaultRenaming -> return []
        HidingRenaming _ -> do
            -- Not valid here for requires!
            addErr $ text "Unsupported syntax" <+>
                     quotes (text "requires hiding (...)")
            return []
        ModuleRenaming rns -> return rns

    let req_rename_listmap :: Map ModuleName [ModuleName]
        req_rename_listmap =
            Map.fromListWith (++) [ (k,[v]) | (k,v) <- req_rename_list ]
    req_rename <- sequenceA . flip Map.mapWithKey req_rename_listmap $ \k vs0 ->
      case vs0 of
        []  -> error "req_rename"
        [v] -> return v
        v:vs -> do addErr $
                    text "Conflicting renamings of requirement" <+> quotes (disp k) $$
                    text "Renamed to: " <+> vcat (map disp (v:vs))
                   return v

    let req_rename_fn k = case Map.lookup k req_rename of
                            Nothing -> k
                            Just v  -> v

    -- Requirement substitution.
    --
    --      A -> X      ==>     A -> <X>
    let req_subst = fmap OpenModuleVar req_rename

    uid_u <- convertUnitId (modSubst req_subst uid)

    -- Requirement mapping.  This is just taking the range of the
    -- requirement substitution, and making a mapping so that it is
    -- convenient to merge things together.  It INCLUDES the implicit
    -- mappings.
    --
    --      A -> X      ==>     X -> <X>, B -> <B>
    reqs_u <- convertModuleRequires . Map.fromList $
                [ (k, [source (OpenModuleVar k)])
                | k <- map req_rename_fn (Set.toList reqs)
                ]

    -- Report errors if there were unused renamings
    let leftover = Map.keysSet req_rename `Set.difference` reqs
    unless (Set.null leftover) $
        addErr $
            hang (text "The" <+> text (showComponentName compname) <+>
                  text "from package" <+> quotes (disp pid)
                  <+> text "does not require:") 4
                 (vcat (map disp (Set.toList leftover)))

    -- Provision computation is more complex.
    -- For example, if we have:
    --
    --      include p (A as X) requires (B as Y)
    --          where A -> q[B=<B>]:A
    --
    -- Then we need:
    --
    --      X -> [("p", q[B=<B>]:A)]
    --
    -- There are a bunch of clever ways to present the algorithm
    -- but here is the simple one:
    --
    --      1. If we have a default renaming, apply req_subst
    --      to provs and use that.
    --
    --      2. Otherwise, build a map by successively looking
    --      up the referenced modules in the renaming in provs.
    --
    -- Importantly, overlapping rename targets get accumulated
    -- together.  It's not an (immediate) error.
    (pre_prov_scope, prov_rns') <-
        case prov_rns of
            DefaultRenaming -> return (Map.toList provs, prov_rns)
            HidingRenaming hides ->
                let hides_set = Set.fromList hides
                in let r = [ (k,v)
                           | (k,v) <- Map.toList provs
                           , not (k `Set.member` hides_set) ]
                   -- GHC doesn't understand hiding, so expand it out!
                   in return (r, ModuleRenaming (map ((\x -> (x,x)).fst) r))
            ModuleRenaming rns -> do
              r <- sequence
                [ case Map.lookup from provs of
                    Just m -> return (to, m)
                    Nothing -> failWith $
                        text "Package" <+> quotes (disp pid) <+>
                        text "does not expose the module" <+> quotes (disp from)
                | (from, to) <- rns ]
              return (r, prov_rns)
    let prov_scope = modSubst req_subst
                   $ Map.fromListWith (++)
                   [ (k, [source v])
                   | (k, v) <- pre_prov_scope ]

    provs_u <- convertModuleProvides prov_scope

    -- TODO: Assert that provs_u is empty if provs was empty
    return ((provs_u, reqs_u),
                -- NB: We test that requirements is not null so that
                -- users can create packages with zero module exports
                -- that cause some C library to linked in, etc.
                (if Map.null provs && not (Set.null reqs)
                    then Right -- is sig
                    else Left) (ComponentInclude {
                                    ci_ann_id = AnnotatedId {
                                            ann_id = uid_u,
                                            ann_pid = pid,
                                            ann_cname = compname
                                        },
                                    ci_renaming = prov_rns',
                                    ci_implicit = ci_implicit ci
                                    }))

-- | Convert a 'ModuleScopeU' to a 'ModuleScope'.
convertModuleScopeU :: ModuleScopeU s -> UnifyM s ModuleScope
convertModuleScopeU (provs_u, reqs_u) = do
    provs <- convertModuleProvidesU provs_u
    reqs  <- convertModuleRequiresU reqs_u
    -- TODO: Test that the requirements are still free. If they
    -- are not, they got unified, and that's dodgy at best.
    return (ModuleScope provs reqs)

-- | Convert a 'ModuleProvides' to a 'ModuleProvidesU'
convertModuleProvides :: ModuleProvides -> UnifyM s (ModuleProvidesU s)
convertModuleProvides = T.mapM (mapM (T.mapM convertModule))

-- | Convert a 'ModuleProvidesU' to a 'ModuleProvides'
convertModuleProvidesU :: ModuleProvidesU s -> UnifyM s ModuleProvides
convertModuleProvidesU = T.mapM (mapM (T.mapM convertModuleU))

convertModuleRequires :: ModuleRequires -> UnifyM s (ModuleRequiresU s)
convertModuleRequires = convertModuleProvides

convertModuleRequiresU :: ModuleRequiresU s -> UnifyM s ModuleRequires
convertModuleRequiresU = convertModuleProvidesU
