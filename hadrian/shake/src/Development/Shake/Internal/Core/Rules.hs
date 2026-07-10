{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, ConstraintKinds, NamedFieldPuns #-}
{-# LANGUAGE ExistentialQuantification, RankNTypes #-}
{-# LANGUAGE TypeFamilies, TypeOperators, DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}

module Development.Shake.Internal.Core.Rules(
    Rules, SRules(..), runRules,
    RuleResult, addBuiltinRule, addBuiltinRuleEx,
    noLint, noIdentity,
    getShakeOptionsRules,
    getUserRuleInternal, getUserRuleOne, getUserRuleList, getUserRuleMaybe,
    addUserRule, alternatives, priority, versioned,
    getTargets, addTarget, withTargetDocs, withoutTargets,
    addHelpSuffix, getHelpSuffix,
    action, withoutActions
    ) where

import Control.Applicative
import Data.Tuple.Extra
import Control.Exception
import Control.Monad.Extra
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Development.Shake.Classes
import General.Binary
import General.Extra
import Data.Typeable
import Data.Data
import Data.List.Extra
import qualified Data.HashMap.Strict as Map
import qualified General.TypeMap as TMap
import Data.Maybe
import Data.IORef
import Data.Semigroup
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Binary.Builder as Bin
import Data.Binary.Put
import Data.Binary.Get
import General.ListBuilder
import Control.Monad.Fail
import Prelude

import Development.Shake.Internal.Core.Types
import Development.Shake.Internal.Core.Monad
import Development.Shake.Internal.Value
import Development.Shake.Internal.Options
import Development.Shake.Internal.Errors


---------------------------------------------------------------------
-- RULES

-- | Get the 'ShakeOptions' that were used.
getShakeOptionsRules :: Rules ShakeOptions
getShakeOptionsRules = Rules $ asks fst


-- | Internal variant, more flexible, but not such a nice API
--   Same args as getuserRuleMaybe, but returns (guaranteed version, items, error to throw if wrong number)
--   Fields are returned lazily, in particular ver can be looked up cheaper
getUserRuleInternal :: forall key a b . (ShakeValue key, Typeable a) => key -> (a -> Maybe String) -> (a -> Maybe b) -> Action (Maybe Ver, [(Int, b)], SomeException)
getUserRuleInternal key disp test = do
    Global{..} <- Action getRO
    let UserRuleVersioned versioned rules = fromMaybe mempty $ TMap.lookup globalUserRules
    let ver = if versioned then Nothing else Just $ Ver 0
    let items = headDef [] $ map snd $ reverse $ groupSort $ f (Ver 0) Nothing rules
    let err = errorMultipleRulesMatch (typeOf key) (show key) (map snd3 items)
    pure (ver, map (\(Ver v,_,x) -> (v,x)) items, err)
    where
        f :: Ver -> Maybe Double -> UserRule a -> [(Double,(Ver,Maybe String,b))]
        f v p (UserRule x) = [(fromMaybe 1 p, (v,disp x,x2)) | Just x2 <- [test x]]
        f v p (Unordered xs) = concatMap (f v p) xs
        f v p (Priority p2 x) = f v (Just $ fromMaybe p2 p) x
        f _ p (Versioned v x) = f v p x
        f v p (Alternative x) = take 1 $ f v p x


-- | Get the user rules that were added at a particular type which return 'Just' on a given function.
--   Return all equally applicable rules, paired with the version of the rule
--   (set by 'versioned'). Where rules are specified with 'alternatives' or 'priority'
--   the less-applicable rules will not be returned.
--
--   If you can only deal with zero/one results, call 'getUserRuleMaybe' or 'getUserRuleOne',
--   which raise informative errors.
getUserRuleList :: Typeable a => (a -> Maybe b) -> Action [(Int, b)]
getUserRuleList test = snd3 <$> getUserRuleInternal () (const Nothing) test


-- | A version of 'getUserRuleList' that fails if there is more than one result
--   Requires a @key@ for better error messages.
getUserRuleMaybe :: (ShakeValue key, Typeable a) => key -> (a -> Maybe String) -> (a -> Maybe b) -> Action (Maybe (Int, b))
getUserRuleMaybe key disp test = do
    (_, xs, err) <- getUserRuleInternal key disp test
    case xs of
        [] -> pure Nothing
        [x] -> pure $ Just x
        _ -> throwM err

-- | A version of 'getUserRuleList' that fails if there is not exactly one result
--   Requires a @key@ for better error messages.
getUserRuleOne :: (ShakeValue key, Typeable a) => key -> (a -> Maybe String) -> (a -> Maybe b) -> Action (Int, b)
getUserRuleOne key disp test = do
    (_, xs, err) <- getUserRuleInternal key disp test
    case xs of
        [x] -> pure x
        _ -> throwM err


-- | Define a set of rules. Rules can be created with calls to functions such as 'Development.Shake.%>' or 'action'.
--   Rules are combined with either the 'Monoid' instance, or (more commonly) the 'Monad' instance and @do@ notation.
--   To define your own custom types of rule, see "Development.Shake.Rule".
newtype Rules a = Rules (ReaderT (ShakeOptions, IORef (SRules ListBuilder)) IO a) -- All IO must be associative/commutative (e.g. creating IORef/MVars)
    deriving (Functor, Applicative, Monad, MonadIO, MonadFix, Control.Monad.Fail.MonadFail)

newRules :: SRules ListBuilder -> Rules ()
newRules x = Rules $ liftIO . flip modifyIORef' (<> x) =<< asks snd

modifyRulesScoped :: (SRules ListBuilder -> SRules ListBuilder) -> Rules a -> Rules a
modifyRulesScoped f (Rules r) = Rules $ do
    (opts, refOld) <- ask
    liftIO $ do
        refNew <- newIORef mempty
        res <- runReaderT r (opts, refNew)
        rules <- readIORef refNew
        modifyIORef' refOld (<> f rules)
        pure res

runRules :: ShakeOptions -> Rules () -> IO (SRules [])
runRules opts (Rules r) = do
    ref <- newIORef mempty{allowOverwrite = shakeAllowRedefineRules opts}
    runReaderT r (opts, ref)
    SRules{..} <- readIORef ref
    pure $ SRules (runListBuilder actions) builtinRules userRules (runListBuilder targets) (runListBuilder helpSuffix) allowOverwrite

-- | Get all targets registered in the given rules. The names in
--   'Development.Shake.phony' and 'Development.Shake.~>' as well as the file patterns
--   in 'Development.Shake.%>', 'Development.Shake.|%>' and 'Development.Shake.&%>' are
--   registered as targets, plus any explicit calls to 'addTarget'.
--   Returns the command, paired with the documentation (if any).
getTargets :: ShakeOptions -> Rules () -> IO [(String, Maybe String)]
getTargets opts rs = do
    SRules{targets} <- runRules opts rs
    pure [(target, documentation) | Target{..} <- targets]

getHelpSuffix :: ShakeOptions -> Rules () -> IO [String]
getHelpSuffix opts rs = do
    SRules{helpSuffix} <- runRules opts rs
    pure helpSuffix

data Target = Target
    {target :: !String
    ,documentation :: !(Maybe String)
    } deriving (Eq,Ord,Show,Read,Data,Typeable)

data SRules list = SRules
    {actions :: !(list (Stack, Action ()))
    ,builtinRules :: !(Map.HashMap TypeRep{-k-} BuiltinRule)
    ,userRules :: !(TMap.Map UserRuleVersioned)
    ,targets :: !(list Target)
    ,helpSuffix :: !(list String)
    ,allowOverwrite :: Bool
    }

instance Semigroup (SRules ListBuilder) where
    (SRules x1 x2 x3 x4 x5 x6) <> (SRules y1 y2 y3 y4 y5 y6) =
      SRules (mappend x1 y1) (Map.unionWithKey f x2 y2) (TMap.unionWith (<>) x3 y3) (mappend x4 y4) (mappend x5 y5) canOverwrite
      where
        canOverwrite = x6 && y6
        f k a b
          | canOverwrite = b
          | otherwise = throwImpure $ errorRuleDefinedMultipleTimes k [builtinLocation a, builtinLocation b]

instance Monoid (SRules ListBuilder) where
    mempty = SRules mempty Map.empty TMap.empty mempty mempty True
    mappend = (<>)

instance Semigroup a => Semigroup (Rules a) where
    (<>) = liftA2 (<>)

instance (Semigroup a, Monoid a) => Monoid (Rules a) where
    mempty = pure mempty
    mappend = (<>)


-- | Add a user rule. In general these should be specialised to the type expected by a builtin rule.
--   The user rules can be retrieved by 'getUserRuleList'.
addUserRule :: Typeable a => a -> Rules ()
addUserRule r = newRules mempty{userRules = TMap.singleton $ UserRuleVersioned False $ UserRule r}

-- | Register a target, as available when passing @--help@ or through 'getTargets'.
--   Called automatically by rules such as 'Development.Shake.phony' and
--   'Development.Shake.%>' - to avoid that use 'withoutTargets'.
--   To add documentation to a target use 'withTargetDocs'.
addTarget :: String -> Rules ()
addTarget t = newRules mempty{targets = newListBuilder $ Target t Nothing}

-- | For all 'addTarget' targets within the 'Rules' provide the specified documentation, if they
--   don't already have documentation.
withTargetDocs :: String -> Rules () -> Rules ()
withTargetDocs d = modifyRulesScoped $ \x -> x{targets = f <$> targets x}
    where f (Target a b) = Target a $ Just $ fromMaybe d b

-- | Remove all targets specified in a set of rules, typically because they are internal details.
--   Overrides 'addTarget'.
withoutTargets :: Rules a -> Rules a
withoutTargets = modifyRulesScoped $ \x -> x{targets=mempty}

-- | Adds some extra information at the end of @--help@.
addHelpSuffix :: String -> Rules ()
addHelpSuffix s = newRules mempty{helpSuffix = newListBuilder s}

-- | A suitable 'BuiltinLint' that always succeeds.
noLint :: BuiltinLint key value
noLint _ _ = pure Nothing

-- | A suitable 'BuiltinIdentity' that always fails with a runtime error, incompatible with 'shakeShare'.
--   Use this function if you don't care about 'shakeShare', or if your rule provides a dependency that can
--   never be cached (in which case you should also call 'Development.Shake.historyDisable').
noIdentity :: BuiltinIdentity key value
noIdentity _ _ = Nothing


-- | The type mapping between the @key@ or a rule and the resulting @value@.
--   See 'addBuiltinRule' and 'Development.Shake.Rule.apply'.
type family RuleResult key -- = value

-- | Before looking at this function, you should read the warnings at the top of this module.
--   This function is not often necessary in build systems.
--
--   Define a builtin rule, passing the functions to run in the right circumstances.
--   The @key@ and @value@ types will be what is used by 'Development.Shake.Rule.apply'.
--   As a start, you can use 'noLint' and 'noIdentity' as the first two functions,
--   but are required to supply a suitable 'BuiltinRun'.
--
--   Raises an error if any other rule exists at this type.
--
--   For a worked example of writing a rule see <https://tech-blog.capital-match.com/posts/5-upgrading-shake.html>.
addBuiltinRule
    :: (RuleResult key ~ value, ShakeValue key, Typeable value, NFData value, Show value, Partial)
    => BuiltinLint key value -> BuiltinIdentity key value -> BuiltinRun key value -> Rules ()
addBuiltinRule = withFrozenCallStack $ addBuiltinRuleInternal $ BinaryOp
    (putEx . Bin.toLazyByteString . execPut . put)
    (runGet get . LBS.fromChunks . pure)

addBuiltinRuleEx
    :: (RuleResult key ~ value, ShakeValue key, BinaryEx key, Typeable value, NFData value, Show value, Partial)
    => BuiltinLint key value -> BuiltinIdentity key value -> BuiltinRun key value -> Rules ()
addBuiltinRuleEx = addBuiltinRuleInternal $ BinaryOp putEx getEx


-- | Unexpected version of 'addBuiltinRule', which also lets me set the 'BinaryOp'.
addBuiltinRuleInternal
    :: (RuleResult key ~ value, ShakeValue key, Typeable value, NFData value, Show value, Partial)
    => BinaryOp key -> BuiltinLint key value -> BuiltinIdentity key value -> BuiltinRun key value -> Rules ()
addBuiltinRuleInternal binary lint check (run :: BuiltinRun key value) = do
    let k = Proxy :: Proxy key
    let lint_ k v = lint (fromKey k) (fromValue v)
    let check_ k v = check (fromKey k) (fromValue v)
    let run_ k v b = fmap newValue <$> run (fromKey k) v b
    let binary_ = BinaryOp (putOp binary . fromKey) (newKey . getOp binary)
    newRules mempty{builtinRules = Map.singleton (typeRep k) $ BuiltinRule lint_ check_ run_ binary_ (Ver 0) callStackTop}


-- | Change the priority of a given set of rules, where higher values take precedence.
--   All matching rules at a given priority must be disjoint, or an error is raised.
--   All builtin Shake rules have priority between 0 and 1.
--   Excessive use of 'priority' is discouraged. As an example:
--
-- @
-- 'priority' 4 $ \"hello.*\" %> \\out -> 'writeFile'' out \"hello.*\"
-- 'priority' 8 $ \"*.txt\" %> \\out -> 'writeFile'' out \"*.txt\"
-- @
--
--   In this example @hello.txt@ will match the second rule, instead of raising an error about ambiguity.
--
--   The 'priority' function obeys the invariants:
--
-- @
-- 'priority' p1 ('priority' p2 r1) === 'priority' p1 r1
-- 'priority' p1 (r1 >> r2) === 'priority' p1 r1 >> 'priority' p1 r2
-- @
priority :: Double -> Rules a -> Rules a
priority d = modifyRulesScoped $ \s -> s{userRules = TMap.map (\(UserRuleVersioned b x) -> UserRuleVersioned b $ Priority d x) $ userRules s}


-- | Indicate that the nested rules have a given version. If you change the semantics of the rule then updating (or adding)
--   a version will cause the rule to rebuild in some circumstances.
--
-- @
-- 'versioned' 1 $ \"hello.*\" %> \\out ->
--     'writeFile'' out \"Writes v1 now\" -- previously wrote out v0
-- @
--
--   You should only use 'versioned' to track changes in the build source, for standard runtime dependencies you should use
--   other mechanisms, e.g. 'Development.Shake.addOracle'.
versioned :: Int -> Rules a -> Rules a
versioned v = modifyRulesScoped $ \s -> s
    {userRules = TMap.map (\(UserRuleVersioned b x) -> UserRuleVersioned (b || v /= 0) $ Versioned (Ver v) x) $ userRules s
    ,builtinRules = Map.map (\b -> b{builtinVersion = Ver v}) $ builtinRules s
    }


-- | Change the matching behaviour of rules so rules do not have to be disjoint, but are instead matched
--   in order. Only recommended for small blocks containing a handful of rules.
--
-- @
-- 'alternatives' $ do
--     \"hello.*\" %> \\out -> 'writeFile'' out \"hello.*\"
--     \"*.txt\" %> \\out -> 'writeFile'' out \"*.txt\"
-- @
--
--   In this example @hello.txt@ will match the first rule, instead of raising an error about ambiguity.
--   Inside 'alternatives' the 'priority' of each rule is not used to determine which rule matches,
--   but the resulting match uses that priority compared to the rules outside the 'alternatives' block.
alternatives :: Rules a -> Rules a
alternatives = modifyRulesScoped $ \r -> r{userRules = TMap.map (\(UserRuleVersioned b x) -> UserRuleVersioned b $ Alternative x) $ userRules r}


-- | Run an action, usually used for specifying top-level requirements.
--
-- @
-- main = 'Development.Shake.shake' 'shakeOptions' $ do
--    'action' $ do
--        b <- 'Development.Shake.doesFileExist' \"file.src\"
--        when b $ 'Development.Shake.need' [\"file.out\"]
-- @
--
--   This 'action' builds @file.out@, but only if @file.src@ exists. The 'action'
--   will be run in every build execution (unless 'withoutActions' is used), so only cheap
--   operations should be performed. On the flip side, consulting system information
--   (e.g. environment variables) can be done directly as the information will not be cached.
--   All calls to 'action' may be run in parallel, in any order.
--
--   For the standard requirement of only 'Development.Shake.need'ing a fixed list of files in the 'action',
--   see 'Development.Shake.want'.
action :: Partial => Action a -> Rules ()
action act = newRules mempty{actions=newListBuilder (addCallStack callStackFull emptyStack, void act)}


-- | Remove all actions specified in a set of rules, usually used for implementing
--   command line specification of what to build.
withoutActions :: Rules a -> Rules a
withoutActions = modifyRulesScoped $ \x -> x{actions=mempty}
