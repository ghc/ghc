
-- | This module is used for defining new types of rules for Shake build systems, e.g. to support values stored in a database.
--   Most users will find the built-in set of rules sufficient. The functions in this module are designed for high-performance,
--   not ease of use or abstraction. As a result, they are difficult to work with and change more often than the other parts of Shake.
--   Before writing a builtin rule you are encouraged to use 'Development.Shake.addOracle' or 'Development.Shake.addOracleCache' if possible.
--   With all those warnings out the way, read on for the grungy details.
module Development.Shake.Rule(
    -- * Builtin rules
    -- $builtin_rules

    -- ** Extensions
    -- $extensions

    -- ** Worked example
    -- $example

    -- * Defining builtin rules
    -- | Functions and types for defining new types of Shake rules.
    addBuiltinRule,
    BuiltinLint, noLint, BuiltinIdentity, noIdentity, BuiltinRun, RunMode(..), RunChanged(..), RunResult(..),
    -- * Calling builtin rules
    -- | Wrappers around calling Shake rules. In general these should be specialised to a builtin rule.
    apply, apply1,
    -- * User rules
    -- | Define user rules that can be used by builtin rules.
    --   Absent any builtin rule making use of a user rule at a given type, a user rule will have on effect -
    --   they have no inherent effect or interpretation on their own.
    addUserRule, getUserRuleList, getUserRuleMaybe, getUserRuleOne,
    -- * Lint integration
    -- | Provide lint warnings when running code.
    lintTrackRead, lintTrackWrite, lintTrackAllow,
    -- * History caching
    -- | Interact with the non-local cache. When using the cache it is important that all
    --   rules have accurate 'BuiltinIdentity' functions.
    historyIsEnabled, historySave, historyLoad
    ) where

import Development.Shake.Internal.Core.Types
import Development.Shake.Internal.Core.Action
import Development.Shake.Internal.Core.Build
import Development.Shake.Internal.Core.Rules

-- $builtin_rules
--
--   Shake \"Builtin\" rules are ones map keys to values - e.g. files to file contents. For each builtin rule you need to think:
--
-- * What is the @key@ type, which uniquely identifies each location, e.g. a filename.
--
-- * What is the @value@ type. The @value@ is not necessarily the full value, but is the result people can get if they ask
--   for the value associated with the @key@. As an example, for files when you 'need' a file you don't get any value back from
--   the file, so a simple file rule could have @()@ as its value.
--
-- * What information is stored between runs. This information should be sufficient to check if the value has changed since last time,
--   e.g. the modification time for files.
--
--   Typically a custom rule will define a wrapper of type 'Rules' that calls 'addBuiltinRule', along with a type-safe wrapper over
--   'apply' so users can introduce dependencies.

-- $extensions
--
--   Once you have implemented the basic functionality there is more scope for embracing additional features of Shake, e.g.:
--
-- * You can integrate with cached history by providing a working 'BuiltinIdentity' and using 'historySave' and 'historyLoad'.
--
-- * You can let users provide their own rules which you interpret with 'addUserRule'.
--
-- * You can integrate with linting by specifying a richer 'BuiltinLint' and options like 'lintTrackRead'.
--
--   There are lots of rules defined in the Shake repo at <https://github.com/ndmitchell/shake/tree/master/src/Development/Shake/Internal/Rules>.
--   You are encouraged to read those for inspiration.

-- $example
--
--   Shake provides a very comprehensive file rule which currently runs to over 500 lines of code, and supports lots of features
--   and optimisations. However, let's imagine we want to define a simpler rule type for files. As mentioned earlier, we have to make some decisions.
--
-- * A @key@ will just be the file name.
--
-- * A @value@ will be @()@ - when the user depends on a file they don't expect any information in return.
--
-- * The stored information will be the contents of the file, in it's entirety. Alternative choices would be the modtime or a hash of the contents,
--   but Shake doesn't require that. The stored information in Shake must be stored in a 'ByteString', so we 'Data.ByteString.pack' and
--   'Data.ByteString.unpack' to convert.
--
-- * We will allow user rules to be defined saying how to build any individual file.
--
--   First we define the type of key and value, deriving all the necessary type classes. We define a @newtype@ over 'FilePath' so we can
--   guarantee not to conflict with anyone else. Typically you wouldn't export the @File@ type, providing only sugar functions over it.
--
-- > newtype File = File FilePath
-- >     deriving (Show,Eq,Hashable,Binary,NFData)
-- > type instance RuleResult File = ()
--
--   Since we have decided we are also going to have user rules, we need to define a new type to capture the information stored by the rules.
--   We need to store at least the file it is producing and the action, which we do with:
--
-- > data FileRule = FileRule File (Action ())
--
--   With the definitions above users could call 'apply' and 'addUserRule' directly, but that's tedious and not very type safe. To make it easier
--   we introduce some helpers:
--
-- > fileRule :: FilePath -> Action () -> Rules ()
-- > fileRule file act = addUserRule $ FileRule (File file) act
-- >
-- > fileNeed :: FilePath -> Action ()
-- > fileNeed = apply1 . File
--
--   These helpers just add our type names, providing a more pleasant interface for the user. Using these function we can
--   exercise our build system with:
--
-- > example = do
-- >     fileRule "a.txt" $ pure ()
-- >     fileRule "b.txt" $ do
-- >         fileNeed "a.txt"
-- >         liftIO $ writeFile "b.txt" . reverse =<< readFile "a.txt"
-- >
-- >     action $ fileNeed "b.txt"
--
--   This example defines rules for @a.txt@ (a source file) and @b.txt@ (the 'reverse' of @a.txt@). At runtime this example will
--   complain about not having a builtin rule for @File@, so the only thing left is to provide one.
--
-- > addBuiltinFileRule :: Rules ()
-- > addBuiltinFileRule = addBuiltinRule noLint noIdentity run
-- >     where
-- >         fileContents (File x) = do b <- IO.doesFileExist x; if b then IO.readFile' x else pure ""
-- >
-- >         run :: BuiltinRun File ()
-- >         run key old mode = do
-- >             now <- liftIO $ fileContents key
-- >             if mode == RunDependenciesSame && fmap BS.unpack old == Just now then
-- >                 pure $ RunResult ChangedNothing (BS.pack now) ()
-- >             else do
-- >                 (_, act) <- getUserRuleOne key (const Nothing) $ \(FileRule k act) -> if k == key then Just act else Nothing
-- >                 act
-- >                 now <- liftIO $ fileContents key
-- >                 pure $ RunResult ChangedRecomputeDiff (BS.pack now) ()
--
--   We define a wrapper @addBuiltinFileRule@ that calls @addBuiltinRule@, opting out of linting and cached storage.
--   The only thing we provide is a 'BuiltinRun' function which gets the previous state, and whether any dependency has changed,
--   and decides whether to rebuild. If something has changed we call 'getUserRuleOne' to find the users rule and rerun it.
--   The 'RunResult' says what changed (either 'ChangedNothing' or 'ChangedRecomputeDiff' in our cases), gives us a new stored value
--   (just packing the contents) and the @value@ which is @()@.
--
--   To execute our example we need to also call @addBuiltinFileRule@, and now everything works.
