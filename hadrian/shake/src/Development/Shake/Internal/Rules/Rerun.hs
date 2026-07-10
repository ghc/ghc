{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Development.Shake.Internal.Rules.Rerun(
    defaultRuleRerun, alwaysRerun
    ) where

import Development.Shake.Internal.Core.Rules
import Development.Shake.Internal.Core.Types
import Development.Shake.Internal.Core.Build
import Development.Shake.Internal.Core.Action
import Development.Shake.Classes
import qualified Data.ByteString as BS
import General.Binary


newtype AlwaysRerunQ = AlwaysRerunQ ()
    deriving (Typeable,Eq,Hashable,Binary,BinaryEx,NFData)
instance Show AlwaysRerunQ where show _ = "alwaysRerun"

type instance RuleResult AlwaysRerunQ = ()


-- | Always rerun the associated action. Useful for defining rules that query
--   the environment. For example:
--
-- @
-- \"ghcVersion.txt\" 'Development.Shake.%>' \\out -> do
--     'alwaysRerun'
--     'Development.Shake.Stdout' stdout <- 'Development.Shake.cmd' \"ghc --numeric-version\"
--     'Development.Shake.writeFileChanged' out stdout
-- @
--
--   In @make@, the @.PHONY@ attribute on file-producing rules has a similar effect.
--
--   Note that 'alwaysRerun' is applied when a rule is executed. Modifying an existing rule
--   to insert 'alwaysRerun' will /not/ cause that rule to rerun next time.
alwaysRerun :: Action ()
alwaysRerun = do
    historyDisable
    apply1 $ AlwaysRerunQ ()

defaultRuleRerun :: Rules ()
defaultRuleRerun =
    addBuiltinRuleEx noLint noIdentity $
        \AlwaysRerunQ{} _ _ -> pure $ RunResult ChangedRecomputeDiff BS.empty ()
