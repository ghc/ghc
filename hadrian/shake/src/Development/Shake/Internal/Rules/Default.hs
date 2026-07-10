
module Development.Shake.Internal.Rules.Default(
    defaultRules
    ) where

import Development.Shake.Internal.Core.Rules
import Development.Shake.Internal.Rules.Directory
import Development.Shake.Internal.Rules.File
import Development.Shake.Internal.Rules.Files
import Development.Shake.Internal.Rules.Rerun

-- All the rules baked into Shake
defaultRules :: Rules ()
defaultRules = do
    defaultRuleFile
    defaultRuleFiles
    defaultRuleDirectory
    defaultRuleRerun
