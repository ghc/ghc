
module Development.Shake.Internal.Rules.OrderOnly(
     orderOnly, orderOnlyBS
    ) where

import Development.Shake.Internal.Core.Types
import Development.Shake.Internal.Core.Action
import Development.Shake.Internal.Rules.File
import qualified Data.ByteString.Char8 as BS


-- | Define order-only dependencies, these are dependencies that will always
--   be built before continuing, but which aren't dependencies of this action.
--   Mostly useful for defining generated dependencies you think might be real dependencies.
--   If they turn out to be real dependencies, you should add an explicit dependency afterwards.
--
-- @
-- \"source.o\" %> \\out -> do
--     'orderOnly' [\"header.h\"]
--     'cmd_' \"gcc -c source.c -o source.o -MMD -MF source.m\"
--     'neededMakefileDependencies' \"source.m\"
-- @
--
--   If @header.h@ is included by @source.c@ then the call to 'needMakefileDependencies' will cause
--   it to be added as a real dependency. If it isn't, then the rule won't rebuild if it changes.
orderOnly :: [FilePath] -> Action ()
orderOnly = orderOnlyAction . need


orderOnlyBS :: [BS.ByteString] -> Action ()
orderOnlyBS = orderOnlyAction . needBS
