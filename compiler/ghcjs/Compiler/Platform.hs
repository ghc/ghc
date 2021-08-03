-- some platform-specific code

module Compiler.Platform ( isWindows
                         ) where

import Prelude

-- XXX fix this. do we have another way to detect windows?
isWindows :: Bool
isWindows = False
