
-- | This module reexports the six necessary type classes that many rule types must support through 'ShakeValue'.
--   You can use this module to define new rules without depending on the @binary@, @deepseq@ and @hashable@ packages.
module Development.Shake.Classes(
    Show(..), Typeable, Eq(..), Hashable(..), Binary(..), NFData(..)
    ) where

-- I would probably reexport this module by default in Development.Shake,
-- but Binary defines 'get', which clashes with the State monad.

import Data.Hashable
import Data.Typeable
import Data.Binary
import Control.DeepSeq
