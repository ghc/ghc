module Basement.Show
    where

import qualified Prelude
import           Basement.Compat.Base
import           Basement.UTF8.Base (String)

-- | Use the Show class to create a String.
--
-- Note that this is not efficient, since
-- an intermediate [Char] is going to be
-- created before turning into a real String.
show :: Prelude.Show a => a -> String
show = fromList . Prelude.show
