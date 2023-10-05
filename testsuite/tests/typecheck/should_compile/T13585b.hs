module T13585b where

import T13585a
import Data.Monoid

extractZonedTime :: Maybe ()
extractZonedTime = ala First foldMap [Nothing]
