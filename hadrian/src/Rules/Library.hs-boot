module Rules.Library where

import Base
import Context

-- Necessary for inter-dependence between Rules.Register and
-- Rules.Library.
needLibrary :: [Context] -> Action ()
