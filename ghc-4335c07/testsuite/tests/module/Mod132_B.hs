-- exporting everything but the Foo dcon.
module Mod132_B (module Mod132_A) where

import Mod132_A hiding (Foo)
import Mod132_A (Foo)
