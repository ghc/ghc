-- !!! ambiguous re-exportation.
module M(module M) where 

import Prelude as M
id x = x
