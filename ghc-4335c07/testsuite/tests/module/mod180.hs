module ShouldFail where

import qualified Mod180_B as Mod180_A
import Mod180_B (T)
import Mod180_A (x)

z :: T
z = x
  -- fiendishly, this error message must mention Mod180_A.T (the type
  -- of x), but in the current scope, Mod180_A.T means something different:
  -- due to the 'import .. as' above, Mod180_A.T actually refers to Mod180_B.T.
  -- GHC should notice this and use a fully qualified name "main:Mod180_A.T"
  -- in the error message.
