-- !!! redefining and using Prelude entities
module A( null, nonNull ) where
import Prelude hiding( null ) 
import qualified Prelude
null, nonNull :: Int -> Bool
null    x = x == 0
nonNull x = not (Prelude.null [x]) && not (null x)
