import Control.DeepSeq
import Control.Exception
import qualified Data.ByteString.Char8 as B
import Data.Compact

main = compact (B.pack "abc")
