import Control.Exception
import qualified Data.ByteString.Char8 as B
import GHC.Compact

main = compact (B.pack ['a'..'c'])
