--
-- The unix wc -l program
--
import qualified Data.ByteString as B

main = print . B.count 10 =<< B.getContents
