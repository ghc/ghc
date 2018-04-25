import System.Environment
import qualified Data.ByteString.Char8 as B
main = do
    n <- head `fmap` getArgs
    f <- B.readFile n
    print . B.count '\n' $ f

-- import qualified Data.ByteString.Lazy as L
-- main = print . L.count 10 =<< L.getContents

--
-- rule should rewrite this to:
-- main = print . length . B.lines =<< B.readFile "bigdata" -- B.getContents
--
