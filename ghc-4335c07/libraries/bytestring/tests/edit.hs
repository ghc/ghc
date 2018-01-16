import System.Environment
import qualified Data.ByteString.Char8 as B

main = do
    [f] <- getArgs
    B.writeFile f . B.unlines . map edit . B.lines =<< B.readFile f

    where
        edit :: B.ByteString -> B.ByteString
        edit s | (B.pack "Instances") `B.isPrefixOf` s = B.pack "EDIT"
               | otherwise                             = s
