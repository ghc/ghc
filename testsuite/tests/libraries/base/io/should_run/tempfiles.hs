import System.FilePath
import System.Directory
import System.IO
import Data.List

tmpDir = "/tmp"

-- Checks that openTempFile returns filenames with the right structure
main = do
 (fp0,h0) <- openTempFile tmpDir ".no_prefix.hs"
 print $ ".hs" `isSuffixOf` fp0 && ".no_prefix" `isPrefixOf` (takeFileName fp0)
 (fp1,h1) <- openTempFile tmpDir "no_suffix"
 print $ (not ('.' `elem` fp1)) && "no_suffix" `isPrefixOf` (takeFileName fp1)
 (fp2,h2) <- openTempFile tmpDir "one_suffix.hs"
 print $ (".hs" `isSuffixOf` fp2) && "one_suffix" `isPrefixOf` (takeFileName fp2)
 (fp3,h3) <- openTempFile tmpDir "two_suffixes.hs.blah"
 print $ (".blah" `isSuffixOf` fp3) && "two_suffixes.hs" `isPrefixOf` (takeFileName fp3)
 mapM_ hClose [h0, h1, h2, h3]
 mapM_ removeFile [fp0, fp1, fp2, fp3]
