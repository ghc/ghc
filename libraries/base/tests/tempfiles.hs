
import Control.Exception
import Data.List
import System.FilePath
import System.Directory
import System.IO

-- Checks that openTempFile returns filenames with the right structure
main :: IO ()
main = do
 fp0 <- otf ".no_prefix.hs"
 print (".hs"        `isSuffixOf` fp0)
 print (".no_prefix" `isPrefixOf` takeFileName fp0)

 fp1 <- otf "no_suffix"
 print (not ('.' `elem` fp1))
 print ("no_suffix" `isPrefixOf` takeFileName fp1)

 fp2 <- otf "one_suffix.hs"
 print (".hs"        `isSuffixOf` fp2)
 print ("one_suffix" `isPrefixOf` takeFileName fp2)

 fp3 <- otf "two_suffixes.hs.blah"
 print (".blah"           `isSuffixOf` fp3)
 print ("two_suffixes.hs" `isPrefixOf` takeFileName fp3)

otf :: FilePath -> IO FilePath
otf fp = do putStrLn fp
            bracket (openTempFile "." fp)
                    (\(fp', h) -> do hClose h
                                     removeFile fp')
                    (\(fp', _) -> case fp' of
                                  '.' : '/'  : fp'' -> return fp''
                                  '.' : '\\' : fp'' -> return fp''
                                  _                 -> return fp')

