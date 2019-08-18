module HpcUtils where

import Trace.Hpc.Util (catchIO, HpcPos, fromHpcPos)
import qualified Data.Map as Map
import System.Directory (createDirectoryIfMissing)
import System.FilePath
import System.IO

dropWhileEndLE :: (a -> Bool) -> [a] -> [a]
-- Spec: dropWhileEndLE p = reverse . dropWhile p . reverse
dropWhileEndLE p = foldr (\x r -> if null r && p x then [] else x:r) []

-- turns \n into ' '
-- | grab's the text behind a HpcPos; 
grabHpcPos :: Map.Map Int String -> HpcPos -> String
grabHpcPos hsMap srcspan = 
         case lns of
           [ln] -> (take ((c2 - c1) + 1) $ drop (c1 - 1) ln)
           _ -> let lns1 = drop (c1 -1) (head lns) : tail lns
                    lns2 = init lns1 ++ [take (c2 + 1) (last lns1) ]
                 in foldl1 (\ xs ys -> xs ++ "\n" ++ ys) lns2
  where (l1,c1,l2,c2) = fromHpcPos srcspan
        lns = map (\ n -> case Map.lookup n hsMap of
                           Just ln -> ln
                           Nothing -> error $ "bad line number : " ++ show n
                  ) [l1..l2]


readFileFromPath :: (String -> IO String) -> String -> [String] -> IO String
readFileFromPath _ filename@('/':_) _ = readFileUtf8 filename
readFileFromPath err filename path0 = readTheFile path0
  where
        readTheFile [] = err $ "could not find " ++ show filename
                                 ++ " in path " ++ show path0
        readTheFile (dir:dirs) =
                catchIO (readFileUtf8 (dir </> filename))
                        (\ _ -> readTheFile dirs)

-- | Read a file in the same manner as `readFile` (using lazy IO), but disregard system
-- locale and assume that the file is encoded in UTF-8. Haskell source files are expected
-- to be encoded in UTF-8 by GHC.
readFileUtf8 :: FilePath -> IO String
readFileUtf8 filepath =
  openBinaryFile filepath ReadMode >>= \h -> do
    hSetEncoding h utf8  -- see #17073
    hGetContents h

-- | Write file in UTF-8 encoding. Parent directory will be created if missing.
writeFileUtf8 :: FilePath -> String -> IO ()
writeFileUtf8 filepath str = do
  createDirectoryIfMissing True (takeDirectory filepath)
  withBinaryFile filepath WriteMode $ \h -> do
    hSetEncoding h utf8  -- see #17073
    hPutStr h str
