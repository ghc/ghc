{-# LANGUAGE RecordWildCards, ViewPatterns #-}

module Generate(main) where

import Control.Exception
import Control.Monad
import Data.Char
import Data.List
import System.Directory
import System.IO


main :: IO ()
main = do
    src <- readFile "System/FilePath/Internal.hs"
    let tests = map renderTest $ concatMap parseTest $ lines src
    writeFileBinaryChanged "tests/TestGen.hs" $ unlines $
        ["-- GENERATED CODE: See ../Generate.hs"
        ,"module TestGen(tests) where"
        ,"import TestUtil"
        ,"import qualified System.FilePath.Windows as W"
        ,"import qualified System.FilePath.Posix as P"
        ,"{-# ANN module \"HLint: ignore\" #-}"
        ,"tests :: [(String, Property)]"
        ,"tests ="] ++
        ["    " ++ c ++ "(" ++ show t1 ++ ", " ++ t2 ++ ")" | (c,(t1,t2)) <- zip ("[":repeat ",") tests] ++
        ["    ]"]



data PW = P | W deriving Show -- Posix or Windows
data Test = Test
    {testPlatform :: PW
    ,testVars :: [(String,String)]   -- generator constructor, variable
    ,testBody :: [String]
    }


parseTest :: String -> [Test]
parseTest (stripPrefix "-- > " -> Just x) = platform $ toLexemes x
    where
        platform ("Windows":":":x) = [valid W x]
        platform ("Posix"  :":":x) = [valid P x]
        platform x                 = [valid P x, valid W x]

        valid p ("Valid":x) = free p a $ drop 1 b
            where (a,b) = break (== "=>") x
        valid p x = free p [] x

        free p val x = Test p [(ctor v, v) | v <- vars] x
            where vars = nub $ sort [v | v@[c] <- x, isAlpha c]
                  ctor v | v < "x" = ""
                         | v `elem` val = "QFilePathValid" ++ show p
                         | otherwise = "QFilePath"
parseTest _ = []


toLexemes :: String -> [String]
toLexemes x = case lex x of
    [("","")] -> []
    [(x,y)] -> x : toLexemes y
    y -> error $ "Generate.toLexemes, " ++ show x ++ " -> " ++ show y


fromLexemes :: [String] -> String
fromLexemes = unwords . f
    where
        f ("`":x:"`":xs) = ("`" ++ x ++ "`") : f xs
        f (x:y:xs) | x `elem` ["[","("] || y `elem` [",",")","]"] = f $ (x ++ y) : xs
        f (x:xs) = x : f xs
        f [] = []


renderTest :: Test -> (String, String)
renderTest Test{..} = (body, code)
    where
        code = "property $ " ++ if null testVars then body else "\\" ++ unwords vars ++ " -> " ++ body
        vars = [if null ctor then v else "(" ++ ctor ++ " " ++ v ++ ")" | (ctor,v) <- testVars]

        body = fromLexemes $ map (qualify testPlatform) testBody


qualify :: PW -> String -> String
qualify pw str
    | str `elem` fpops || (all isAlpha str && length str > 1 && str `notElem` prelude) = show pw ++ "." ++ str
    | otherwise = str
    where
        prelude = ["elem","uncurry","snd","fst","not","null","if","then","else"
                  ,"True","False","Just","Nothing","fromJust","concat","isPrefixOf","isSuffixOf","any"]
        fpops = ["</>","<.>","-<.>"]


---------------------------------------------------------------------
-- UTILITIES

writeFileBinary :: FilePath -> String -> IO ()
writeFileBinary file x = withBinaryFile file WriteMode $ \h -> hPutStr h x

readFileBinary' :: FilePath -> IO String
readFileBinary' file = withBinaryFile file ReadMode $ \h -> do
    s <- hGetContents h
    evaluate $ length s
    return s

writeFileBinaryChanged :: FilePath -> String -> IO ()
writeFileBinaryChanged file x = do
    b <- doesFileExist file
    old <- if b then fmap Just $ readFileBinary' file else return Nothing
    when (Just x /= old) $
        writeFileBinary file x
