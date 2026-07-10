{-# LANGUAGE PatternGuards, ViewPatterns #-}

module Test.Docs(main) where

import Development.Shake
import Development.Shake.FilePath
import qualified System.FilePattern.Directory as IO
import System.Directory
import Test.Type
import Data.Char
import Data.List.Extra


-- Can't get the paths to work on CI with tracking etc
main = testBuild (notCI . defaultTest) $ do
    let index = "dist/doc/html/shake/index.html"
    let setup = "dist/setup.exe"
    let config = "dist/setup-config"
    want ["Success.txt"]
    let trackIgnore = trackAllow ["dist/**", "dist-newstyle/**"]

    let needSource = need =<< getDirectoryFiles "." (map (shakeRoot </>)
            ["src/Development/Shake.hs","src/Development/Shake//*.hs","src/Development/Ninja/*.hs","src/General//*.hs"])

    let runSetup :: [String] -> Action ()
        runSetup args = do
            trackIgnore
            need [setup]
            -- Make Cabal and Stack play nicely with GHC_PACKAGE_PATH
            setup <- liftIO $ canonicalizePath setup
            cmd_ (RemEnv "GHC_PACKAGE_PATH") (Cwd shakeRoot) setup args

    setup %> \_ -> do
        -- Important to compile the setup binary, or we run foul of
        -- https://gitlab.haskell.org/ghc/ghc/issues/17575
        trackIgnore
        need [shakeRoot </> "Setup.hs"]
        setup <- liftIO $ canonicalizePath setup
        curdir <- liftIO $ canonicalizePath "dist"
        cmd_ (Cwd shakeRoot) "ghc -package=Cabal Setup.hs -o" [setup] "-outputdir" [curdir]

    config %> \_ -> do
        path <- getEnv "GHC_PACKAGE_PATH"
        dist <- liftIO $ canonicalizePath "dist" -- make sure it works even if we cwd
        need [shakeRoot </> "shake.cabal"]
        runSetup $
            ["configure","--builddir=" ++ dist,"--user"] ++
            -- package-db is very sensitive, see #267
            -- note that the reverse ensures the behaviour is consistent between the flags and the env variable
            ["--package-db=" ++ x | x <- maybe [] (reverse . filter (`notElem` [".",""]) . splitSearchPath) path]

        -- Paths_shake is only created by "Setup build" (which we want to skip), and required by "Setup haddock", so we fake it
        copyFile' (shakeRoot </> "src/Paths.hs") "dist/build/autogen/Paths_shake.hs"
        copyFile' (shakeRoot </> "src/Paths.hs") "dist/build/shake/autogen/Paths_shake.hs"
        writeFile' "dist/build/autogen/cabal_macros.h" ""
        writeFile' "dist/build/shake/autogen/cabal_macros.h" ""

    index %> \_ -> do
        need $ config : map (shakeRoot </>) ["shake.cabal","Setup.hs","README.md","CHANGES.txt","docs/Manual.md","docs/shake-progress.png"]
        needSource
        trackIgnore
        dist <- liftIO $ canonicalizePath "dist"
        runSetup ["haddock", "--builddir=" ++ dist]

    "Part_*.hs" %> \out -> do
        need [shakeRoot </> "src/Test/Docs.hs"] -- so much of the generator is in this module
        let noR = filter (/= '\r')
        src <- if "_md" `isSuffixOf` takeBaseName out then
            fmap (findCodeMarkdown . lines . checkBlacklist . noR) $ readFile' $ shakeRoot </> "docs/" ++ drop 5 (reverse (drop 3 $ reverse $ takeBaseName out)) ++ ".md"
         else
            fmap (findCodeHaddock . checkBlacklist . noR) $ readFile' $ "dist/doc/html/shake/" ++ replace "_" "-" (drop 5 $ takeBaseName out) ++ ".html"

        let (imports,rest) = partition ("import " `isPrefixOf`) $ showCode src
        writeFileChanged out $ unlines $
            ["{-# LANGUAGE DeriveDataTypeable, RankNTypes, ExtendedDefaultRules, GeneralizedNewtypeDeriving #-}"
            ,"{-# LANGUAGE NoMonomorphismRestriction, ScopedTypeVariables, ConstraintKinds, FlexibleContexts, TypeFamilies #-}"
            ,"{-# OPTIONS_GHC -w #-}"
            ,"module " ++ takeBaseName out ++ "() where"
            ,"import Control.Applicative"
            ,"import Control.Concurrent"
            ,"import Control.Exception"
            ,"import Control.Monad"
            ,"import Control.Monad.Trans.Reader"
            ,"import Data.ByteString(ByteString, pack, unpack)"
            ,"import qualified Data.ByteString.Char8 as BS"
            ,"import qualified System.Directory.Extra as IO"
            ,"import qualified System.IO.Extra as IO"
            ,"import Data.Char"
            ,"import Data.Data"
            ,"import Data.Dynamic"
            ,"import Data.List.Extra"
            ,"import System.Time.Extra"
            ,"import Data.Maybe"
            ,"import Data.Monoid"
            ,"import Development.Shake hiding ((*>))"
            ,"import Development.Shake.Command"
            ,"import Development.Shake.Classes"
            ,"import Development.Shake.Database"
            ,"import Development.Shake.Rule"
            ,"import Development.Shake.Util"
            ,"import Development.Shake.FilePath"
            ,"import System.Console.GetOpt"
            ,"import System.Directory(setCurrentDirectory, withCurrentDirectory)"
            ,"import qualified System.Directory"
            ,"import System.Environment(withArgs, lookupEnv, getEnvironment)"
            ,"import System.Process"
            ,"import System.Exit"
            ,"import Control.Applicative"
            ,"import Control.Monad.IO.Class"
            ,"import Control.Monad.Fail"
            ,"import System.IO hiding (readFile')"] ++
            ["import " ++ replace "_" "." (drop 5 $ takeBaseName out) | not $ "_md.hs" `isSuffixOf` out] ++
            imports ++
            ["(==>) :: Bool -> Bool -> Bool"
            ,"(==>) = undefined"
            ,"(<==) = ()"
            ,"infix 1 ==>"
            ,"infix 0 ==="
            ,"(===) :: a -> a -> b"
            ,"(===) = undefined"
            ,"forAll f = f undefined"
            ,"remaining = 1.1"
            ,"done = 1.1"
            ,"time_elapsed = 1.1"
            ,"old = \"\""
            ,"new = \"\""
            ,"myvar = \"\""
            ,"myfile = \"\""
            ,"inputs = [\"\"]"
            ,"files = [\"\"]"
            ,"input = \"\""
            ,"output = \"\""
            ,"opts = shakeOptions"
            ,"result = undefined :: IO (Maybe (Rules ()))"
            ,"launchMissiles = undefined :: Bool -> IO ()"
            ,"myVariable = ()"
            ,"instance Eq (OptDescr a)"
            ,"(foo,bar,baz) = undefined"
            ,"(p1,p2) = (0.0, 0.0)"
            ,"(r1,r2) = (pure () :: Rules(), pure () :: Rules())"
            ,"xs = []"
            ,"ys = []"
            ,"os = [\"file.o\"]"
            ,"out = \"\""
            ,"str1 = \"\""
            ,"str2 = \"\""
            ,"def = undefined"
            ,"var = undefined"
            ,"newValue = undefined"
            ,"newStore = BS.empty"
            ,"change = ChangedNothing"
            ,"str = \"\""] ++
            rest

    "Files.lst" %> \out -> do
        need [shakeRoot </> "src/Test/Docs.hs"] -- so much of the generator is in this module
        need [index]
        filesHs <- liftIO $ IO.getDirectoryFiles "dist/doc/html/shake" ["Development-*.html"]
        -- filesMd on Travis will only include Manual.md, since it's the only one listed in the .cabal
        -- On AppVeyor, where we build from source, it will check the rest of the website
        filesMd <- getDirectoryFiles (shakeRoot </> "docs") ["*.md"]
        writeFileChanged out $ unlines $
            ["Part_" ++ replace "-" "_" (takeBaseName x) | x <- filesHs,
                not $ any (`isSuffixOf` x) ["-Classes.html", "-FilePath.html"]] ++
            ["Part_" ++ takeBaseName x ++ "_md" | x <- filesMd,
                takeBaseName x `notElem` ["Developing","Model","Architecture"]]

    let needModules = do mods <- readFileLines "Files.lst"; need [m <.> "hs" | m <- mods]; pure mods

    "Main.hs" %> \out -> do
        mods <- needModules
        writeFileLines out $ ["module Main(main) where"] ++ ["import " ++ m ++ "()" | m <- mods] ++ ["main = pure ()"]

    "Success.txt" %> \out -> do
        putInfo . ("Checking documentation for:\n" ++) =<< readFile' "Files.lst"
        needModules
        need ["Main.hs"]
        trackIgnore
        needSource
        cmd_ "ghc -fno-code -ignore-package=hashmap" ["-idist/build/autogen","-i" ++ shakeRoot </> "src","Main.hs"]
        writeFile' out ""

checkBlacklist :: String -> String
checkBlacklist xs = if null bad then xs else error $ show ("Blacklist", bad)
    where bad = [(w, x) | x <- map lower $ lines xs, w <- blacklist, w `isInfixOf` x]

---------------------------------------------------------------------
-- FIND THE CODE

newtype Code = Code [String] deriving (Show,Eq,Ord)


findCodeHaddock :: String -> [Code]
findCodeHaddock src =
    [ Code $ unindent $ lines $ innerText x
    | tag <- ["code","pre"]
    , x <- insideTag tag src
    , let bad = nubOrd (insideTag "em" x) \\ italics
    , if null bad then True else error $ "Bad italics, " ++ show bad
    ]


findCodeMarkdown :: [String] -> [Code]
findCodeMarkdown (x:xs) | indented x && not (isBlank x) =
    let (a,b) = span (\x -> indented x || isBlank x) (x:xs)
    in Code (dropWhileEnd isBlank $ unindent a) : findCodeMarkdown b
    where
        indented x = length (takeWhile isSpace x) >= 4
findCodeMarkdown (x:xs) = map (Code . pure) (evens $ splitOn "`" x) ++ findCodeMarkdown xs
    where
        evens (_:x:xs) = x : evens xs
        evens _ = []
findCodeMarkdown [] = []


---------------------------------------------------------------------
-- RENDER THE CODE

showCode :: [Code] -> [String]
showCode = concat . zipWithFrom f 1 . nubOrd
    where
        f i (Code x) | "#" `isPrefixOf` concat x = []
                     | all whitelist x = []
                     | otherwise = showStmt i $ filter (not . isBlank . dropComment) $ fixCmd $ map undefDots x


fixCmd :: [String] -> [String]
fixCmd xs
    | all ("cmd_ " `isPrefixOf`) xs = xs ++ ["pure () :: IO () "]
    | otherwise = map (replace "Stdout out" "Stdout (out :: String)" . replace "Stderr err" "Stderr (err :: String)") xs

-- | Replace ... with undefined (don't use undefined with cmd; two ...'s should become one replacement)
undefDots :: String -> String
undefDots x | Just x <- stripSuffix "..." x, Just (x,_) <- stripInfix "..." x = x ++ new
            | otherwise = replace "..." new x
    where new = if words x `disjoint` ["cmd","cmd_","Development.Shake.cmd","Development.Shake.cmd_"] then "undefined" else "[\"\"]"

showStmt :: Int -> [String] -> [String]
showStmt _ [] = []
showStmt i xs | isDecl $ unlines xs = map f xs
    where f x = if fst (word1 x) `elem` dupes then "_" ++ show i ++ "_" ++ x else x
showStmt i [x] | filter isAlpha (fst $ word1 x) `elem` types = ["type Code_" ++ show i ++ " = " ++ x]
showStmt i [x] | length (words x) <= 2 = ["code_" ++ show i ++ " = (" ++ x ++ ")"] -- deal with operators and sections
showStmt i xs | all isPredicate xs, length xs > 1 =
    zipWithFrom (\j x -> "code_" ++ show i ++ "_" ++ show j ++ " = " ++ x) 1 xs
showStmt i xs = ("code_" ++ show i ++ " = do") : map ("  " ++) xs ++ ["  undefined" | isBindStmt $ last xs]

isPredicate :: String -> Bool
isPredicate x = not $ disjoint (words x) ["==","?=="]

isBindStmt :: String -> Bool
isBindStmt x = "let " `isPrefixOf` x || " <- " `isInfixOf` x

isDecl :: String -> Bool
isDecl x | fst (word1 x) `elem` ["import","infix","instance","newtype","data"] = True
isDecl (words -> name:"::":_) | all isAlphaNum name = True -- foo :: Type Signature
isDecl x | "=" `elem` takeWhile (`notElem` ["let","where"]) (words $ takeWhile (/= '{') x) = True -- foo arg1 arg2 = an implementation
isDecl _ = False


---------------------------------------------------------------------
-- TEXT MANIPULATION

-- | Is a string empty or whitespace
isBlank :: String -> Bool
isBlank = all isSpace

-- | If all lines are indented by at least n spaces, then trim n spaces from each line
unindent :: [String] -> [String]
unindent xs = map (drop n) xs
    where n = minimum $ 1000 : map (length . takeWhile (== ' ')) (filter (not . isBlank) xs)

-- | Remove line comments from the end of lines
dropComment :: String -> String
dropComment = fst . breakOn "--"

-- | Find all pieces of text inside a given tag
insideTag :: String -> String -> [String]
insideTag tag = map (fst . breakOn ("</" ++ tag ++ ">")) . drop1 . splitOn ("<" ++ tag ++ ">")

-- | Given some HTML, find the raw text
innerText :: String -> String
innerText ('<':xs) = innerText $ drop1 $ dropWhile (/= '>') xs
innerText ('&':xs)
    | Just xs <- stripPrefix "quot;" xs = '\"' : innerText xs
    | Just xs <- stripPrefix "lt;" xs = '<' : innerText xs
    | Just xs <- stripPrefix "gt;" xs = '>' : innerText xs
    | Just xs <- stripPrefix "amp;" xs = '&' : innerText xs
innerText (x:xs) = x : innerText xs
innerText [] = []


---------------------------------------------------------------------
-- DATA SECTION

-- | Only the following identifiers can appear in italic code blocks in Haddock
--   (otherwise it's a common markup mistake)
italics :: [String]
italics = words "command-name file-name N"

-- | Identifiers that indicate the fragment is a type
types :: [String]
types = words $
    "MVar IO String FilePath Maybe [String] FSATrace Char ExitCode ReaderT Change " ++
    "Action Resource Rebuild FilePattern Development.Shake.FilePattern " ++
    "Lint Verbosity Rules CmdOption Int Double " ++
    "NFData Binary Hashable Eq Typeable Show Applicative " ++
    "CmdResult ByteString ProcessHandle Rule Monad MonadFail Monoid Data TypeRep " ++
    "BuiltinRun BuiltinLint BuiltinCheck ShakeDatabase"

-- | Duplicated identifiers which require renaming
dupes :: [String]
dupes = words "main progressSimple rules"


isFilePath :: String -> Bool
isFilePath x = "C:\\" `isPrefixOf` x || (all validChar  x && ("foo/" `isPrefixOf` x || takeExtension x `elem` exts))
    where
        validChar x = isAlphaNum x || x `elem` "_./*"
        exts = words $ ".txt .hi .hs .o .exe .tar .cpp .cfg .dep .out .deps .m .h .c .html .zip " ++
                       ".js .json .trace .database .src .sh .bat .ninja .rot13 .version .digits .prof .md"

isCmdFlag :: String -> Bool
isCmdFlag "+RTS" = True
isCmdFlag x = length a `elem` [1,2] && all (\x -> isAlphaNum x || x `elem` "-=/_[]") b
    where (a,b) = span (== '-') x

isCmdFlags :: String -> Bool
isCmdFlags = all (\x -> let y = dropSuffix "," x in isCmdFlag y || isArg y) . words
    where isArg = all (\x -> isUpper x || x == '=')

isEnvVar :: String -> Bool
isEnvVar x | Just x <- stripPrefix "$" x = all validChar x
           | Just x <- stripPrefix "%" x, Just x <- stripSuffix "%" x = all validChar x
           | otherwise = False
    where validChar x = isAlpha x || x == '_'

isProgram :: String -> Bool
isProgram (words -> x:xs) = x `elem` programs && all (\x -> isCmdFlag x || isFilePath x || all isAlpha x || x == "&&") xs
    where programs = words "excel gcc docker cl make ghc ghci cabal distcc npm build tar git fsatrace ninja touch pwd runhaskell rot13 main shake stack rm cat sed sh apt-get build-multiple"
isProgram _ = False

-- | Should a fragment be whitelisted and not checked
whitelist :: String -> Bool
whitelist x | null x || isFilePath x || isCmdFlags x || isEnvVar x || isProgram x = True
whitelist x | elem x $ words $
    "newtype do a q m c x value key os contents clean _make " ++
    ".. /. // \\ //* dir/*/* dir [ " ++
    "ConstraintKinds TemplateHaskell ApplicativeDo OverloadedLists OverloadedStrings GeneralizedNewtypeDeriving DeriveDataTypeable TypeFamilies SetConsoleTitle " ++
    "Data.List System.Directory Development.Shake.FilePath run " ++
    "NoProgress Error src about://tracing " ++
    ".make/i586-linux-gcc/output build " ++
    "/usr/special /usr/special/userbinary " ++
    "Hidden extension xterm main opts result flagValues argValues fail " ++
    "HEADERS_DIR /path/to/dir CFLAGS let linkFlags temp code out err " ++
    "_shake _shake/build manual chrome://tracing/ compdb " ++
    "docs/manual foo.* _build _build/run depfile 0.000s " ++
    "@ndm_haskell file-name .PHONY filepath trim base stack extra #include " ++
    "*> BuiltinRun BuiltinLint BuiltinIdentity RuleResult " ++
    "oldStore mode node_modules llbuild Makefile " ++
    "RebuildNever RLIMIT_NOFILE "
    = True
whitelist x = x `elem`
    ["[Foo.hi, Foo.o]"
    ,"shake-progress"
    ,"type instance"
    ,"1m25s (15%)"
    ,"3m12s (82%)"
    ,"getPkgVersion $ GhcPkgVersion \"shake\""
    ,"ghc --make MyBuildSystem -threaded -rtsopts \"-with-rtsopts=-I0 -qg\""
    ,"# command-name (for file-name)"
    ,"<i>build rules</i>"
    ,"<i>actions</i>"
    ,"shakeFiles=\"_build\""
    ,"#include \""
    ,"pattern %> actions = (pattern ?==) ?> actions" -- because it overlaps
    ,"buildDir = \"_build\""
    ,"#!/bin/sh"
    ,"shake-build-system"
    ,"\"_build\" </> x -<.> \"o\""
    ,"[item1,item2,item2]"
    ,"$(LitE . StringL . loc_filename <$> location)"
    ,"-d[ FILE], --debug[=FILE]"
    ,"-r[ FILE], --report[=FILE], --profile[=FILE]"
    ,"man 2 getrlimit"
    ]

blacklist :: [String]
blacklist =
    -- from https://twitter.com/jesstelford/status/992756386160234497
    ["obviously"
    ,"basically"
    ,"simply"
    ,"of course"
    ,"clearly"
    ,"everyone knows"
    -- ,"however"
    -- ,"so,"
    -- ,"easy"
    ]
