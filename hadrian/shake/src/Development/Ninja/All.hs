{-# LANGUAGE RecordWildCards, ViewPatterns, ScopedTypeVariables, TupleSections #-}

module Development.Ninja.All(runNinja) where

import Development.Ninja.Env
import Development.Ninja.Type
import Development.Ninja.Parse
import Development.Shake hiding (addEnv)
import Development.Shake.Classes
import qualified Data.ByteString as BS8
import qualified Data.ByteString.Char8 as BS

import System.Directory
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import Data.Tuple.Extra
import Control.Exception.Extra
import Control.Monad
import Data.Maybe
import Data.Char
import Data.List.Extra
import System.Info.Extra

-- Internal imports
import General.Extra(removeFile_, headErr)
import General.Timing(addTiming)
import General.Makefile(parseMakefile)
import Development.Shake.Internal.FileName(filepathNormalise, fileNameFromString)
import Development.Shake.Internal.FileInfo(getFileInfo)
import Development.Shake.Internal.Errors(throwM, errorStructured)
import Development.Shake.Internal.Rules.File(needBS, neededBS)
import Development.Shake.Internal.Rules.OrderOnly(orderOnlyBS)


-- | Given the Ninja source file, a list of file arguments, a tool name.
--   Return a bool if you should restart and the rules.
runNinja :: IO () -> FilePath -> [String] -> Maybe String -> IO (Maybe (Rules ()))
runNinja _ file args (Just "compdb") = do
    dir <- getCurrentDirectory
    Ninja{..} <- parse file =<< newEnv
    rules<- pure $ Map.fromList [r | r <- rules, BS.unpack (fst r) `elem` args]
    -- the build items are generated in reverse order, hence the reverse
    let xs = [(a,b,file,rule) | (a,b@Build{..}) <- reverse $ multiples ++ map (first pure) singles
                              , Just rule <- [Map.lookup ruleName rules], file:_ <- [depsNormal]]
    xs <- forM xs $ \(out,Build{..},file,Rule{..}) -> do
        -- the order of adding new environment variables matters
        env <- scopeEnv env
        addEnv env (BS.pack "out") (BS.unwords $ map quote out)
        addEnv env (BS.pack "in") (BS.unwords $ map quote depsNormal)
        addEnv env (BS.pack "in_newline") (BS.unlines depsNormal)
        forM_ buildBind $ \(a,b) -> addEnv env a b
        addBinds env ruleBind
        commandline <- fmap BS.unpack $ askVar env $ BS.pack "command"
        pure $ CompDb dir commandline $ BS.unpack file
    putStr $ printCompDb xs
    pure Nothing

runNinja _ _ _ (Just x) = errorIO $ "Unknown tool argument, expected 'compdb', got " ++ x

runNinja restart file args Nothing = do
    addTiming "Ninja parse"
    ninja@Ninja{..} <- parse file =<< newEnv
    pure $ Just $ do
        phonys<- pure $ Map.fromList phonys
        needDeps<- pure $ needDeps ninja phonys -- partial application
        singles<- pure $ Map.fromList $ map (first filepathNormalise) singles
        multiples<- pure $ Map.fromList [(x,(xs,b)) | (xs,b) <- map (first $ map filepathNormalise) multiples, x <- xs]
        rules<- pure $ Map.fromList rules
        pools <- fmap Map.fromList $ forM ((BS.pack "console",1):pools) $ \(name,depth) ->
            (name,) <$> newResource (BS.unpack name) depth

        action $ do
            -- build the .ninja files, if they change, restart the build
            before <- liftIO $ mapM (getFileInfo False . fileNameFromString) sources
            need sources
            after <- liftIO $ mapM (getFileInfo False . fileNameFromString) sources
            if before /= after then
                runAfter restart
             else
                needBS $ concatMap (resolvePhony phonys) $
                    if not $ null args then map BS.pack args
                    else if not $ null defaults then defaults
                    else Map.keys singles ++ Map.keys multiples

        (\x -> map BS.unpack . fst <$> Map.lookup (BS.pack x) multiples) &?> \out -> let out2 = map BS.pack out in
            build needDeps phonys rules pools out2 $ snd $ multiples Map.! headErr out2

        (flip Map.member singles . BS.pack) ?> \out -> let out2 = BS.pack out in
            build needDeps phonys rules pools [out2] $ singles Map.! out2


resolvePhony :: Map.HashMap Str [Str] -> Str -> [Str]
resolvePhony mp = f $ Left 100
    where
        f (Left 0) x = f (Right []) x
        f (Right xs) x | x `elem` xs = error $ "Ninja recursive phony involving " ++ BS.unpack x
        f a x = case Map.lookup x mp of
            Nothing -> [x]
            Just xs -> concatMap (f $ either (Left . subtract 1) (Right . (x:)) a) xs


quote :: Str -> Str
quote x | BS.any isSpace x = let q = BS.singleton '\"' in BS.concat [q,x,q]
        | otherwise = x


build :: (Build -> [Str] -> Action ()) -> Map.HashMap Str [Str] -> Map.HashMap Str Rule -> Map.HashMap Str Resource -> [Str] -> Build -> Action ()
build needDeps phonys rules pools out build@Build{..} = do
    needBS $ concatMap (resolvePhony phonys) $ depsNormal ++ depsImplicit
    orderOnlyBS $ concatMap (resolvePhony phonys) depsOrderOnly
    case Map.lookup ruleName rules of
        Nothing -> liftIO $ errorIO $ "Ninja rule named " ++ BS.unpack ruleName ++ " is missing, required to build " ++ BS.unpack (BS.unwords out)
        Just Rule{..} -> do
            env <- liftIO $ scopeEnv env
            liftIO $ do
                -- the order of adding new environment variables matters
                addEnv env (BS.pack "out") (BS.unwords $ map quote out)
                addEnv env (BS.pack "in") (BS.unwords $ map quote depsNormal)
                addEnv env (BS.pack "in_newline") (BS.unlines depsNormal)
                forM_ buildBind $ \(a,b) -> addEnv env a b
                addBinds env ruleBind

            applyRspfile env $ do
                commandline <- liftIO $ fmap BS.unpack $ askVar env $ BS.pack "command"
                depfile <- liftIO $ fmap BS.unpack $ askVar env $ BS.pack "depfile"
                deps <- liftIO $ fmap BS.unpack $ askVar env $ BS.pack "deps"
                description <- liftIO $ fmap BS.unpack $ askVar env $ BS.pack "description"
                pool <- liftIO $ askVar env $ BS.pack "pool"

                let withPool act = case Map.lookup pool pools of
                        _ | BS.null pool -> act
                        Nothing -> liftIO $ errorIO $ "Ninja pool named " ++ BS.unpack pool ++ " not found, required to build " ++ BS.unpack (BS.unwords out)
                        Just r -> withResource r 1 act

                when (description /= "") $ putInfo description
                let (cmdOpts, cmdProg, cmdArgs) = toCommand commandline
                if deps == "msvc" then do
                    Stdout stdout <- withPool $ command cmdOpts cmdProg cmdArgs
                    prefix <- liftIO $ fmap (fromMaybe $ BS.pack "Note: including file: ") $
                                       askEnv env $ BS.pack "msvc_deps_prefix"
                    needDeps build $ parseShowIncludes prefix $ BS.pack stdout
                 else
                    withPool $ command_ cmdOpts cmdProg cmdArgs
                when (depfile /= "") $ do
                    when (deps /= "gcc") $ need [depfile]
                    depsrc <- liftIO $ BS.readFile depfile
                    needDeps build $ concatMap snd $ parseMakefile depsrc
                    -- correct as per the Ninja spec, but breaks --skip-commands
                    -- when (deps == "gcc") $ liftIO $ removeFile depfile


needDeps :: Ninja -> Map.HashMap Str [Str] -> Build -> [Str] -> Action ()
needDeps Ninja{..} phonysMp = \build xs -> do -- eta reduced so 'builds' is shared
    opts <- getShakeOptions
    if isNothing $ shakeLint opts then needBS xs else do
        neededBS xs
        -- now try and statically validate needed will never fail
        -- first find which dependencies are generated files
        xs<- pure $ filter (`Map.member` builds) xs
        -- now try and find them as dependencies
        -- performance note: allDependencies generates lazily, and difference consumes lazily,
        -- with the property that in the common case it won't generate much of the list at all
        let bad = xs `difference` allDependencies build
        case bad of
            [] -> pure ()
            xs -> throwM $ errorStructured
                ("Lint checking error - " ++
                    (if length xs == 1 then "file in deps is" else "files in deps are") ++
                    " generated and not a pre-dependency")
                [("File", Just $ BS.unpack x) | x <- xs]
                ""
    where
        builds :: Map.HashMap FileStr Build
        builds = Map.fromList $ singles ++ [(x,y) | (xs,y) <- multiples, x <- xs]

        -- do list difference, assuming a small initial set, most of which occurs early in the list
        difference :: (Eq a, Hashable a) => [a] -> [a] -> [a]
        difference [] _ = []
        difference xs ys = f (Set.fromList xs) ys
            where
                f xs [] = Set.toList xs
                f xs (y:ys)
                    | y `Set.member` xs = let xs2 = Set.delete y xs in if Set.null xs2 then [] else f xs2 ys
                    | otherwise = f xs ys

        -- find all dependencies of a rule, no duplicates, with all dependencies of this rule listed first
        allDependencies :: Build -> [FileStr]
        allDependencies rule = f Set.empty [] [rule]
            where
                f _ [] [] = []
                f seen [] (x:xs) = f seen (map filepathNormalise $ concatMap (resolvePhony phonysMp) $ depsNormal x ++ depsImplicit x ++ depsOrderOnly x) xs
                f seen (x:xs) rest | x `Set.member` seen = f seen xs rest
                                   | otherwise = x : f (Set.insert x seen) xs (maybeToList (Map.lookup x builds) ++ rest)


applyRspfile :: Env Str Str -> Action a -> Action a
applyRspfile env act = do
    rspfile <- liftIO $ fmap BS.unpack $ askVar env $ BS.pack "rspfile"
    rspfile_content <- liftIO $ askVar env $ BS.pack "rspfile_content"
    if rspfile == "" then
        act
     else
        flip actionFinally (removeFile_ rspfile) $ do
            liftIO $ BS.writeFile rspfile rspfile_content
            act


parseShowIncludes :: Str -> Str -> [FileStr]
parseShowIncludes prefix out =
    [y | x <- BS.lines out, prefix `BS.isPrefixOf` x
       , let y = BS.dropWhile isSpace $ BS.drop (BS.length prefix) x
       , not $ isSystemInclude y]

-- Dodgy, but ported over from the original Ninja
isSystemInclude :: FileStr -> Bool
isSystemInclude x = bsProgFiles `BS.isInfixOf` tx || bsVisStudio `BS.isInfixOf` tx
    where tx = BS8.map (\c -> if c >= 97 then c - 32 else c) x
               -- optimised toUpper that only cares about letters and spaces

bsProgFiles = BS.pack "PROGRAM FILES"
bsVisStudio = BS.pack "MICROSOFT VISUAL STUDIO"


data CompDb = CompDb
    {cdbDirectory :: String
    ,cdbCommand :: String
    ,cdbFile :: String
    }
    deriving Show

printCompDb :: [CompDb] -> String
printCompDb xs = unlines $ ["["] ++ concat (zipWithFrom f 1 xs) ++ ["]"]
    where
        n = length xs
        f i CompDb{..} =
            ["  {"
            ,"    \"directory\": " ++ g cdbDirectory ++ ","
            ,"    \"command\": " ++ g cdbCommand ++ ","
            ,"    \"file\": " ++ g cdbFile
            ,"  }" ++ (if i == n then "" else ",")]
        g = show


toCommand :: String -> ([CmdOption], String, [String])
toCommand s
    -- On POSIX, Ninja does a /bin/sh -c, and so does Haskell in Shell mode (easy).
    | not isWindows = ([Shell], s, [])
    -- On Windows, Ninja passes the string directly to CreateProcess,
    -- but Haskell applies some escaping first.
    -- We try and get back as close to the original as we can, but it's very hacky
    | length s < 8000 =
        -- Using the "cmd" program adds overhead (I measure 7ms), and a limit of 8191 characters,
        -- but is the most robust, requiring no additional escaping.
        ([Shell], s, [])
    | (cmd,s) <- word1 s, upper cmd `elem` ["CMD","CMD.EXE"], ("/c",s) <- word1 s =
        -- Given "cmd.exe /c <something>" we translate to Shell, which adds cmd.exe
        -- (looked up on the current path) and /c to the front. CMake uses this rule a lot.
        -- Adding quotes around pieces are /c goes very wrong.
        ([Shell], s, [])
    | otherwise =
        -- It's a long command line which doesn't call "cmd /c". We reverse the escaping
        -- Haskell applies, but each argument will still gain quotes around it.
        let xs = splitArgs s in ([], headDef "" xs, drop1 xs)


data State
    = Gap -- ^ Current in the gap between words
    | Word -- ^ Currently inside a space-separated argument
    | Quot -- ^ Currently inside a quote-surrounded argument

-- | The process package contains a translate function, reproduced below. The aim is that after command line
--   parsing we should get out mostly the same answer.
splitArgs :: String -> [String]
splitArgs = f Gap
    where
        f Gap (x:xs) | isSpace x = f Gap xs
        f Gap ('\"':xs) = f Quot xs
        f Gap [] = []
        f Gap xs = f Word xs
        f Word (x:xs) | isSpace x = [] : f Gap xs
        f Quot ('\"':xs) = [] : f Gap xs
        f s ('\\':xs) | (length -> a, b) <- span (== '\\') xs = case b of
            '\"':xs | even a -> add (replicate (a `div` 2) '\\' ++ "\"") $ f s xs
                    | otherwise -> add (replicate ((a+1) `div` 2) '\\') $ f s ('\"':xs)
            xs -> add (replicate (a+1) '\\') $ f s xs
        f s (x:xs) = add [x] $ f s xs
        f _ [] = [[]]

        add a (b:c) = (a++b):c
        add a [] = [a]

{-
translate (cmd,args) = unwords $ f cmd : map f args
    where
        f x = '"' : snd (foldr escape (True,"\"") xs)
        escape '"'  (_,     str) = (True,  '\\' : '"'  : str)
        escape '\\' (True,  str) = (True,  '\\' : '\\' : str)
        escape c    (_,     str) = (False, c : str)
-}
