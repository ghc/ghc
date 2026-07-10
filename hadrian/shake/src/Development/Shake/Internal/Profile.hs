{-# LANGUAGE PatternGuards, RecordWildCards #-}

module Development.Shake.Internal.Profile(writeProfile) where

import General.Template
import Data.Tuple.Extra
import Data.Function
import Data.List.Extra
import Data.Maybe
import System.FilePath
import System.IO.Extra
import Numeric.Extra
import General.Extra
import Development.Shake.Internal.Errors
import Development.Shake.Internal.Core.Types
import Development.Shake.Internal.Core.Database
import Development.Shake.Internal.Value
import qualified Data.HashSet as Set
import Development.Shake.Internal.Paths
import Development.Shake.Classes
import System.Time.Extra
import qualified Data.HashMap.Strict as Map
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Char8 as BS
import General.Intern(Id)


-- | Given a map of representing a dependency order (with a show for error messages), find an ordering for the items such
--   that no item points to an item before itself.
--   Raise an error if you end up with a cycle.
dependencyOrder :: (Eq a, Hashable a) => (a -> String) -> Map.HashMap a [a] -> [a]
-- Algorithm:
--    Divide everyone up into those who have no dependencies [Id]
--    And those who depend on a particular Id, Dep :-> Maybe [(Key,[Dep])]
--    Where d :-> Just (k, ds), k depends on firstly d, then remaining on ds
--    For each with no dependencies, add to list, then take its dep hole and
--    promote them either to Nothing (if ds == []) or into a new slot.
--    k :-> Nothing means the key has already been freed
dependencyOrder shw status = f (map fst noDeps) $ Map.map Just $ Map.fromListWith (++) [(d, [(k,ds)]) | (k,d:ds) <- hasDeps]
    where
        (noDeps, hasDeps) = partition (null . snd) $ Map.toList status

        f [] mp | null bad = []
                | otherwise = throwImpure $ errorInternal $ unlines $
                    "Internal invariant broken, database seems to be cyclic" :
                    map ("    " ++) bad ++
                    ["... plus " ++ show (length badOverflow) ++ " more ..." | not $ null badOverflow]
            where (bad,badOverflow) = splitAt 10 [shw i | (i, Just _) <- Map.toList mp]

        f (x:xs) mp = x : f (now++xs) later
            where Just free = Map.lookupDefault (Just []) x mp
                  (now,later) = foldl' g ([], Map.insert x Nothing mp) free

        g (free, mp) (k, []) = (k:free, mp)
        g (free, mp) (k, d:ds) = case Map.lookupDefault (Just []) d mp of
            Nothing -> g (free, mp) (k, ds)
            Just todo -> (free, Map.insert d (Just $ (k,ds) : todo) mp)


-- | Eliminate all errors from the database, pretending they don't exist
resultsOnly :: Map.HashMap Id (Key, Status) -> Map.HashMap Id (Key, Result (Either BS.ByteString Value))
resultsOnly mp = Map.map (\(k, v) -> (k, let Just r = getResult v in r{depends = map (Depends . filter (isJust . flip Map.lookup keep) . fromDepends) $ depends r})) keep
    where keep = Map.filter (isJust . getResult . snd) mp

removeStep :: Map.HashMap Id (Key, Result a) -> Map.HashMap Id (Key, Result a)
removeStep = Map.filter (\(k,_) -> k /= stepKey)

toReport :: Database -> IO [ProfileEntry]
toReport db = do
    status <- removeStep . resultsOnly <$> getKeyValuesFromId db
    let order = let shw i = maybe "<unknown>" (show . fst) $ Map.lookup i status
                in dependencyOrder shw $ Map.map (concatMap fromDepends . depends . snd) status
        ids = Map.fromList $ zip order [0..]

        steps = let xs = Set.toList $ Set.fromList $ concat [[changed, built] | (_,Result{..}) <- Map.elems status]
                in Map.fromList $ zip (sortBy (flip compare) xs) [0..]

        f (k, Result{..}) = ProfileEntry
            {prfName = show k
            ,prfBuilt = fromStep built
            ,prfChanged = fromStep changed
            ,prfDepends = filter (not . null) $ map (mapMaybe (`Map.lookup` ids) . fromDepends) depends
            ,prfExecution = floatToDouble execution
            ,prfTraces = map fromTrace $ sortOn traceStart traces
            }
            where fromStep i = fromJust $ Map.lookup i steps
                  fromTrace (Trace a b c) = ProfileTrace (BS.unpack a) (floatToDouble b) (floatToDouble c)
    pure [maybe (throwImpure $ errorInternal "toReport") f $ Map.lookup i status | i <- order]


data ProfileEntry = ProfileEntry
    {prfName :: String, prfBuilt :: Int, prfChanged :: Int, prfDepends :: [[Int]], prfExecution :: Double, prfTraces :: [ProfileTrace]}
data ProfileTrace = ProfileTrace
    {prfCommand :: String, prfStart :: Double, prfStop :: Double}
prfTime ProfileTrace{..} = prfStop - prfStart

-- | Generates an report given some build system profiling data.
writeProfile :: FilePath -> Database -> IO ()
writeProfile out db = writeProfileInternal out =<< toReport db

writeProfileInternal :: FilePath -> [ProfileEntry] -> IO ()
writeProfileInternal out xs
    | takeExtension out == ".js" = writeFileBinary out $ "var profile = \n" ++ generateJSON xs
    | takeExtension out == ".json" = writeFileBinary out $ generateJSON xs
    | takeExtension out == ".trace" = writeFileBinary out $ generateTrace xs
    | out == "-" = putStr $ unlines $ generateSummary xs
    -- NOTE: On my laptop writing 1.5Mb of profile report takes 0.6s.
    --       This is fundamentals of my laptop, not a Haskell profiling issue.
    --       Verified with similar "type foo > bar" commands taking similar time.
    | otherwise = LBS.writeFile out =<< generateHTML xs


generateSummary :: [ProfileEntry] -> [String]
generateSummary xs =
    ["* This database has tracked " ++ show (maximum (0 : map prfChanged xs) + 1) ++ " runs."
    ,let f = show . length in "* There are " ++ f xs ++ " rules (" ++ f ls ++ " rebuilt in the last run)."
    ,let f = show . sum . map (length . prfTraces) in "* Building required " ++ f xs ++ " traced commands (" ++ f ls ++ " in the last run)."
    ,"* The total (unparallelised) time is " ++ showDuration (sum $ map prfExecution xs) ++
        " of which " ++ showDuration (sum $ map prfTime $ concatMap prfTraces xs) ++ " is traced commands."
    ,let f xs = if null xs then "0s" else (\(a,b) -> showDuration a ++ " (" ++ b ++ ")") $ maximumBy' (compare `on` fst) xs in
        "* The longest rule takes " ++ f (map (prfExecution &&& prfName) xs) ++
        ", and the longest traced command takes " ++ f (map (prfTime &&& prfCommand) $ concatMap prfTraces xs) ++ "."
    ,let sumLast = sum $ map prfTime $ concatMap prfTraces ls
         maxStop = maximum $ 0 : map prfStop (concatMap prfTraces ls) in
        "* Last run gave an average parallelism of " ++ showDP 2 (if maxStop == 0 then 0 else sumLast / maxStop) ++
        " times over " ++ showDuration maxStop ++ "."
    ]
    where ls = filter ((==) 0 . prfBuilt) xs


generateHTML :: [ProfileEntry] -> IO LBS.ByteString
generateHTML xs = do
    report <- readDataFileHTML "profile.html"
    let f "data/profile-data.js" = pure $ LBS.pack $ "var profile =\n" ++ generateJSON xs
    runTemplate f report


generateTrace :: [ProfileEntry] -> String
generateTrace xs = jsonListLines $
    showEntries 0 [y{prfCommand=prfName x} | x <- onlyLast, y <- prfTraces x] ++
    showEntries 1 (concatMap prfTraces onlyLast)
    where
        onlyLast = filter (\x -> prfBuilt x == 0) xs
        showEntries pid xs = map (showEntry pid) $ snd $ mapAccumL alloc [] $ sortOn prfStart xs

        alloc :: [ProfileTrace] -> ProfileTrace -> ([ProfileTrace], (Int, ProfileTrace))
        -- FIXME: I don't really understand what this code is doing, or the invariants it ensures
        alloc as r | (a1,_:a2) <- break (\a -> prfStop a <= prfStart r) as = (a1++r:a2, (length a1,r))
                   | otherwise = (as++[r], (length as,r))

        showEntry pid (tid, ProfileTrace{..}) = jsonObject
            [("args","{}"), ("ph",show "X"), ("cat",show "target")
            ,("name",show prfCommand), ("tid",show tid), ("pid",show pid)
            ,("ts",show $ 1000000*prfStart), ("dur",show $ 1000000*(prfStop-prfStart))]


generateJSON :: [ProfileEntry] -> String
generateJSON = jsonListLines . map showEntry
    where
        showEntry ProfileEntry{..} = jsonList $
            [show prfName
            ,showTime prfExecution
            ,show prfBuilt
            ,show prfChanged] ++
            [show prfDepends | not (null prfDepends) || not (null prfTraces)] ++
            [jsonList $ map showTrace prfTraces | not (null prfTraces)]
        showTrace ProfileTrace{..} = jsonList
            [show prfCommand, showTime prfStart, showTime prfStop]
        showTime x = if '.' `elem` y then dropWhileEnd (== '.') $ dropWhileEnd (== '0') y else y
            where y = showDP 4 x

jsonListLines xs = "[" ++ intercalate "\n," xs ++ "\n]"
jsonList xs = "[" ++ intercalate "," xs ++ "]"
jsonObject xs = "{" ++ intercalate "," [show a ++ ":" ++ b | (a,b) <- xs] ++ "}"
