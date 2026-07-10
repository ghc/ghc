{-# LANGUAGE RecordWildCards, ScopedTypeVariables, TupleSections #-}

module Development.Shake.Internal.History.Shared(
    Shared, newShared,
    addShared, lookupShared,
    removeShared, listShared,
    sanityShared
    ) where

import Control.Exception
import Development.Shake.Internal.Value
import Development.Shake.Internal.History.Types
import Development.Shake.Internal.History.Symlink
import Development.Shake.Internal.Core.Database
import Development.Shake.Classes
import General.Binary
import General.Extra
import Data.List
import Control.Monad.Extra
import System.Directory.Extra
import System.FilePath
import System.IO.Extra
import Numeric
import Development.Shake.Internal.FileInfo
import General.Wait
import Development.Shake.Internal.FileName
import Data.Monoid
import Control.Monad.IO.Class
import Data.Maybe
import qualified Data.ByteString as BS
import Prelude


data Shared = Shared
    {globalVersion :: !Ver
    ,keyOp :: BinaryOp Key
    ,sharedRoot :: FilePath
    ,useSymlink :: Bool
    }

newShared :: Bool -> BinaryOp Key -> Ver -> FilePath -> IO Shared
newShared useSymlink keyOp globalVersion sharedRoot = pure Shared{..}


data Entry = Entry
    {entryKey :: Key
    ,entryGlobalVersion :: !Ver
    ,entryBuiltinVersion :: !Ver
    ,entryUserVersion :: !Ver
    ,entryDepends :: [[(Key, BS_Identity)]]
    ,entryResult :: BS_Store
    ,entryFiles :: [(FilePath, FileHash)]
    } deriving (Show, Eq)

putEntry :: BinaryOp Key -> Entry -> Builder
putEntry binop Entry{..} =
    putExStorable entryGlobalVersion <>
    putExStorable entryBuiltinVersion <>
    putExStorable entryUserVersion <>
    putExN (putOp binop entryKey) <>
    putExN (putExList $ map (putExList . map putDepend) entryDepends) <>
    putExN (putExList $ map putFile entryFiles) <>
    putEx entryResult
    where
        putDepend (a,b) = putExN (putOp binop a) <> putEx b
        putFile (a,b) = putExStorable b <> putEx a

getEntry :: BinaryOp Key -> BS.ByteString -> Entry
getEntry binop x
    | (x1, x2, x3, x) <- binarySplit3 x
    , (x4, x) <- getExN x
    , (x5, x) <- getExN x
    , (x6, x7) <- getExN x
    = Entry
        {entryGlobalVersion = x1
        ,entryBuiltinVersion = x2
        ,entryUserVersion = x3
        ,entryKey = getOp binop x4
        ,entryDepends = map (map getDepend . getExList) $ getExList x5
        ,entryFiles = map getFile $ getExList x6
        ,entryResult = getEx x7
        }
    where
        getDepend x | (a, b) <- getExN x = (getOp binop a, getEx b)
        getFile x | (b, a) <- binarySplit x = (getEx a, b)

hexed x = showHex (abs $ hash x) ""

-- | The path under which everything relating to a Key lives
sharedFileDir :: Shared -> Key -> FilePath
sharedFileDir shared key = sharedRoot shared </> ".shake.cache" </> hexed key

-- | The list of files containing Entry values, given a result of 'sharedFileDir'
sharedFileKeys :: FilePath -> IO [FilePath]
sharedFileKeys dir = do
    b <- doesDirectoryExist_ $ dir </> "_key"
    if not b then pure [] else listFiles $ dir </> "_key"

loadSharedEntry :: Shared -> Key -> Ver -> Ver -> IO [IO (Maybe Entry)]
loadSharedEntry shared@Shared{..} key builtinVersion userVersion =
    map f <$> sharedFileKeys (sharedFileDir shared key)
    where
        f file = do
            e@Entry{..} <- getEntry keyOp <$> BS.readFile file
            let valid = entryKey == key && entryGlobalVersion == globalVersion && entryBuiltinVersion == builtinVersion && entryUserVersion == userVersion
            pure $ if valid then Just e else Nothing


-- | Given a way to get the identity, see if you can find a stored cloud version
lookupShared :: Shared -> (Key -> Wait Locked (Maybe BS_Identity)) -> Key -> Ver -> Ver -> Wait Locked (Maybe (BS_Store, [[Key]], IO ()))
lookupShared shared ask key builtinVersion userVersion = do
    ents <- liftIO $ loadSharedEntry shared key builtinVersion userVersion
    flip firstJustWaitUnordered ents $ \act -> do
        me <- liftIO act
        case me of
            Nothing -> pure Nothing
            Just Entry{..} -> do
                -- use Nothing to indicate success, Just () to bail out early on mismatch
                let result x = if isJust x then Nothing else Just $ (entryResult, map (map fst) entryDepends, ) $ do
                        let dir = sharedFileDir shared entryKey
                        forM_ entryFiles $ \(file, hash) ->
                            copyFileLink (useSymlink shared) (dir </> show hash) file
                result <$> firstJustM id
                    [ firstJustWaitUnordered id
                        [ test <$> ask k | (k, i1) <- kis
                        , let test = maybe (Just ()) (\i2 -> if i1 == i2 then Nothing else Just ())]
                    | kis <- entryDepends]


saveSharedEntry :: Shared -> Entry -> IO ()
saveSharedEntry shared entry = do
    let dir = sharedFileDir shared (entryKey entry)
    createDirectoryRecursive dir
    forM_ (entryFiles entry) $ \(file, hash) ->
        unlessM (doesFileExist_ $ dir </> show hash) $
            copyFileLink (useSymlink shared) file (dir </> show hash)
    -- Write key after files to make sure cache is always useable
    let v = runBuilder $ putEntry (keyOp shared) entry
    let dirName = dir </> "_key"
    createDirectoryRecursive dirName
    -- #757, make sure we write this file atomically
    (tempFile, cleanUp) <- newTempFileWithin dir
    (BS.writeFile tempFile v >> renameFile tempFile (dirName </> hexed v)) `onException` cleanUp


addShared :: Shared -> Key -> Ver -> Ver -> [[(Key, BS_Identity)]] -> BS_Store -> [FilePath] -> IO ()
addShared shared entryKey entryBuiltinVersion entryUserVersion entryDepends entryResult files = do
    files <- mapM (\x -> (x,) <$> getFileHash (fileNameFromString x)) files
    saveSharedEntry shared Entry{entryFiles = files, entryGlobalVersion = globalVersion shared, ..}

removeShared :: Shared -> (Key -> Bool) -> IO ()
removeShared Shared{..} test = do
    dirs <- listDirectories $ sharedRoot </> ".shake.cache"
    deleted <- forM dirs $ \dir -> do
        files <- sharedFileKeys dir
        -- if any key matches, clean them all out
        b <- flip anyM files $ \file -> handleSynchronous (\e -> putStrLn ("Warning: " ++ show e) >> pure False) $
            evaluate . test . entryKey . getEntry keyOp =<< BS.readFile file
        when b $ removePathForcibly dir
        pure b
    liftIO $ putStrLn $ "Deleted " ++ show (length (filter id deleted)) ++ " entries"

listShared :: Shared -> IO ()
listShared Shared{..} = do
    dirs <- listDirectories $ sharedRoot </> ".shake.cache"
    forM_ dirs $ \dir -> do
        putStrLn $ "Directory: " ++ dir
        keys <- sharedFileKeys dir
        forM_ keys $ \key ->
            handleSynchronous (\e -> putStrLn $ "Warning: " ++ show e) $ do
                Entry{..} <- getEntry keyOp <$> BS.readFile key
                putStrLn $ "  Key: " ++ show entryKey
                forM_ entryFiles $ \(file,_) ->
                    putStrLn $ "    File: " ++ file

sanityShared :: Shared -> IO ()
sanityShared Shared{..} = do
    dirs <- listDirectories $ sharedRoot </> ".shake.cache"
    forM_ dirs $ \dir -> do
        putStrLn $ "Directory: " ++ dir
        keys <- sharedFileKeys dir
        forM_ keys $ \key ->
            handleSynchronous (\e -> putStrLn $ "Warning: " ++ show e) $ do
                Entry{..} <- getEntry keyOp <$> BS.readFile key
                putStrLn $ "  Key: " ++ show entryKey
                putStrLn $ "  Key file: " ++ key
                forM_ entryFiles $ \(file,hash) ->
                    checkFile file dir hash
    where
      checkFile filename dir keyHash = do
          let cachefile = dir </> show keyHash
          putStrLn $ "    File: " ++ filename
          putStrLn $ "    Cache file: " ++ cachefile
          ifM (not <$> doesFileExist_ cachefile)
              (putStrLn "      Error: cache file does not exist") $
              ifM ((/= keyHash) <$> getFileHash (fileNameFromString cachefile))
                  (putStrLn "      Error: cache file hash does not match stored hash")
                  (putStrLn "      OK")
