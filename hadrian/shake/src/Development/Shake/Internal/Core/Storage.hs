{-# LANGUAGE ScopedTypeVariables, RecordWildCards, FlexibleInstances #-}
{-# LANGUAGE BangPatterns, GeneralizedNewtypeDeriving #-}
{-
This module stores the meta-data so its very important its always accurate
We can't rely on getting any exceptions or termination at the end, so we'd better write out a journal
We store a series of records, and if they contain twice as many records as needed, we compact
-}

module Development.Shake.Internal.Core.Storage(
    usingStorage
    ) where

import General.Chunks
import General.Cleanup
import General.Binary
import General.Intern
import Development.Shake.Internal.Options
import Development.Shake.Internal.Errors
import General.Timing
import General.FileLock
import qualified General.Ids as Ids

import Control.Exception.Extra
import Control.Monad.Extra
import Data.Monoid
import Data.Either.Extra
import Data.Time
import Data.Char
import Data.Word
import System.Info
import Development.Shake.Classes
import Numeric
import General.Extra
import Data.List.Extra
import Data.Maybe
import System.FilePath
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.HashMap.Strict as Map

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString as BS8
import Prelude


-- Increment every time the on-disk format/semantics change,
-- @x@ is for the users version number
databaseVersion :: String -> String
-- THINGS I WANT TO DO ON THE NEXT CHANGE
-- * Change filepaths to store a 1 byte prefix saying 8bit ASCII or UTF8
-- * Duration and Time should be stored as number of 1/10000th seconds Int32
databaseVersion x = "SHAKE-DATABASE-14-" ++ os ++ "-" ++ arch ++ "-" ++  s ++ "\r\n"
    where s = tailErr $ init $ show x -- call show, then take off the leading/trailing quotes
                                      -- ensures we do not get \r or \n in the user portion


messageCorrupt :: FilePath -> SomeException -> IO [String]
messageCorrupt dbfile err = do
    msg <- showException err
    pure $
        ("Error when reading Shake database " ++ dbfile) :
        map ("  "++) (lines msg) ++
        ["All files will be rebuilt"]


messageDatabaseVersionChange :: FilePath -> BS.ByteString -> BS.ByteString -> [String]
messageDatabaseVersionChange dbfile old new =
    ["Shake database version changed (either shake library version, or shakeVersion):"
    ,"  File:         " ++ dbfile
    ,"  Old version:  " ++ disp (limit $ BS.unpack old)
    ,"  New version:  " ++ disp (BS.unpack new)
    ,"All rules will be rebuilt"]
    where
        limit x = let (a,b) = splitAt 200 x in a ++ (if null b then "" else "...")
        disp = map (\x -> if isPrint x && isAscii x then x else '?') . takeWhile (`notElem` ("\r\n" :: String))


messageMissingTypes :: FilePath -> [String] -> [String]
messageMissingTypes dbfile types =
    ["Shake database rules have changed for the following types:"
    ,"  File:  " ++ dbfile] ++
    ["  Type:  " ++ x | x <- types] ++
    ["All rules using these types will be rebuilt"]


-- | Storage of heterogeneous things. In the particular case of Shake,
--   k ~ QTypeRep, v ~ (Key, Status{Value}).
--
--   The storage starts with a witness table saying what can be contained.
--   If any entries in the witness table don't have a current Witness then a fake
--   error witness is manufactured. If the witness ever changes the entire DB is
--   rewritten.
usingStorage
    :: (Show k, Eq k, Hashable k, NFData k, Show v, NFData v)
    => Cleanup
    -> ShakeOptions                                    -- ^ Storage options
    -> (IO String -> IO ())                            -- ^ Logging function
    -> Map.HashMap k (Ver, BinaryOp v)                 -- ^ Witnesses
    -> IO (Ids.Ids v, k -> Id -> v -> IO ())
usingStorage _ ShakeOptions{..} diagnostic _ | shakeFiles == "/dev/null" = do
    diagnostic $ pure "Using in-memory database"
    ids <- Ids.empty
    pure (ids, \_ _ _ -> pure ())

usingStorage cleanup ShakeOptions{..} diagnostic witness = do
    let lockFile = shakeFiles </> ".shake.lock"
    diagnostic $ pure $ "Before usingLockFile on " ++ lockFile
    usingLockFile cleanup lockFile
    diagnostic $ pure "After usingLockFile"

    let dbfile = shakeFiles </> ".shake.database"
    createDirectoryRecursive shakeFiles

    -- complete a partially failed compress
    whenM (restoreChunksBackup dbfile) $ do
        unexpected "Backup file exists, restoring over the previous file\n"
        diagnostic $ pure "Backup file move to original"

    addTiming "Database read"
    h <- usingChunks cleanup dbfile shakeFlush
    let corrupt
            | not shakeStorageLog = resetChunksCorrupt Nothing h
            | otherwise = do
                let file = dbfile <.> "corrupt"
                resetChunksCorrupt (Just file) h
                unexpected $ "Backup of corrupted file stored at " ++ file ++ "\n"

    -- check the version information matches
    let ver = BS.pack $ databaseVersion shakeVersion
    oldVer <- readChunkMax h $ fromIntegral $ BS.length ver + 100000
    let verEq = Right ver == oldVer
    when (not shakeVersionIgnore && not verEq && oldVer /= Left BS.empty) $ do
        outputErr $ messageDatabaseVersionChange dbfile (fromEither oldVer) ver
        corrupt

    (!witnessNew, !save) <- evaluate $ saveWitness witness
    witnessOld <- readChunk h
    ids <- case witnessOld of
        Left _ -> do
            resetChunksCorrupt Nothing h
            pure Nothing
        Right witnessOld ->  handleBool (not . isAsyncException) (\err -> do
            outputErr =<< messageCorrupt dbfile err
            corrupt
            pure Nothing) $ do

            (!missing, !load) <- evaluate $ loadWitness witness witnessOld
            when (missing /= []) $ outputErr $ messageMissingTypes dbfile missing
            ids <- Ids.empty
            let raw bs = "[len " ++ show (BS.length bs) ++ "] " ++ concat
                            [['0' | length c == 1] ++ c | x <- BS8.unpack bs, let c = showHex x ""]
            let go !i = do
                    v <- readChunk h
                    case v of
                        Left e -> do
                            let slop = fromIntegral $ BS.length e
                            when (slop > 0) $ unexpected $ "Last " ++ show slop ++ " bytes do not form a whole record\n"
                            diagnostic $ pure $ "Read " ++ show i ++ " chunks, plus " ++ show slop ++ " slop"
                            pure i
                        Right bs | (id, Just (k,v)) <- load bs -> do
                            evaluate $ rnf k
                            evaluate $ rnf v
                            Ids.insert ids id (k,v)
                            diagnostic $ do
                                let pretty (Left x) = "FAILURE: " ++ show x
                                    pretty (Right x) = x
                                x2 <- try_ $ evaluate $ let s = show v in rnf s `seq` s
                                pure $ "Chunk " ++ show i ++ " " ++ raw bs ++ " " ++ show id ++ " = " ++ pretty x2
                            go $ i+1
                        Right bs -> do
                            diagnostic $ pure $ "Chunk " ++ show i ++ " " ++ raw bs ++ " UNKNOWN WITNESS"
                            go i
            countItems <- go 0
            countDistinct <- Ids.sizeUpperBound ids
            diagnostic $ pure $ "Found at most " ++ show countDistinct ++ " distinct entries out of " ++ show countItems

            when (countItems > countDistinct*2 || not verEq || witnessOld /= witnessNew) $ do
                addTiming "Database compression"
                resetChunksCompact h $ \out -> do
                    out $ putEx ver
                    out $ putEx witnessNew
                    Ids.forWithKeyM_ ids $ \i (k,v) -> out $ save k i v
            Just <$> Ids.forCopy ids snd

    ids <- case ids of
        Just ids -> pure ids
        Nothing -> do
            writeChunk h $ putEx ver
            writeChunk h $ putEx witnessNew
            Ids.empty

    addTiming "With database"
    out <- usingWriteChunks cleanup h
    pure (ids, \k i v -> out $ save k i v)
    where
        unexpected x = when shakeStorageLog $ do
            t <- getCurrentTime
            appendFile (shakeFiles </> ".shake.storage.log") $ "\n[" ++ show t ++ "]: " ++ trimEnd x ++ "\n"
        outputErr x = do
            when (shakeVerbosity >= Warn) $ shakeOutput Warn $ unlines x
            unexpected $ unlines x


-- | A list oft witnesses, saved
type Witnesses = BS.ByteString

-- | The version and key, serialised
newtype Witness = Witness BS.ByteString
    deriving (Eq, Hashable, Ord)

toWitness :: Show k => Ver -> k -> Witness
toWitness (Ver v) k = Witness $ UTF8.fromString (show k ++ (if v == 0 then "" else ", v" ++ show v))

instance BinaryEx [Witness] where
    putEx xs = putEx [x | Witness x <- xs]
    getEx = map Witness . getEx


-- | Given the current witness table, and the serialised one from last time, return
--   (witnesses that got removed, way to deserialise an entry into an Id, and (if the witness remains) the key and value)
loadWitness :: forall k v . Show k => Map.HashMap k (Ver, BinaryOp v) -> Witnesses -> ([String], BS.ByteString -> (Id, Maybe (k, v)))
loadWitness mp bs = (,) missing $ seq ind $ \bs ->
            let (wInd :: Word16, i :: Id, bs2) = binarySplit2 bs
            in case ind (fromIntegral wInd) of
                    Nothing -> throwImpure $ errorInternal $ "Witness index out of bounds, " ++ show wInd
                    Just f -> (i, f bs2)
    where
        ws :: [Witness] = getEx bs
        missing = [UTF8.toString w | (i, Witness w) <- zipFrom 0 ws, isNothing $ fromJust (ind i) BS.empty]

        mp2 :: Map.HashMap Witness (k, BinaryOp v) = Map.fromList [(toWitness ver k, (k, bin)) | (k,(ver,bin)) <- Map.toList mp]

        ind :: (Int -> Maybe (BS.ByteString -> Maybe (k, v))) = seq mp2 $ fastAt $ flip map ws $ \w ->
            case Map.lookup w mp2 of
                Nothing -> const Nothing
                Just (k, BinaryOp{..}) -> \bs -> Just (k, getOp bs)


saveWitness :: forall k v . (Eq k, Hashable k, Show k) => Map.HashMap k (Ver, BinaryOp v) -> (Witnesses, k -> Id -> v -> Builder)
saveWitness mp
    | Map.size mp > fromIntegral (maxBound :: Word16) = throwImpure $ errorInternal $ "Number of distinct witness types exceeds limit, got " ++ show (Map.size mp)
    | otherwise = (runBuilder $ putEx ws
                  ,mpSave `seq` \k -> fromMaybe (throwImpure $ errorInternal $ "Don't know how to save, " ++ show k) $ Map.lookup k mpSave)
    where
        -- the entries in the witness table (in a stable order, to make it more likely to get a good equality)
        ws :: [Witness] = sort $ map (\(k,(ver,_)) -> toWitness ver k) $ Map.toList mp

        -- an index for each of the witness entries
        wsIndex :: Map.HashMap Witness Word16 = Map.fromList $ zip ws [0 :: Word16 ..]

        -- the save functions
        mpSave :: Map.HashMap k (Id -> v -> Builder) = flip Map.mapWithKey mp $
            \k (ver,BinaryOp{..}) ->
                let tag = putEx $ wsIndex Map.! toWitness ver k
                in \(Id w) v -> tag <> putEx w <> putOp v
