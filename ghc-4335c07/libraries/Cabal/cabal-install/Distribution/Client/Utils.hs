{-# LANGUAGE ForeignFunctionInterface, CPP #-}

module Distribution.Client.Utils ( MergeResult(..)
                                 , mergeBy, duplicates, duplicatesBy
                                 , readMaybe
                                 , inDir, withEnv, logDirChange
                                 , withExtraPathEnv
                                 , determineNumJobs, numberOfProcessors
                                 , removeExistingFile
                                 , withTempFileName
                                 , makeAbsoluteToCwd
                                 , makeRelativeToCwd, makeRelativeToDir
                                 , makeRelativeCanonical
                                 , filePathToByteString
                                 , byteStringToFilePath, tryCanonicalizePath
                                 , canonicalizePathNoThrow
                                 , moreRecentFile, existsAndIsMoreRecentThan
                                 , tryFindAddSourcePackageDesc
                                 , tryFindPackageDesc
                                 , relaxEncodingErrors)
       where

import Prelude ()
import Distribution.Client.Compat.Prelude

import Distribution.Compat.Environment
import Distribution.Compat.Exception   ( catchIO )
import Distribution.Compat.Time ( getModTime )
import Distribution.Simple.Setup       ( Flag(..) )
import Distribution.Verbosity
import Distribution.Simple.Utils       ( die', findPackageDesc )
import qualified Data.ByteString.Lazy as BS
import Data.Bits
         ( (.|.), shiftL, shiftR )
import System.FilePath
import Data.List
         ( groupBy )
import Foreign.C.Types ( CInt(..) )
import qualified Control.Exception as Exception
         ( finally, bracket )
import System.Directory
         ( canonicalizePath, doesFileExist, getCurrentDirectory
         , removeFile, setCurrentDirectory )
import System.IO
         ( Handle, hClose, openTempFile
#if MIN_VERSION_base(4,4,0)
         , hGetEncoding, hSetEncoding
#endif
         )
import System.IO.Unsafe ( unsafePerformIO )

#if MIN_VERSION_base(4,4,0)
import GHC.IO.Encoding
         ( recover, TextEncoding(TextEncoding) )
import GHC.IO.Encoding.Failure
         ( recoverEncode, CodingFailureMode(TransliterateCodingFailure) )
#endif

#if defined(mingw32_HOST_OS) || MIN_VERSION_directory(1,2,3)
import qualified System.Directory as Dir
import qualified System.IO.Error as IOError
#endif

-- | Generic merging utility. For sorted input lists this is a full outer join.
--
mergeBy :: (a -> b -> Ordering) -> [a] -> [b] -> [MergeResult a b]
mergeBy cmp = merge
  where
    merge []     ys     = [ OnlyInRight y | y <- ys]
    merge xs     []     = [ OnlyInLeft  x | x <- xs]
    merge (x:xs) (y:ys) =
      case x `cmp` y of
        GT -> OnlyInRight   y : merge (x:xs) ys
        EQ -> InBoth      x y : merge xs     ys
        LT -> OnlyInLeft  x   : merge xs  (y:ys)

data MergeResult a b = OnlyInLeft a | InBoth a b | OnlyInRight b

duplicates :: Ord a => [a] -> [[a]]
duplicates = duplicatesBy compare

duplicatesBy :: (a -> a -> Ordering) -> [a] -> [[a]]
duplicatesBy cmp = filter moreThanOne . groupBy eq . sortBy cmp
  where
    eq a b = case cmp a b of
               EQ -> True
               _  -> False
    moreThanOne (_:_:_) = True
    moreThanOne _       = False

-- | Like 'removeFile', but does not throw an exception when the file does not
-- exist.
removeExistingFile :: FilePath -> IO ()
removeExistingFile path = do
  exists <- doesFileExist path
  when exists $
    removeFile path

-- | A variant of 'withTempFile' that only gives us the file name, and while
-- it will clean up the file afterwards, it's lenient if the file is
-- moved\/deleted.
--
withTempFileName :: FilePath
                 -> String
                 -> (FilePath -> IO a) -> IO a
withTempFileName tmpDir template action =
  Exception.bracket
    (openTempFile tmpDir template)
    (\(name, _) -> removeExistingFile name)
    (\(name, h) -> hClose h >> action name)

-- | Executes the action in the specified directory.
--
-- Warning: This operation is NOT thread-safe, because current
-- working directory is a process-global concept.
inDir :: Maybe FilePath -> IO a -> IO a
inDir Nothing m = m
inDir (Just d) m = do
  old <- getCurrentDirectory
  setCurrentDirectory d
  m `Exception.finally` setCurrentDirectory old

-- | Executes the action with an environment variable set to some
-- value.
--
-- Warning: This operation is NOT thread-safe, because current
-- environment is a process-global concept.
withEnv :: String -> String -> IO a -> IO a
withEnv k v m = do
  mb_old <- lookupEnv k
  setEnv k v
  m `Exception.finally` (case mb_old of
    Nothing -> unsetEnv k
    Just old -> setEnv k old)

-- | Executes the action, increasing the PATH environment
-- in some way
--
-- Warning: This operation is NOT thread-safe, because the
-- environment variables are a process-global concept.
withExtraPathEnv :: [FilePath] -> IO a -> IO a
withExtraPathEnv paths m = do
  oldPathSplit <- getSearchPath
  let newPath = mungePath $ intercalate [searchPathSeparator] (paths ++ oldPathSplit)
      oldPath = mungePath $ intercalate [searchPathSeparator] oldPathSplit
      -- TODO: This is a horrible hack to work around the fact that
      -- setEnv can't take empty values as an argument
      mungePath p | p == ""   = "/dev/null"
                  | otherwise = p
  setEnv "PATH" newPath
  m `Exception.finally` setEnv "PATH" oldPath

-- | Log directory change in 'make' compatible syntax
logDirChange :: (String -> IO ()) -> Maybe FilePath -> IO a -> IO a
logDirChange _ Nothing m = m
logDirChange l (Just d) m = do
  l $ "cabal: Entering directory '" ++ d ++ "'\n"
  m `Exception.finally`
    (l $ "cabal: Leaving directory '" ++ d ++ "'\n")

foreign import ccall "getNumberOfProcessors" c_getNumberOfProcessors :: IO CInt

-- The number of processors is not going to change during the duration of the
-- program, so unsafePerformIO is safe here.
numberOfProcessors :: Int
numberOfProcessors = fromEnum $ unsafePerformIO c_getNumberOfProcessors

-- | Determine the number of jobs to use given the value of the '-j' flag.
determineNumJobs :: Flag (Maybe Int) -> Int
determineNumJobs numJobsFlag =
  case numJobsFlag of
    NoFlag        -> 1
    Flag Nothing  -> numberOfProcessors
    Flag (Just n) -> n

-- | Given a relative path, make it absolute relative to the current
-- directory. Absolute paths are returned unmodified.
makeAbsoluteToCwd :: FilePath -> IO FilePath
makeAbsoluteToCwd path | isAbsolute path = return path
                       | otherwise       = do cwd <- getCurrentDirectory
                                              return $! cwd </> path

-- | Given a path (relative or absolute), make it relative to the current
-- directory, including using @../..@ if necessary.
makeRelativeToCwd :: FilePath -> IO FilePath
makeRelativeToCwd path =
    makeRelativeCanonical <$> canonicalizePath path <*> getCurrentDirectory

-- | Given a path (relative or absolute), make it relative to the given
-- directory, including using @../..@ if necessary.
makeRelativeToDir :: FilePath -> FilePath -> IO FilePath
makeRelativeToDir path dir =
    makeRelativeCanonical <$> canonicalizePath path <*> canonicalizePath dir

-- | Given a canonical absolute path and canonical absolute dir, make the path
-- relative to the directory, including using @../..@ if necessary. Returns
-- the original absolute path if it is not on the same drive as the given dir.
makeRelativeCanonical :: FilePath -> FilePath -> FilePath
makeRelativeCanonical path dir
  | takeDrive path /= takeDrive dir = path
  | otherwise                       = go (splitPath path) (splitPath dir)
  where
    go (p:ps) (d:ds) | p == d = go ps ds
    go    []     []           = "./"
    go    ps     ds           = joinPath (replicate (length ds) ".." ++ ps)

-- | Convert a 'FilePath' to a lazy 'ByteString'. Each 'Char' is
-- encoded as a little-endian 'Word32'.
filePathToByteString :: FilePath -> BS.ByteString
filePathToByteString p =
  BS.pack $ foldr conv [] codepts
  where
    codepts :: [Word32]
    codepts = map (fromIntegral . ord) p

    conv :: Word32 -> [Word8] -> [Word8]
    conv w32 rest = b0:b1:b2:b3:rest
      where
        b0 = fromIntegral $ w32
        b1 = fromIntegral $ w32 `shiftR` 8
        b2 = fromIntegral $ w32 `shiftR` 16
        b3 = fromIntegral $ w32 `shiftR` 24

-- | Reverse operation to 'filePathToByteString'.
byteStringToFilePath :: BS.ByteString -> FilePath
byteStringToFilePath bs | bslen `mod` 4 /= 0 = unexpected
                        | otherwise = go 0
  where
    unexpected = "Distribution.Client.Utils.byteStringToFilePath: unexpected"
    bslen = BS.length bs

    go i | i == bslen = []
         | otherwise = (chr . fromIntegral $ w32) : go (i+4)
      where
        w32 :: Word32
        w32 = b0 .|. (b1 `shiftL` 8) .|. (b2 `shiftL` 16) .|. (b3 `shiftL` 24)
        b0 = fromIntegral $ BS.index bs i
        b1 = fromIntegral $ BS.index bs (i + 1)
        b2 = fromIntegral $ BS.index bs (i + 2)
        b3 = fromIntegral $ BS.index bs (i + 3)

-- | Workaround for the inconsistent behaviour of 'canonicalizePath'. Always
-- throws an error if the path refers to a non-existent file.
tryCanonicalizePath :: FilePath -> IO FilePath
tryCanonicalizePath path = do
  ret <- canonicalizePath path
#if defined(mingw32_HOST_OS) || MIN_VERSION_directory(1,2,3)
  exists <- liftM2 (||) (doesFileExist ret) (Dir.doesDirectoryExist ret)
  unless exists $
    IOError.ioError $ IOError.mkIOError IOError.doesNotExistErrorType "canonicalizePath"
                        Nothing (Just ret)
#endif
  return ret

-- | A non-throwing wrapper for 'canonicalizePath'. If 'canonicalizePath' throws
-- an exception, returns the path argument unmodified.
canonicalizePathNoThrow :: FilePath -> IO FilePath
canonicalizePathNoThrow path = do
  canonicalizePath path `catchIO` (\_ -> return path)

--------------------
-- Modification time

-- | Like Distribution.Simple.Utils.moreRecentFile, but uses getModTime instead
-- of getModificationTime for higher precision. We can't merge the two because
-- Distribution.Client.Time uses MIN_VERSION macros.
moreRecentFile :: FilePath -> FilePath -> IO Bool
moreRecentFile a b = do
  exists <- doesFileExist b
  if not exists
    then return True
    else do tb <- getModTime b
            ta <- getModTime a
            return (ta > tb)

-- | Like 'moreRecentFile', but also checks that the first file exists.
existsAndIsMoreRecentThan :: FilePath -> FilePath -> IO Bool
existsAndIsMoreRecentThan a b = do
  exists <- doesFileExist a
  if not exists
    then return False
    else a `moreRecentFile` b

-- | Sets the handler for encoding errors to one that transliterates invalid
-- characters into one present in the encoding (i.e., \'?\').
-- This is opposed to the default behavior, which is to throw an exception on
-- error. This function will ignore file handles that have a Unicode encoding
-- set. It's a no-op for versions of `base` less than 4.4.
relaxEncodingErrors :: Handle -> IO ()
relaxEncodingErrors handle = do
#if MIN_VERSION_base(4,4,0)
  maybeEncoding <- hGetEncoding handle
  case maybeEncoding of
    Just (TextEncoding name decoder encoder) | not ("UTF" `isPrefixOf` name) ->
      let relax x = x { recover = recoverEncode TransliterateCodingFailure }
      in hSetEncoding handle (TextEncoding name decoder (fmap relax encoder))
    _ ->
#endif
      return ()

-- |Like 'tryFindPackageDesc', but with error specific to add-source deps.
tryFindAddSourcePackageDesc :: Verbosity -> FilePath -> String -> IO FilePath
tryFindAddSourcePackageDesc verbosity depPath err = tryFindPackageDesc verbosity depPath $
    err ++ "\n" ++ "Failed to read cabal file of add-source dependency: "
    ++ depPath

-- |Try to find a @.cabal@ file, in directory @depPath@. Fails if one cannot be
-- found, with @err@ prefixing the error message. This function simply allows
-- us to give a more descriptive error than that provided by @findPackageDesc@.
tryFindPackageDesc :: Verbosity -> FilePath -> String -> IO FilePath
tryFindPackageDesc verbosity depPath err = do
    errOrCabalFile <- findPackageDesc depPath
    case errOrCabalFile of
        Right file -> return file
        Left _ -> die' verbosity err
