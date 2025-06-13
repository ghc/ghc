{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
{- Note [The need for Ar.hs]
   ~~~~~~~~~~~~~~~~~~~~~~~~~
Building `-staticlib` previously required the presence of libtool, and was a
such restricted to mach-o only. As libtool on macOS and gnu libtool are very
different, there was no simple portable way to support this.

libtool for static archives does essentially: concatenate the input archives,
add the input objects, and create a symbol index. Using `ar` for this task
fails as even `ar` (bsd and gnu, llvm, ...) do not provide the same
features across platforms (e.g. index prefixed retrieval of objects with
the same name.)

As archives are rather simple structurally, we can just build the archives
with Haskell directly and use ranlib on the final result to get the symbol
index. This should allow us to work around with the differences/availability
of libtool across different platforms.
-}
module GHC.SysTools.Ar
  (ArchiveEntry(..)
  ,Archive(..)
  ,afilter

  ,parseAr

  ,loadAr
  ,loadObj
  ,writeBSDAr
  ,writeGNUAr

  ,isBSDSymdef
  ,isGNUSymdef
  )
   where

import GHC.Prelude

import Data.List (mapAccumL, isPrefixOf)
import Data.Monoid ((<>))
import Data.Binary.Get
import Data.Binary.Put
import Control.Monad
import Control.Applicative
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
#if !defined(mingw32_HOST_OS)
import qualified System.Posix.Files as POSIX
#endif
import System.FilePath (takeFileName)

data ArchiveEntry = ArchiveEntry
    { filename :: String       -- ^ File name.
    , filetime :: Int          -- ^ File modification time.
    , fileown  :: Int          -- ^ File owner.
    , filegrp  :: Int          -- ^ File group.
    , filemode :: Int          -- ^ File mode.
    , filesize :: Int          -- ^ File size.
    , filedata :: B.ByteString -- ^ File bytes.
    } deriving (Eq, Show)

newtype Archive = Archive [ArchiveEntry]
        deriving (Eq, Show, Semigroup, Monoid)

afilter :: (ArchiveEntry -> Bool) -> Archive -> Archive
afilter f (Archive xs) = Archive (filter f xs)

isBSDSymdef, isGNUSymdef :: ArchiveEntry -> Bool
isBSDSymdef a = "__.SYMDEF" `isPrefixOf` (filename a)
isGNUSymdef a = "/" == (filename a)

-- | Archives have numeric values padded with '\x20' to the right.
getPaddedInt :: B.ByteString -> Int
getPaddedInt = read . C.unpack . C.takeWhile (/= '\x20')

putPaddedInt :: Int -> Int -> Put
putPaddedInt padding i = putPaddedString '\x20' padding (show i)

putPaddedString :: Char -> Int -> String -> Put
putPaddedString pad padding s = putByteString . C.pack . take padding $ s `mappend` (repeat pad)

getBSDArchEntries :: Get [ArchiveEntry]
getBSDArchEntries = do
    empty <- isEmpty
    if empty then
        return []
     else do
        name    <- getByteString 16
        when ('/' `C.elem` name && C.take 3 name /= "#1/") $
          fail "Looks like GNU Archive"
        time    <- getPaddedInt <$> getByteString 12
        own     <- getPaddedInt <$> getByteString 6
        grp     <- getPaddedInt <$> getByteString 6
        mode    <- getPaddedInt <$> getByteString 8
        st_size <- getPaddedInt <$> getByteString 10
        end     <- getByteString 2
        when (end /= "\x60\x0a") $
          fail ("[BSD Archive] Invalid archive header end marker for name: " ++
                C.unpack name)
        off1    <- liftM fromIntegral bytesRead :: Get Int
        -- BSD stores extended filenames, by writing #1/<length> into the
        -- name field, the first @length@ bytes then represent the file name
        -- thus the payload size is filesize + file name length.
        name    <- if C.unpack (C.take 3 name) == "#1/" then
                        liftM (C.unpack . C.takeWhile (/= '\0')) (getByteString $ read $ C.unpack $ C.drop 3 name)
                    else
                        return $ C.unpack $ C.takeWhile (/= ' ') name
        off2    <- liftM fromIntegral bytesRead :: Get Int
        file    <- getByteString (st_size - (off2 - off1))
        -- data sections are two byte aligned (see #15396)
        when (odd st_size) $
          void (getByteString 1)

        rest    <- getBSDArchEntries
        return $ (ArchiveEntry name time own grp mode (st_size - (off2 - off1)) file) : rest

-- | GNU Archives feature a special '//' entry that contains the
-- extended names. Those are referred to as /<num>, where num is the
-- offset into the '//' entry.
-- In addition, filenames are terminated with '/' in the archive.
getGNUArchEntries :: Maybe ArchiveEntry -> Get [ArchiveEntry]
getGNUArchEntries extInfo = do
  empty <- isEmpty
  if empty
    then return []
    else
    do
      name    <- getByteString 16
      time    <- getPaddedInt <$> getByteString 12
      own     <- getPaddedInt <$> getByteString 6
      grp     <- getPaddedInt <$> getByteString 6
      mode    <- getPaddedInt <$> getByteString 8
      st_size <- getPaddedInt <$> getByteString 10
      end     <- getByteString 2
      when (end /= "\x60\x0a") $
        fail ("[BSD Archive] Invalid archive header end marker for name: " ++
              C.unpack name)
      file <- getByteString st_size
      -- data sections are two byte aligned (see #15396)
      when (odd st_size) $
        void (getByteString 1)
      name <- return . C.unpack $
        if C.unpack (C.take 1 name) == "/"
        then case C.takeWhile (/= ' ') name of
               name@"/"  -> name               -- symbol table
               name@"//" -> name               -- extended file names table
               name      -> getExtName extInfo (read . C.unpack $ C.drop 1 name)
        else C.takeWhile (/= '/') name
      case name of
        "/"  -> getGNUArchEntries extInfo
        "//" -> getGNUArchEntries (Just (ArchiveEntry name time own grp mode st_size file))
        _    -> (ArchiveEntry name time own grp mode st_size file :) <$> getGNUArchEntries extInfo

  where
   getExtName :: Maybe ArchiveEntry -> Int -> B.ByteString
   getExtName Nothing _ = error "Invalid extended filename reference."
   getExtName (Just info) offset = C.takeWhile (/= '/') . C.drop offset $ filedata info

-- | put an Archive Entry. This assumes that the entries
-- have been preprocessed to account for the extended file name
-- table section "//" e.g. for GNU Archives. Or that the names
-- have been move into the payload for BSD Archives.
putArchEntry :: ArchiveEntry -> PutM ()
putArchEntry (ArchiveEntry name time own grp mode st_size file) = do
  putPaddedString ' '  16 name
  putPaddedInt         12 time
  putPaddedInt          6 own
  putPaddedInt          6 grp
  putPaddedInt          8 mode
  putPaddedInt         10 st_size
  putByteString           "\x60\x0a"
  putByteString           file
  when (pad == 1) $
    putWord8              0x0a
  where
    pad         = st_size `mod` 2

getArchMagic :: Get ()
getArchMagic = do
  magic <- liftM C.unpack $ getByteString 8
  if magic /= "!<arch>\n"
    then fail $ "Invalid magic number " ++ show magic
    else return ()

putArchMagic :: Put
putArchMagic = putByteString $ C.pack "!<arch>\n"

getArch :: Get Archive
getArch = Archive <$> do
  getArchMagic
  getBSDArchEntries <|> getGNUArchEntries Nothing

putBSDArch :: Archive -> PutM ()
putBSDArch (Archive as) = do
  putArchMagic
  mapM_ putArchEntry (processEntries as)

  where
    padStr pad size str = take size $ str <> repeat pad
    nameSize name = case length name `divMod` 4 of
      (n, 0) -> 4 * n
      (n, _) -> 4 * (n + 1)
    needExt name = length name > 16 || ' ' `elem` name
    processEntry :: ArchiveEntry -> ArchiveEntry
    processEntry archive@(ArchiveEntry name _ _ _ _ st_size _)
      | needExt name = archive { filename = "#1/" <> show sz
                               , filedata = C.pack (padStr '\0' sz name) <> filedata archive
                               , filesize = st_size + sz }
      | otherwise    = archive

      where sz = nameSize name

    processEntries = map processEntry

putGNUArch :: Archive -> PutM ()
putGNUArch (Archive as) = do
  putArchMagic
  mapM_ putArchEntry (processEntries as)

  where
    processEntry :: ArchiveEntry -> ArchiveEntry -> (ArchiveEntry, ArchiveEntry)
    processEntry extInfo archive@(ArchiveEntry name _ _ _ _ _ _)
      | length name > 15 = ( extInfo { filesize = filesize extInfo + length name + 2
                                    ,  filedata = filedata extInfo <>  C.pack name <> "/\n" }
                           , archive { filename = "/" <> show (filesize extInfo) } )
      | otherwise        = ( extInfo, archive { filename = name <> "/" } )

    processEntries :: [ArchiveEntry] -> [ArchiveEntry]
    processEntries =
      uncurry (:) . mapAccumL processEntry (ArchiveEntry "//" 0 0 0 0 0 mempty)

parseAr :: B.ByteString -> Archive
parseAr = runGet getArch . L.fromChunks . pure

writeBSDAr, writeGNUAr :: FilePath -> Archive -> IO ()
writeBSDAr fp = L.writeFile fp . runPut . putBSDArch
writeGNUAr fp = L.writeFile fp . runPut . putGNUArch

loadAr :: FilePath -> IO Archive
loadAr fp = parseAr <$> B.readFile fp

loadObj :: FilePath -> IO ArchiveEntry
loadObj fp = do
  payload <- B.readFile fp
  (modt, own, grp, mode) <- fileInfo fp
  return $ ArchiveEntry
    (takeFileName fp) modt own grp mode
    (B.length payload) payload

-- | Take a filePath and return (mod time, own, grp, mode in decimal)
fileInfo :: FilePath -> IO ( Int, Int, Int, Int) -- ^ mod time, own, grp, mode (in decimal)
#if defined(mingw32_HOST_OS)
-- on windows mod time, owner group and mode are zero.
fileInfo _ = pure (0,0,0,0)
#else
fileInfo fp = go <$> POSIX.getFileStatus fp
  where go status = ( fromEnum $ POSIX.modificationTime status
                    , fromIntegral $ POSIX.fileOwner status
                    , fromIntegral $ POSIX.fileGroup status
                    , oct2dec . fromIntegral $ POSIX.fileMode status
                    )

oct2dec :: Int -> Int
oct2dec = foldl' (\a b -> a * 10 + b) 0 . reverse . dec 8
  where dec _ 0 = []
        dec b i = let (rest, last) = i `quotRem` b
                  in last:dec b rest

#endif
