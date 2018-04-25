----------------------------------------------------------------------------
-- |
-- Module      :  Test.Cabal.CheckArMetadata
-- Created     :   8 July 2017
--
-- Check well-formedness of metadata of .a files that @ar@ command produces.
-- One of the crucial properties of .a files is that they must be
-- deterministic - i.e. they must not include creation date as their
-- contents to facilitate deterministic builds.
----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Test.Cabal.CheckArMetadata (checkMetadata) where

import Test.Cabal.Prelude

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Char (isSpace)
import System.IO

import Distribution.Compiler              (CompilerFlavor(..), CompilerId(..))
import Distribution.Package               (getHSLibraryName)
import Distribution.Version               (mkVersion)
import Distribution.Simple.Compiler       (compilerId)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo, compiler, localUnitId)

-- Almost a copypasta of Distribution.Simple.Program.Ar.wipeMetadata
checkMetadata :: LocalBuildInfo -> FilePath -> IO ()
checkMetadata lbi dir = withBinaryFile path ReadMode $ \ h ->
  hFileSize h >>= checkArchive h
  where
    path = dir </> "lib" ++ getHSLibraryName (localUnitId lbi) ++ ".a"

    _ghc_7_10 = case compilerId (compiler lbi) of
      CompilerId GHC version | version >= mkVersion [7, 10]  -> True
      _                                                      -> False

    checkError msg = assertFailure (
        "PackageTests.DeterministicAr.checkMetadata: " ++ msg ++
        " in " ++ path) >> undefined
    archLF = "!<arch>\x0a" -- global magic, 8 bytes
    x60LF = "\x60\x0a" -- header magic, 2 bytes
    metadata = BS.concat
        [ "0           " -- mtime, 12 bytes
        , "0     " -- UID, 6 bytes
        , "0     " -- GID, 6 bytes
        , "0644    " -- mode, 8 bytes
        ]
    headerSize = 60

    -- http://en.wikipedia.org/wiki/Ar_(Unix)#File_format_details
    checkArchive :: Handle -> Integer -> IO ()
    checkArchive h archiveSize = do
        global <- BS.hGet h (BS.length archLF)
        unless (global == archLF) $ checkError "Bad global header"
        checkHeader (toInteger $ BS.length archLF)

      where
        checkHeader :: Integer -> IO ()
        checkHeader offset = case compare offset archiveSize of
            EQ -> return ()
            GT -> checkError (atOffset "Archive truncated")
            LT -> do
                header <- BS.hGet h headerSize
                unless (BS.length header == headerSize) $
                    checkError (atOffset "Short header")
                let magic = BS.drop 58 header
                unless (magic == x60LF) . checkError . atOffset $
                    "Bad magic " ++ show magic ++ " in header"

                unless (metadata == BS.take 32 (BS.drop 16 header))
                    . checkError . atOffset $ "Metadata has changed"

                let size = BS.take 10 $ BS.drop 48 header
                objSize <- case reads (BS8.unpack size) of
                    [(n, s)] | all isSpace s -> return n
                    _ -> checkError (atOffset "Bad file size in header")

                let nextHeader = offset + toInteger headerSize +
                        -- Odd objects are padded with an extra '\x0a'
                        if odd objSize then objSize + 1 else objSize
                hSeek h AbsoluteSeek nextHeader
                checkHeader nextHeader

          where
            atOffset msg = msg ++ " at offset " ++ show offset
