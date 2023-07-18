{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module GHC.Toolchain.Tools.Ar (Ar(..), findAr) where

import Control.Monad
import System.FilePath
import Data.List (isInfixOf)

import GHC.Toolchain.Prelude
import GHC.Toolchain.Utils
import GHC.Toolchain.Program

data Ar = Ar { arMkArchive :: Program
             , arIsGnu :: Bool
             , arSupportsAtFile :: Bool
             , arSupportsDashL :: Bool
             , arNeedsRanlib :: Bool
             }
    deriving (Read, Eq, Ord)

-- These instances are more suitable for diffing
instance Show Ar where
  show Ar{..} = unlines
    [ "Ar"
    , "{ arMkArchive = " ++ show arMkArchive
    , ", arIsGnu = " ++ show arIsGnu
    , ", arSupportsAtFile = " ++ show arSupportsAtFile
    , ", arSupportsDashL = " ++ show arSupportsDashL
    , ", arNeedsRanlib = " ++ show arNeedsRanlib
    , "}"
    ]

findAr :: Maybe String -- ^ Vendor name from the target triple, if specified
       -> ProgOpt -> M Ar
findAr vendor progOpt = checking "for 'ar'" $ do
    bareAr <- findProgram "ar archiver" progOpt ["ar", "llvm-ar"]
    arIsGnu <- ("GNU" `isInfixOf`) <$> readProgramStdout bareAr ["--version"]

    -- Figure out how to invoke ar to create archives...
    mkArchive <- checking "for how to make archives"
        $ makeArchiveProgram arIsGnu bareAr

    arSupportsAtFile <- checkArSupportsAtFile bareAr mkArchive <|> return False
    arSupportsDashL <- checkArSupportsDashL bareAr <|> return False
    let arNeedsRanlib
          | arIsGnu = False
          -- TODO: It'd be better not to handle Apple specifically here?
          -- It's quite tedious to check for Apple's crazy timestamps in
          -- .a files, so we hardcode it.
          | vendor == Just "apple" = True
          | mode:_ <- prgFlags mkArchive
          , 's' `elem` mode = False
          | otherwise = True

    return $ Ar { arMkArchive = mkArchive
                , arIsGnu
                , arSupportsAtFile
                , arSupportsDashL
                , arNeedsRanlib
                }

makeArchiveProgram :: Bool  -- ^ is GNU ar?
                   -> Program -> M Program
makeArchiveProgram isGnuAr ar
  | isGnuAr =
    -- GNU ar needs special treatment: it appears to have problems with
    -- object files with the same name if you use the 's' modifier, but
    -- simple 'ar q' works fine, and doesn't need a separate ranlib.
    check (set _prgFlags ["q"] ar)
  | otherwise =
    oneOf err
      (map (\flag -> check $ set _prgFlags [flag] ar)
           ["qclsZ", "qcls", "qcs", "qcl", "qc"])
  where
    check ar' = ar' <$ checkArWorks ar'
    err = "Failed to figure out how to make archives"

checkArWorks :: Program -> M ()
checkArWorks prog = checking "that ar works" $ withTempDir $ \dir -> do
    let dummy = dir </> "conftest.dummy"
        archive = dir </> "conftest.a"
    createFile dummy
    callProgram prog [archive, dummy]
    -- Check that result was created as some llvm-ar versions exit with code
    -- zero even if they fail to parse the command-line.
    expectFileExists archive "ar didn't create an archive"

checkArSupportsDashL :: Program -> M Bool
checkArSupportsDashL bareAr = checking "that ar supports -L" $ withTempDir $ \dir -> do
    let file ext = dir </> "conftest" <.> ext
        archive1 = dir </> "conftest-a.a"
        archive2 = dir </> "conftest-b.a"
        merged   = dir </> "conftest.a"
    mapM_ (createFile . file) ["file", "a0", "a1", "b0", "b1"]
    -- Build two archives, merge them, and check that the
    -- result contains the original files rather than the two
    -- archives.
    callProgram bareAr ["qc", archive1, file "a0", file "a1"]
    callProgram bareAr ["qc", archive2, file "b0", file "b1"]
    oneOf "trying -L"
        [ do callProgram bareAr ["qcL", merged, archive1, archive2]
             contents <- readProgramStdout bareAr ["t", merged]
             return $ "conftest.a1" `isInfixOf` contents
        , return False
        ]

checkArSupportsAtFile :: Program -> Program -> M Bool
checkArSupportsAtFile bareAr mkArchive = checking "that ar supports @-files" $ withTempDir $ \dir -> do
    let conftest = "conftest.file"
        f = dir </> conftest
        atfile = dir </> "conftest.atfile"
        archive = dir </> "conftest.a"
        objs = replicate 2 f
    createFile f
    writeFile atfile (unlines objs)
    callProgram mkArchive [archive, "@" ++ atfile]
    contents <- readProgramStdout bareAr ["t", archive]
    -- Careful: The files output by `ar t` use relative paths, so we can't
    -- compare against `objs`
    if lines contents == replicate 2 conftest
      then return True
      else logDebug "Contents didn't match" >> return False
