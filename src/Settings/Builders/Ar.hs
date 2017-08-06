module Settings.Builders.Ar (arBuilderArgs, arCmd, chunksOfSize) where

import Settings.Builders.Common

arBuilderArgs :: Args
arBuilderArgs = builder Ar ? mconcat [ arg "q"
                                     , arg =<< getOutput
                                     , getInputs ]

-- This count includes arg "q" and arg file parameters in arBuilderArgs.
-- Update this value appropriately when changing arBuilderArgs.
arFlagsCount :: Int
arFlagsCount = 2

-- | Invoke 'Ar' builder given a path to it and a list of arguments. Take care
-- not to exceed the limit on command line length, which differs across
-- supported operating systems (see 'cmdLineLengthLimit'). 'Ar' needs to be
-- handled in a special way because we sometimes need to archive __a lot__ of
-- files (in Cabal package, for example, command line length can reach 2MB!).
-- To work around the limit on the command line length we pass the list of files
-- to be archived via a temporary file, or alternatively, we split argument list
-- into chunks and call 'Ar' multiple times (when passing arguments via a
-- temporary file is not supported).
arCmd :: FilePath -> [String] -> Action ()
arCmd path argList = do
    arSupportsAtFile <- flag ArSupportsAtFile
    let flagArgs = take arFlagsCount argList
        fileArgs = drop arFlagsCount argList
    if arSupportsAtFile
    then useAtFile path flagArgs fileArgs
    else useSuccessiveInvocations path flagArgs fileArgs

useAtFile :: FilePath -> [String] -> [String] -> Action ()
useAtFile path flagArgs fileArgs = withTempFile $ \tmp -> do
    writeFile' tmp $ unwords fileArgs
    cmd [path] flagArgs ('@' : tmp)

useSuccessiveInvocations :: FilePath -> [String] -> [String] -> Action ()
useSuccessiveInvocations path flagArgs fileArgs = do
    maxChunk <- cmdLineLengthLimit
    forM_ (chunksOfSize maxChunk fileArgs) $ \argsChunk ->
        unit . cmd [path] $ flagArgs ++ argsChunk

-- | @chunksOfSize size strings@ splits a given list of strings into chunks not
-- exceeding the given @size@. If that is impossible, it uses singleton chunks.
chunksOfSize :: Int -> [String] -> [[String]]
chunksOfSize n = repeatedly f
    where f xs = splitAt (max 1 $ length $ takeWhile (<= n) $ scanl1 (+) $ map length xs) xs
