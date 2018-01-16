{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__  < 610
import System.Process
import qualified Control.Exception as C
#else
import System.Process hiding (readProcess)
import qualified Control.OldException as C
#endif

import System.Exit
import System.IO
import Data.List
import Data.Maybe
import System.Directory

import Control.Monad
import Control.Concurrent
import Text.Printf

import Text.Regex.PCRE.Light.Char8

------------------------------------------------------------------------

flags= [["-O","-fspec-constr"]
       ,["-O2"]
       ]

tests =
    [(Just 2, "cons",                       flags )     -- expect 2 fusions, with -O2 and -O
    ,(Just 2, "snoc",                       flags )
    ,(Just 2, "empty",                      flags )
--  ,(Just 1, "from-to",                    flags )
    ,(Just 2, "singleton",                  flags )
    ,(Just 4, "map",                        flags )
    ,(Just 5, "filter",                     flags )
    ,(Just 2, "replicate",                  flags )
    ,(Just 2, "takeWhile",                  flags )
    ,(Just 2, "index",                      flags )
    ,(Just 3, "null",                       flags )
    ,(Just 1, "length",                     flags )
    ,(Just 1, "length-bool",                flags )
    ,(Just 1, "length-unit",                flags )
    ,(Just 1, "length-char",                flags )
    ,(Just 1, "length-word",                flags )

    ,(Just 1, "length-word8",                flags )
    ,(Just 1, "length-word16",                flags )
    ,(Just 1, "length-word32",                flags )
    ,(Just 1, "length-word64",                flags )

    ,(Just 1, "length-int8",                flags )
    ,(Just 1, "length-int16",                flags )
    ,(Just 1, "length-int32",                flags )
    ,(Just 1, "length-int64",                flags )

    ,(Just 1, "length-double",                flags )
    ,(Just 1, "length-float",                flags )
    ,(Just 1, "head",                       flags )
    ,(Just 3, "append",                     flags )
    ,(Just 2, "sum",                        flags )
    ,(Just 3, "product",                    flags )
    ,(Just 1, "and",                        flags )
    ,(Just 1, "or",                         flags )
    ,(Just 2, "elem",                         flags )
    ,(Just 2, "tail",                         flags )
    ,(Just 2, "find",                         flags )
    ,(Just 2, "findIndex",                         flags )
    ,(Just 2, "init",                         flags )
    ,(Just 2, "last",                         flags )
    ,(Just 3, "foldl1",                         flags )
    ,(Just 3, "minimum",                         flags )
    ,(Just 3, "maximum",                         flags )
    ,(Just 3, "maximumBy",                         flags )
    ,(Just 3, "minimumBy",                         flags )
    ,(Just 2, "take",                         flags )
    ,(Just 2, "drop",                         flags )
    ,(Just 4, "zipwith",                         flags )
    ,(Just 4, "zipwith3",                         flags )
    ,(Just 3, "zip",                         flags ) -- expect zipU fusion
    ]

------------------------------------------------------------------------

main = do
    printf "Running %d fusion tests.\n" (length tests)
    vs <- forM tests $ \x -> do v <- run x
                                putChar '\n'
                                return v
    printf "\nDone.\n"
    if not (and vs)
       then exitWith (ExitFailure 1)
       else return ()

run :: (Maybe Int, String, [[String]]) -> IO Bool
run (n, name, args) = do
  printf "%20s: " name >> hFlush stdout
  v <- forM args $ \opt -> do
    putChar '.' >> hFlush stdout
    (cmd,ex,fusion) <- compile_program name opt
    if ex /= n
       then do
               printf "\n%s failed to trigger fusion. Expected %s, Actual %s.\n"
                            name (show n) (show ex)
               printf "Command line: %s\n" (show $ intercalate " " cmd)
               return False
       else
         if isJust fusion
            then do
                   printf "\n%s failed to remove all vectors.\n" name
                   printf "Remnants: %s\n" (show fusion)
                   printf "Command line: %s\n" (show $ intercalate " " cmd)
                   return False
            else return True
  return (and v)

------------------------------------------------------------------------

compile_program s opt = do

    let command = [(s ++ ".hs"), "-ddump-simpl","-ddump-simpl-stats","-no-recomp","--make"] ++ opt
    x <- readProcess "ghc" command [] 
    removeFile s
    case x of
         Left (err,str) -> do
            print str
            printf "GHC failed to compile %s\n" s
            exitWith (ExitFailure 1) -- fatal

         Right str      -> do
            return $ case match fusion_regex str [] of
                          Nothing -> (command,Nothing,Nothing)
                          Just xs -> {- trace (show xs) $ -}
                               let fusion_result = (read . last $ xs)
                               in case match left_over_vector str [] of
                                     Nothing -> (command, Just fusion_result, Nothing)
                                     Just n  -> (command, Just fusion_result, Just n)

------------------------------------------------------------------------

-- Fusion happened
fusion_regex = compile "(\\d+).*(?:stream|length|head)/unstream" []

-- Data.Array.Vector.Strict.Prim.UVec
-- UVectors were left behind
left_over_vector = compile "Data\\.Vector\\.Unboxed\\.Base\\.Vector" []

------------------------------------------------------------------------

-- Also, bytestring input/output, since we're strict
-- Document that this isn't for interactive

--
-- | readProcess forks an external process, reads its standard output
-- strictly, blocking until the process terminates, and returns either the output
-- string, or, in the case of non-zero exit status, an error code, and
-- any output.
--
-- Output is returned strictly, so this is not suitable for
-- interactive applications.
--
-- Users of this library should compile with -threaded if they
-- want other Haskell threads to keep running while waiting on
-- the result of readProcess.
--
-- >  > readProcess "date" [] []
-- >  Right "Thu Feb  7 10:03:39 PST 2008\n"
--
-- The argumenst are:
--
-- * The command to run, which must be in the $PATH, or an absolute path 
--  
-- * A list of separate command line arguments to the program
--
-- * A string to pass on the standard input to the program.
--
readProcess :: FilePath                     -- ^ command to run
            -> [String]                     -- ^ any arguments
            -> String                       -- ^ standard input
            -> IO (Either (ExitCode,String) String)  -- ^ either the stdout, or an exitcode and any output

readProcess cmd args input = C.handle (return . handler) $ do
    (inh,outh,errh,pid) <- runInteractiveProcess cmd args Nothing Nothing
    output  <- hGetContents outh
    outMVar <- newEmptyMVar
    forkIO $ (C.evaluate (length output) >> putMVar outMVar ())
    when (not (null input)) $ hPutStr inh input
    takeMVar outMVar
    ex     <- C.catch (waitForProcess pid) (\_e -> return ExitSuccess)
    hClose outh
    hClose inh          -- done with stdin
    hClose errh         -- ignore stderr

    return $ case ex of
        ExitSuccess   -> Right output
        ExitFailure _ -> Left (ex, output)

  where
    handler (C.ExitException e) = Left (e,"")
    handler e                   = Left (ExitFailure 1, show e)
