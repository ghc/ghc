#ifdef HEAD
module PreludeIO (
    FilePath, IOError, fail, userError, catch,
    putChar, putStr, putStrLn, print,
    getChar, getLine, getContents, interact,
    readFile, writeFile, appendFile, readIO, readLn
  ) where

import PreludeBuiltin
#endif /* HEAD */
#ifdef BODY

#if STD_PRELUDE
type  FilePath = String

data IOError    -- The internals of this type are system dependent

instance  Show IOError  where ...
instance  Eq IOError  where ...
#endif

#if STD_PRELUDE
fail             ::  IOError -> IO a 
fail             =   primFail

userError        ::  String -> IOError
userError        =   primUserError

catch            ::  IO a -> (IOError -> IO a) -> IO a 
catch            =   primCatch
#else
#endif

#if STD_PRELUDE
#else
-- this guy can go in either monad
primFail         :: Exception -> ST s a 
primFail err     =  ST (\ s -> primRaise err)
#endif

#if STD_PRELUDE
putChar          :: Char -> IO ()
putChar          =  primPutChar

putStr           :: String -> IO ()
putStr s         =  mapM_ putChar s

putStrLn         :: String -> IO ()
putStrLn s       =  do putStr s
                       putStr "\n"

print            :: Show a => a -> IO ()
print x          =  putStrLn (show x)

getChar          :: IO Char
getContents      =  primGetChar

getLine          :: IO String
getLine          =  do c <- getChar
                       if c == '\n' then return "" else 
                          do s <- getLine
                             return (c:s)
            
getContents      :: IO String
getContents      =  primGetContents

interact         ::  (String -> String) -> IO ()
interact f       =   do s <- getContents
                        putStr (f s)

readFile         :: FilePath -> IO String
readFile         =  primReadFile

writeFile        :: FilePath -> String -> IO ()
writeFile        =  primWriteFile

appendFile       :: FilePath -> String -> IO ()
appendFile       =  primAppendFile
#endif

  -- raises an exception instead of an error
readIO           :: Read a => String -> IO a
readIO s         =  case [x | (x,t) <- reads s, ("","") <- lex t] of
                         [x] -> return x
                         []  -> fail (userError "PreludeIO.readIO: no parse")
                         _   -> fail (userError 
                                       "PreludeIO.readIO: ambiguous parse")

#if STD_PRELUDE
readLn           :: Read a => IO a
readLn           =  do l <- getLine
                       r <- readIO l
                       return r
#endif

#endif /* BODY */
