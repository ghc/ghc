module Main (main) where

import IO
import Posix

import Directory (setCurrentDirectory)
import System    ( getEnv, exitWith, ExitCode(..) )
import Char      (isSpace)

main :: IO ()
main =
   do
    initialize
    commandLoop

{- 
   Standard shell practice: move std descriptors out of the way so
   it's more convenient to set them up for children.  Also set up an
   interrupt handler which will put us back in the main loop.
-}

initialize :: IO ()
initialize =
    dupTo stdInput  myStdin			>>
    dupTo stdOutput myStdout			>>
    dupTo stdError  myStderr			>>
    fdClose stdInput				>>
    fdClose stdOutput				>>
--  fdClose stdError				>>
    installHandler sigINT (Catch intr) Nothing	>>
    return ()

-- some random fd numbers...
myStdin  = intToFd 16
myStdout = intToFd 17
myStderr = intToFd 18

-- For user interrupts 

intr :: IO ()
intr =
    fdWrite myStdout "\n"	>>
    commandLoop

{-
   Simple command loop: print a prompt, read a command, process the command.
   Repeat as necessary.
-}

commandLoop :: IO ()    
commandLoop =
    fdWrite myStdout "$ "  >>
    try (readCommand myStdin)  >>=
    either
      (\ err -> 
	 if isEOFError err then
	    return ()
	 else
	    dieHorribly)
      (\ cmd ->
	try (processCommand cmd) >>= either (\ err -> commandLoop) (\ succ -> commandLoop))
  where
    dieHorribly :: IO ()
    dieHorribly =
	do
	 errMsg "read failed"
	 exitWith (ExitFailure 1)

{-
   Read a command a character at a time (to allow for fancy processing later).
   On newline, you're done, unless the newline was escaped by a backslash.
-}

readCommand :: Fd -> IO String
readCommand fd = 
    accumString ""				>>= \ cmd ->
    return cmd
  where
    accumString :: String -> IO String
    accumString s =
	myGetChar fd				>>= \ c ->
	case c of
	  '\\' ->
	    myGetChar fd			>>= \ c' ->
	    accumString (c':c:s)
	  '\n' -> return (reverse s)
          ch  -> accumString (ch:s)

myGetChar :: Fd -> IO Char
myGetChar chan =
   do
    (s,len) <- fdRead chan 1
    case len of
      0 -> myGetChar chan
      1 -> return (head s)

{-
   To process a command, first parse it into words, then do the necessary
   redirections, and finally perform the desired command.  Built-ins are
   checked first, and if none match, we execute an external command.
-}

processCommand :: String -> IO ()
processCommand "" = return ()
processCommand s =
  do
   words <- parseCommand s
   (inFile, outFile, words) <- parseRedirection words
   performRedirections inFile outFile
   let
    cmd = head words
    args = tail words
   case builtin cmd of
     Just f -> 
	do
	 f args
	 fdClose stdInput
	 fdClose stdOutput
     Nothing -> exec cmd args

{-
   Redirections are a bit of a pain, really.  If none are specified, we
   dup our own file descriptors.  Otherwise, we try to open the files
   as requested.
-}

performRedirections :: Maybe String -> Maybe String -> IO ()
performRedirections inFile outFile =
    (case inFile of
	Nothing -> dupTo myStdin stdInput
	Just x  ->
	    try (openFd x ReadOnly Nothing defaultFileFlags)
						>>=
	    either
	      (\ err ->
		errMsg ("Can't redirect input from " ++ x) >>
		fail (userError "redirect"))
	      (\ succ -> return ()))	        >>
    (case outFile of
	Nothing ->
	    dupTo myStdout stdOutput
	Just x ->
	    try (createFile x stdFileMode) >>=
	    either
	      (\ err ->
		do
		 errMsg ("Can't redirect output to " ++ x) 
		 fdClose stdInput
		 fail (userError "redirect"))
	      (\ succ -> return ()))

{-
   We parse a command line into words according to the following rules:
    1) Anything inside pairs of "" or '' is parsed literally.
    2) Anything (outside of quotes) escaped by \ is taken literally.
    3) '<' and '>' are words all by themselves, unless escaped or quoted.
    4) Whitespace separates words
-}

parseCommand :: String -> IO [String]
parseCommand = getTokens []
  where
    getTokens :: [String] -> String -> IO [String]
    getTokens ts "" = return (reverse ts)
    getTokens ts (c:cs) | isSpace c = getTokens ts cs
    getTokens ts s = 
	getToken s				>>= \ (t, s') ->
	getTokens (t:ts) s'

    getToken :: String -> IO (String, String)
    getToken (c:cs)
      | c == '<' || c == '>' = return ([c], cs)
      | c == '"' || c == '\'' = accumQuote c "" cs
      | otherwise = accumToken [c] cs

    accumToken :: [Char] -> String -> IO (String, String)
    accumToken cs "" = return (reverse cs, "")
    accumToken cs ('\\':c:s) = accumToken (c:cs) s
    accumToken cs x@(c:s)
      | isSpace c || c == '<' || c == '>' = return (reverse cs, x)
      | c == '"' || c == '\'' = accumQuote c cs s
      | otherwise = accumToken (c:cs) s

    accumQuote :: Char -> [Char] -> String -> IO (String, String)
    accumQuote q cs "" =
	errMsg ("Unmatched " ++ [q])		>>
	fail (userError "unmatched quote")
    accumQuote q cs (c:s)
      | c == q = accumToken cs s
      | otherwise = accumQuote q (c:cs) s

{-
  Here we look for "<" and ">".  When we find one, we remove it and the
  following word from the word list.  The arguments following the redirection
  symbols and the remaining words are returned to our caller.  However, it's
  an error to end a word list with a redirection or for the same redirection
  to appear twice.
-}

parseRedirection :: [String] -> IO (Maybe String, Maybe String, [String])
parseRedirection = redirect Nothing Nothing []
  where
    redirect inFile outFile args [] =
	return (inFile, outFile, reverse args)
    redirect inFile outFile args [arg]
      | arg == "<" || arg == ">" =
	errMsg "Missing name for redirect"	>>
	fail (userError "parse redirect")
      | otherwise =
	return (inFile, outFile, reverse (arg:args))
    redirect inFile outFile args ("<":name:more) 
      | inFile == Nothing =
	redirect (Just name) outFile args more
      | otherwise =
	errMsg "Ambiguous input redirect"	>>
	fail (userError "parse redirect")
    redirect inFile outFile args (">":name:more) 
      | outFile == Nothing =
	redirect inFile (Just name) args more
      | otherwise =
	errMsg "Ambiguous output redirect"	>>
	fail (userError "parse redirect")
    redirect inFile outFile args (arg:more) =
	redirect inFile outFile (arg:args) more

{- 
  Executing an external command is pretty simple, but what if it fails?
  Fortunately, we don't have any way to redirect stdError just yet,
  so we let it complain and then exit.
-}

exec :: String -> [String] -> IO ()
exec cmd args =
    forkProcess					>>= \ maybe_pid ->
    case maybe_pid of
      Nothing ->
       do
	dupTo myStderr stdError
	fdClose myStdin
	fdClose myStdout
	fdClose myStderr
	executeFile cmd True args Nothing		
	    `catch`
	     (\ err -> 
	       fdWrite stdError ("command not found: " ++ cmd ++ ".\n") >>
	       exitImmediately (ExitFailure 1))
      Just pid -> 
       do
        fdClose stdInput
	fdClose stdOutput
--	fdClose stdError
	getProcessStatus True False pid
        return ()

{-
    Builtins:
	cd [arg] -> change directory (default to HOME)
	exit ... -> exit successfully

    Builtins must provide their own error messages, since the main command
    loop ignores any errors.
-}

builtin :: String -> Maybe ([String] -> IO ())
builtin "cd"   = Just chdir
builtin "exit" = Just exit
builtin _      = Nothing

chdir :: [String] -> IO ()
chdir [] =
   do
    home <- getEnv "HOME"
    setCurrentDirectory home `catch` \ err -> errMsg "cd: can't go home"

chdir [dir] =
   do
    setCurrentDirectory dir `catch`  \ err -> errMsg ("cd: can't chdir to " ++ dir)
chdir _ =  errMsg "cd: too many arguments"

exit :: [String] -> IO ()
exit _ = exitWith ExitSuccess

-- Print an error message to my std error.

errMsg :: String -> IO ()
errMsg msg =
    fdWrite myStderr ("hsh: " ++ msg ++ ".\n")	>>
    return ()
