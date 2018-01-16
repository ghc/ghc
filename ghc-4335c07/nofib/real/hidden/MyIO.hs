module MyIO(getFilename,process) where
import Numbers
import Vectors
import System.IO--1.3
import System.Environment(getArgs)--1.3

type InputCont = [String] -> IO ()

getFilename :: (String -> InputCont) -> InputCont
getFilename success inp = 
	getArgs >>= \ args ->
	case  args  of
		"help":_ -> usage
		[filename] -> success filename inp
		[] -> fromInp inp
		_ -> usage
  where
	test filename inp ('t':'r':_) = success filename inp
	test filename inp ('f':'r':_) = success filename inp
	test filename inp _ = 
		hPutStr stderr ("Can not read: "++filename++"\n") >>
		hPutStr stderr "Give the filename of an object: " >>
		fromInp inp
	fromInp = error "fromInp"
{-OLD:
	fromInp [] = return ()
	fromInp (filename:rest) =
		statusFile filename >>
		test filename rest
-}

usage = hPutStr stderr "Usage: hiddenline [filename of object]\n"


getDirection,getit :: (Vector -> InputCont) -> InputCont
getDirection success inp =
	hPutStr stderr ("Give a view direction in the form of: x,y,z\n"++
			   "or 'quit' to stop\n") >>
	getit success inp

getit success          [] = return ()
getit success ("quit":ls) = return ()
getit success      (l:ls) =
 case reads ("vec ["++l++"]") of
  [(v,_)] -> success v ls
  _       -> hPutStr stderr "again: " >> getit success ls


process :: (Vector -> String -> String) -> String -> InputCont
process f filename =
	getDirection 
                (\ viewdir ls ->
		   readFile filename >>= \ cs ->
		   printFrom viewdir (process f filename) cs ls
		)
	where printFrom viewdir cont cs ls =
		putStr (f viewdir cs) >> cont ls
