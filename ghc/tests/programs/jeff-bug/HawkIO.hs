module HawkIO
  (
	 hawkMain
  ) where

import IO
import Memory

-- Begin Signature ------------------------------------------------------
{-
Given a filename, and function from a memory state and a starting point to a 
an output, "hawkMain" will open the file and parse it and apply the
function to it.
-}


hawkMain :: (Read a,Read w) => String -> ((MemoryState w a,w) -> IO b) -> IO b
-- End Signature ------------------------------------------------------


hawkMain filename f = do {ps <- readInitFile filename; f ps}


readInitFile :: (Read a,Read w) => String -> IO (MemoryState w a,w)
readInitFile infile =
 do{ h <- openFile infile ReadMode
   ; s <- hGetContents h
   ; return (read s)
   }
