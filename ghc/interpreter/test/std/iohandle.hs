--!!! Testing File I/O operations and errors

import IO

testFile    = "test/iohandle.tst"
unreadable  = "test/unreadable.tst"
unwritable  = "test/unwritable.tst"
nonexistent = "test/nonexistent.tst"

-- Handle free ops
a1 = writeFile testFile (show [1..10])
a2 = readFile testFile >>= \ s -> putStr s
a3 = appendFile testFile (show [11..20])
a4 = readFile testFile >>= \ s -> putStr s

-- Same stuff - but using handle-based operations
b1 = openFile testFile WriteMode  >>= \ h ->
     hPutStr h (show [1..10])
b2 = openFile testFile ReadMode   >>= \ h ->
     hGetContents h               >>= \ s ->
     putStr s
b3 = openFile testFile AppendMode >>= \ h ->
     hPutStr h (show [11..20])     
b4 = openFile testFile ReadMode   >>= \ h ->
     hGetContents h               >>= \ s ->
     putStr s

-- Miscellaneous little functions
c1 = openFile testFile WriteMode           >>= \ h ->
     mapM_ (hPutChar h) (show [1..10])     >>
     hClose h
c2 = openFile testFile ReadMode   >>= \ h ->
     let loop = 
           hGetChar h >>= \ c ->
           putChar c  >>
           loop
     in
     loop  :: IO ()
c3 = openFile testFile AppendMode          >>= \ h ->
     hPutStr h (show [11..20])             >>
     hClose h
c4 = openFile testFile ReadMode   >>= \ h ->
     let loop = 
           hGetChar h >>= \ c ->
           putChar c  >>
           loop
     in
     loop `catch` (\err -> if isEOFError err then return () else fail err)
-- If this function raises an uncaught EOF error, then hIsEOF probably
-- implements ANSI C feof semantics which is quite different from 
-- Haskell 1.3 semantics (but much easier to implement).
c5 = openFile testFile ReadMode   >>= \ h ->
     let loop = 
	   hIsEOF h >>= \ eof ->
           if eof then return () else
           hGetChar h >>= \ c ->
           putChar c  >>
           loop
     in
     loop :: IO ()
    
c6 = openFile testFile ReadMode  >>= \ h ->
     hFlush h                    >>
     hGetContents h              >>= \ s ->
     putStr s

-- should print first 10 characters of file twice
c7 = openFile testFile ReadMode  >>= \ h ->
     hGetContents h              >>= \ s ->
     putStr (take 10 s)          >>
     hClose h                    >>
     putStr s


-- Deliberately trying to trigger IOErrors:

-- Note: Linux allows a file to be opened twice
d1 = openFile testFile WriteMode  >>= \ h1 ->
     openFile testFile WriteMode  >>= \ h2 ->
     let x = [h1,h2] in -- try to make sure both pointers remain live
     return ()

d2 = openFile testFile WriteMode  >>= \ h ->
     hGetContents h               >>= \ s ->
     putStr s

d3 = openFile testFile ReadMode  >>= \ h ->
     hPutStr h (show [5..10])

-- This should succeed
d4 = openFile unreadable WriteMode  >>= \ h ->
     return ()

-- This should fail
d5 = openFile unreadable ReadMode  >>= \ h ->
     return ()

-- This should succeed
d6 = openFile unwritable ReadMode  >>= \ h ->
     return ()

-- This should fail
d7 = openFile unwritable WriteMode  >>= \ h ->
     return ()

d8 = openFile testFile ReadMode  >>= \ h ->
     hClose h                    >>
     hGetContents h              >>= \ s ->
     putStr s

d9 = openFile testFile ReadMode  >>= \ h ->
     hClose h                    >>
     hClose h

-- should fail
d10 = openFile testFile ReadMode  >>= \ h ->
      hGetContents h              >>= \ s1 ->
      hGetContents h              >>= \ s2 ->
      putStr s1                   >>
      putStr s2



