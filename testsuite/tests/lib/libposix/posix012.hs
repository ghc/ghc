import Posix
import IO -- 1.3

main =
    createFile "po012.out" stdFileMode >>= \ fd ->
    installHandler processStatusChanged (Catch (reap1 fd)) Nothing >>
    ls >>
    awaitSignal Nothing

ls =
    runProcess "ls" ["-l"] Nothing Nothing Nothing Nothing Nothing

reap1 fd =
    hPutStrLn stderr "Reaper1"         >>
    getAnyProcessStatus True False     >>
    installHandler processStatusChanged (Catch (reap2 fd)) Nothing >>
    fdWrite fd (take 666 (repeat 'x')) >>
    ls                                 >>
    awaitSignal Nothing
    
reap2 fd =
    hPutStrLn stderr "Reaper2"     >>
    getAnyProcessStatus True False >>
    installHandler processStatusChanged (Catch (reap3 fd)) Nothing >>
    setFileMode "po012.out" 
	(foldr1 unionFileModes [ownerReadMode,ownerWriteMode,groupReadMode,otherReadMode]) >>
    ls >>
    awaitSignal Nothing
    
reap3 fd =
    hPutStrLn stderr "Reaper3"     >>
    getAnyProcessStatus True False >>
    installHandler processStatusChanged (Catch (reap4 fd)) Nothing >>
    setFileTimes "po012.out" 0 0 >>
    ls >>
    awaitSignal Nothing
    
reap4 fd =
    hPutStrLn stderr "Reaper4"     >>
    getAnyProcessStatus True False >>
    installHandler processStatusChanged (Catch (reap5 fd)) Nothing >>
    --removeLink "po012.out" >>
    ls >>
    awaitSignal Nothing

reap5 fd =
    hPutStrLn stderr "Reaper5"     >>
    getAnyProcessStatus True False >>
    fdSeek fd SeekFromEnd 0        >>= \ bytes ->
    if bytes == 666 then
	fdSeek fd AbsoluteSeek 0   >>
	hPutStrLn stderr "Reaper5"     >>
	fdRead fd 666             >>= \ (str, _) ->
	if str == (take 666 (repeat 'x')) then
	    putStr "Okay\n"
	else
	    putStr "Read failed\n"
    else
	putStr "Seek returned wrong size\n"
