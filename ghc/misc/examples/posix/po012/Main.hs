import Posix
import IO -- 1.3

main =
    createFile "po012.out" stdFileMode >>= \ fd ->
    installHandler processStatusChanged (Catch (reap1 fd)) Nothing >>
    ls >>
    awaitSignal Nothing

ls =
    runProcess "ls" ["-l","po012.out"] Nothing Nothing Nothing Nothing Nothing

reap1 fd =
    getAnyProcessStatus True False >>
    installHandler processStatusChanged (Catch (reap2 fd)) Nothing >>
    writeChannel fd (take 666 (repeat 'x')) >>
    ls >>
    awaitSignal Nothing
    
reap2 fd =
    getAnyProcessStatus True False >>
    installHandler processStatusChanged (Catch (reap3 fd)) Nothing >>
    setFileMode "po012.out" 
	(foldr1 unionFileModes [ownerReadMode,ownerWriteMode,groupReadMode,otherReadMode]) >>
    ls >>
    awaitSignal Nothing
    
reap3 fd =
    getAnyProcessStatus True False >>
    installHandler processStatusChanged (Catch (reap4 fd)) Nothing >>
    setFileTimes "po012.out" 0 0 >>
    ls >>
    awaitSignal Nothing
    
reap4 fd =
    getAnyProcessStatus True False >>
    installHandler processStatusChanged (Catch (reap5 fd)) Nothing >>
    removeLink "po012.out" >>
    ls >>
    awaitSignal Nothing

reap5 fd =
    getAnyProcessStatus True False >>
    seekChannel fd SeekFromEnd 0 >>= \ bytes ->
    if bytes == 666 then
	seekChannel fd AbsoluteSeek 0 >>
	readChannel fd 1024 >>= \ (str, _) ->
	if str == (take 666 (repeat 'x')) then
	    putStr "Okay\n"
	else
	    putStr "Read failed\n"
    else
	putStr "Seek returned wrong size\n"
