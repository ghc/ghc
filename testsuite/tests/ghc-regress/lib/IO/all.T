include ($confdir ++ "/../vanilla-test.T")

test "IOError001"	{ $stdin = "IOError001.hs" 
			  vtr("","","") }
test "finalization001" 	{ vtr("","","") }
test "hClose001" 	{ vtr("","","") }
test "hFileSize001" 	{ vtr("","","") }
test "hFileSize002" 	{ vtr("","","") }
test "hFlush001" 	{ vtr("","","") }
test "hGetBuffering001" { $stdin = "hGetBuffering001.hs" 
			  vtr("","","") }
test "hGetChar001" 	{ vtr("","","") }
test "hGetLine001" 	{ $stdin = "hGetLine001.hs"
			  vtr("-cpp","","") }
test "hGetPosn001" 	{ vtr("-cpp","","") }
test "hIsEOF001" 	{ vtr("","","") }
test "hIsEOF002" 	{ vtr("-cpp","","") }
test "hReady001" 	{ vtr("-cpp","","") }
test "hSeek001" 	{ vtr("-cpp","","") }
test "hSeek002" 	{ vtr("-cpp","","") }
test "hSeek003" 	{ vtr("-cpp","","") }
test "hSeek004" 	{ vtr("-cpp","","") }
test "hSetBuffering002" { $stdin = "hSetBuffering002.hs" 
			  vtr("","","") }
test "hSetBuffering003" { $stdin = "hSetBuffering003.hs" 
			  vtr("","","") }
test "ioeGetErrorString001" { vtr("-cpp","","") }
test "ioeGetFileName001" { vtr("-cpp","","") }
test "ioeGetHandle001"	{ vtr("-cpp","","") }
test "isEOF001" 	{ vtr("","","") }
test "misc001" 		{ vtr("","misc001.hs misc001.out","") }
test "openFile001" 	{ vtr("","","") }
test "openFile002" 	{ vtr("","","1") }
test "openFile003" 	{ vtr("","","") }
test "openFile004" 	{ vtr("","","") }
test "openFile005" 	{ vtr("","","") }
test "openFile006" 	{ vtr("","","") }
test "openFile007" 	{ vtr("","","") }
test "putStr001" 	{ vtr("","","") }
test "readFile001" 	{ vtr("","","") }
test "readwrite001" 	{ vtr("-cpp","","") }
test "readwrite002" 	{ $stdin = "readwrite002.hs" 
			  vtr("-cpp","","") }
