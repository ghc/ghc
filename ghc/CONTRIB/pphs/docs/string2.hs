main = appendChan stdout "please type a filename\n" exit (
       readChan stdin exit (\ userInput ->
       let (name : _) = lines userInput in
       appendChan stdout name exit (
       readFile name (\ ioerror -> appendChan stdout
				   "can't open file" exit done)
		     (\ contents ->
       appendChan stdout contents exit done))))
