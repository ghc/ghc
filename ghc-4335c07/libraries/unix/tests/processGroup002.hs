import System.Posix.Process

main = do
	pid <- getProcessID
	ppid <- getParentProcessID
	ppgid <- getProcessGroupIDOf ppid
	-- join the parent process
	putStr "Testing joinProcessGroup: "
	joinProcessGroup ppgid
	pgid1 <- getProcessGroupID
	print $ ppgid == pgid1
	-- be a leader
	putStr "Testing createProcessGroupFor: "
	createProcessGroupFor pid
	pgid2 <- getProcessGroupID
	print $ pid == fromIntegral pgid2
	-- and join the parent again
	putStr "Testing setProcessGroupIDOf: "
	setProcessGroupIDOf pid ppgid
	pgid3 <- getProcessGroupID
	print $ ppgid == pgid3
