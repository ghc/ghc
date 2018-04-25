import System.Posix.Process

main = do
	pgid <- getProcessGroupID
	pgid' <- getProcessGroupIDOf =<< getProcessID
	putStr "Testing getProcessGroupID == getProcessGroupIDOf =<< getProcessID: "
	print $ pgid == pgid'
