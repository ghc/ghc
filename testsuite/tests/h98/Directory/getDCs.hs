import Directory

main = do
  -- We can no longer first ask what the current dir is and
  -- then read its contents, because the results thereof depend
  -- on the current directory at the time the test run was 
  -- started, and that can't be assumed to be any particular value.
  fs <- getDirectoryContents "/usr"
  mapM_ (\f-> putStrLn ("    "++f)) fs
