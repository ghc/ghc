import Directory

main = do
  d <- getCurrentDirectory
  putStrLn ("Current Dir: "++d)
  fs <- getDirectoryContents d
  mapM_ (\f-> putStrLn ("    "++f)) fs
