import Directory (getCurrentDirectory, setCurrentDirectory, 
                     createDirectory, removeDirectory, getDirectoryContents)

main = do
    oldpwd <- getCurrentDirectory
    createDirectory "foo"
    setCurrentDirectory "foo"
    ~[n1, n2] <- getDirectoryContents "."
    if dot n1 && dot n2 
     then do
        setCurrentDirectory oldpwd
        removeDirectory "foo"
        putStr "Okay\n"
      else
        ioError (userError "Oops")

dot :: String -> Bool
dot "." = True
dot ".." = True
dot _ = False
