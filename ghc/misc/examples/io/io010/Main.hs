import LibDirectory (getCurrentDirectory, setCurrentDirectory, 
                     createDirectory, removeDirectory, getDirectoryContents)

main =
    getCurrentDirectory >>= \ oldpwd ->
    createDirectory "foo" >>
    setCurrentDirectory "foo" >> 
    getDirectoryContents "." >>= \ [n1, n2] ->
    if dot n1 && dot n2 then
        setCurrentDirectory oldpwd >>
        removeDirectory "foo" >>
        putStr "Okay\n"
    else
        fail "Oops"


dot :: String -> Bool
dot "." = True
dot ".." = True
dot _ = False
