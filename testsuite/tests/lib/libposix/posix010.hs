import System.Posix

main = do
    root <- getUserEntryForName "root"
    putStrLn (ue2String root)
    root' <- getUserEntryForID (userID root)
    putStrLn (ue2String root')
    if homeDirectory root == homeDirectory root' &&
       userShell     root == userShell     root'
        then putStrLn "OK"
        else putStrLn "Mismatch"

ue2String ue = concat [name, ":", show uid, ":", show gid]
    where name = userName ue
          uid = userID ue
          gid = userGroupID ue
