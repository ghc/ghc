import Posix

main =
    getUserEntryForName "mattson" >>= \ mattson ->
    getUserEntryForName "partain" >>= \ partain ->
    putStr (ue2String mattson) >>
    putChar '\n' >>    
    putStr (ue2String partain) >>
    putChar '\n' >>    
    getUserEntryForID (userID mattson) >>= \ muid ->
    getUserEntryForID (userID partain) >>= \ puid ->
    putStr (ue2String muid) >>
    putChar '\n' >>    
    putStr (ue2String puid) >>
    putChar '\n'

ue2String ue =
    name ++ (':' : (show uid) ++ (':' : (show gid) ++ (':' : home ++ (':' : shell))))
  where
    name = userName ue
    uid = userID ue
    gid = userGroupID ue
    home = homeDirectory ue
    shell = userShell ue
