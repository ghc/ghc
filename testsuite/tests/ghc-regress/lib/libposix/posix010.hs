import Posix

main =
    getUserEntryForName "sof"     >>= \ sof ->
    getUserEntryForName "partain" >>= \ partain ->
    putStr (ue2String sof) >>
    putChar '\n' >>    
    putStr (ue2String partain) >>
    putChar '\n' >>    
    getUserEntryForID (userID sof) >>= \ muid ->
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
