import Posix

main =
    getGroupEntryForName "grasp" >>= \ grasp ->
    getGroupEntryForName "staff" >>= \ staff ->
    putStr (ge2String grasp) >>
    putChar '\n' >>    
    putStr (ge2String staff) >>
    putChar '\n' >>    
    getGroupEntryForID (groupID grasp) >>= \ guid ->
    getGroupEntryForID (groupID staff) >>= \ suid ->
    putStr (ge2String guid) >>
    putChar '\n' >>    
    putStr (ge2String suid) >>
    putChar '\n'

ge2String ge =
    name ++ (':' : (show gid) ++ (':' : members))
  where
    name = groupName ge
    gid = groupID ge
    members = foldr (\x y -> x ++ (',' : y)) "" (groupMembers ge)
