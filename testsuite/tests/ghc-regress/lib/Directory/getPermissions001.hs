import Directory

main = do
  p <- getPermissions "."
  print p
  p <- getPermissions "getPermissions001.hs"
  print p
#ifndef i386_unknown_mingw32
  p <- getPermissions "getPermissions001"
#else
  p <- getPermissions "getPermissions001.exe"
#endif
  print p
