import Directory

main = do
  p <- getPermissions "."
  print p
  p <- getPermissions "getPermissions001.hs"
  print p
  p <- getPermissions "getPermissions001"
  print p
