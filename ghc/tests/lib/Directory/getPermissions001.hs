import Directory

main = do
  p <- getPermissions "."
  print p
  p <- getPermissions "io034.hs"
  print p
  p <- getPermissions "io034.bin"
  print p
