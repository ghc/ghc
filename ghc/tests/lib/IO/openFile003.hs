import IO

-- !!! Open a directory (should fail)

main = do
  r <- try (openFile "." ReadMode)
  print r
  r <- try (openFile "." WriteMode)
  print r
  r <- try (openFile "." AppendMode)
  print r
  r <- try (openFile "." ReadWriteMode)
  print r
