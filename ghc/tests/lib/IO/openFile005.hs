-- !!! test multiple-reader single-writer locking semantics

import IO

file1 = "openFile005.out1"
file2 = "openFile005.out2"

main = do
  -- two writes (should fail)
  h <- openFile file1 WriteMode
  try (openFile file1 WriteMode) >>= print
  hClose h

  -- write and an append (should fail)
  h <- openFile file1 WriteMode
  try (openFile file1 AppendMode) >>= print
  hClose h

  -- read/write and a write (should fail)
  h <- openFile file1 ReadWriteMode
  try (openFile file1 WriteMode) >>= print
  hClose h

  -- read and a read/write (should fail)
  h <- openFile file1 ReadMode
  try (openFile file1 ReadWriteMode) >>= print
  hClose h

  -- write and a read (should fail)
  h <- openFile file1 WriteMode
  try (openFile file1 ReadMode) >>= print
  hClose h

  -- two writes, different files (silly, but should succeed)
  h1 <- openFile file1 WriteMode
  h2 <- openFile file2 WriteMode
  hClose h1
  hClose h2

  -- two reads, should succeed
  h1 <- openFile file1 ReadMode
  h2 <- openFile file1 ReadMode
  hClose h1
  hClose h2
