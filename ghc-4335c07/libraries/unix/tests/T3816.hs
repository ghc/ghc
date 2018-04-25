import System.Posix
main = do
  getAllGroupEntries >>= print . (>0) . length
  getAllGroupEntries >>= print . (>0) . length
