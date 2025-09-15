import Lib
import IfaceSharingName
import GHC.Iface.Binary

main :: IO ()
main = do
  sz <- testSize MaximumCompression (concat (replicate 1000 names))
  writeFile "FULLSIZE" (show sz)
  sz <- testSize SafeExtraCompression (concat (replicate 1000 names))
  writeFile "MEDIUMSIZE" (show sz)
  sz <- testSize NormalCompression (concat (replicate 1000 names))
  writeFile "NORMALSIZE" (show sz)
