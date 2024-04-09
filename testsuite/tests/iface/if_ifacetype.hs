import Lib
import IfaceSharingIfaceType
import GHC.Iface.Binary

main :: IO ()
main = do
  sz <- testSize MaximalCompression (concat (replicate 500 types))
  writeFile "FULLSIZE" (show sz)
  sz <- testSize SafeExtraCompression (concat (replicate 500 types))
  writeFile "MEDIUMSIZE" (show sz)
  sz <- testSize NormalCompression (concat (replicate 500 types))
  writeFile "NORMALSIZE" (show sz)

