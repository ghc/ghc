import GHC.Foreign
import GHC.IO.Encoding
import qualified Foreign.C.String as F

main = withCStringLen utf8 "\x1F424" $ \csl -> do
  s <- F.peekCAStringLen csl
  print s
  s <- peekCStringLen utf8 csl
  print s
