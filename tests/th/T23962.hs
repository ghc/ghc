{-# LANGUAGE Haskell2010, KindSignatures, StarIsType, TemplateHaskell #-}

import Data.Typeable (Proxy (Proxy))
import Language.Haskell.TH (runQ)
import Language.Haskell.TH.Ppr (pprint)

main =
  runQ [|typeOf (Proxy :: Proxy *)|]
  >>= putStrLn . pprint
