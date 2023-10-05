{-# LANGUAGE GADTs, TemplateHaskell, TypeFamilies #-}

import Language.Haskell.TH (runQ)
import Language.Haskell.TH.Ppr (pprint)

main =
  runQ [d|
         class C a where {data D a; f :: a -> D a};
         instance C Int where {data D Int where {C1 :: Int -> D Int; C2 :: D Int}; f = C1}
       |]
  >>= putStrLn . pprint
