{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH
import Prelude

main = putStrLn $(do
  expr <- [|
    do x <- getLine
       y <- getLine
       pure (x, y)
    |]
  stringE (pprint expr))
