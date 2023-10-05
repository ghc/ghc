-- Same as T14471 but also enables RebindableSyntax, since that's a
-- tricky case.

{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RebindableSyntax #-}
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
