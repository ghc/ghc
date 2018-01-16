module Main where

import Language.Haskell.TH

-- The recover successfully find that 'ola' is not in scope
-- and use '1' instead

y = $(recover (return (LitE (IntegerL 1)))
              (reify (mkName ("ola")) >> return (LitE (IntegerL 2))))

main = print y

