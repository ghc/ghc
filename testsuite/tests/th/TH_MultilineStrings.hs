{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH (runQ)

{-
Test the MultilineStrings proposal
https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0569-multiline-strings.rst
-}

main :: IO ()
main = do
  print =<< runQ [|
      """
      hello
      world
      """
    |]
