{-# LANGUAGE UnicodeSyntax   #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Language.Haskell.TH

main :: IO ()
main = do
  print $
    ∫(let matches :: PatQ -> ExpQ
          matches pat = ⟦ \x ->
            case x of
              ∫pat -> True
              _    -> False
            ⟧
      in matches [p| 'x' |]) 'y'

  print $ ∬(const [|| 'x' ||] 'y')
