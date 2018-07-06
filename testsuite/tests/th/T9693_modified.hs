module T9693 where
import Language.Haskell.TH

stuff = do
  let x = mkName "X"
  -- x <- newName "X"
  sequence $ [dataD (return []) x [] Nothing [
      normalC x []
    ] []]
