module TupleN where
import Language.Haskell.TH

tuple :: Int -> ExpQ
tuple n = [|\list -> $(tupE (exprs [|list|])) |]
  where
    exprs list = id [infixE (Just (list))
                           (varE '(!!))
                           (Just (litE $ integerL (toInteger num)))
                      | num <- [0..(n - 1)]]