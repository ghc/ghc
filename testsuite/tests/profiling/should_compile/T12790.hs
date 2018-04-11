module T12790 (list) where

import Data.Foldable (asum)
import Text.ParserCombinators.Parsec (Parser, sepBy, try)

data Expr
  = Var Fixity String
  | App Expr Expr

data Fixity = Pref | Inf

cons, nil :: Expr
cons = Var Inf  ":"
nil  = Var Pref "[]"

brackets :: Parser a -> Parser a
brackets = undefined

symbol :: String -> Parser String
symbol = undefined

list :: Parser Expr
list = asum (map (try . brackets) plist) where
  plist = [
    foldr (\e1 e2 -> cons `App` e1 `App` e2) nil `fmap`
      (myParser False `sepBy` symbol ","),
    do e <- myParser False
       _ <- symbol ".."
       return $ Var Pref "enumFrom" `App` e,
    do e  <- myParser False
       _  <- symbol ","
       e' <- myParser False
       _  <- symbol ".."
       return $ Var Pref "enumFromThen" `App` e `App` e',
    do e  <- myParser False
       _  <- symbol ".."
       e' <- myParser False
       return $ Var Pref "enumFromTo" `App` e `App` e',
    do e   <- myParser False
       _   <- symbol ","
       e'  <- myParser False
       _   <- symbol ".."
       e'' <- myParser False
       return $ Var Pref "enumFromThenTo" `App` e `App` e' `App` e''
    ]

myParser :: Bool -> Parser Expr
myParser = undefined
