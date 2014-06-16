{-# LANGUAGE DeriveDataTypeable #-}
module Expr where

import Data.Generics
import Language.Haskell.TH as TH
import Language.Haskell.TH.Quote

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char

data Expr  =  IntExpr Integer
           |  AntiIntExpr String
           |  BinopExpr BinOp Expr Expr
           |  AntiExpr String
    deriving(Typeable, Data)

data BinOp  =  AddOp
            |  SubOp
            |  MulOp
            |  DivOp
    deriving(Typeable, Data)

eval :: Expr -> Integer
eval (IntExpr n)        = n
eval (BinopExpr op x y) = (opToFun op) (eval x) (eval y)
  where
    opToFun AddOp = (+)
    opToFun SubOp = (-)
    opToFun MulOp = (*)
    opToFun DivOp = (div)

small   = lower <|> char '_'
large   = upper
idchar  = small <|> large <|> digit <|> char '\''

lexeme p    = do{ x <- p; spaces; return x  }
symbol name = lexeme (string name)
parens p    = between (symbol "(") (symbol ")") p

_expr :: CharParser st Expr
_expr   = term   `chainl1` mulop

term :: CharParser st Expr
term    = factor `chainl1` addop

factor :: CharParser st Expr
factor  = parens _expr <|> integer <|> anti

mulop   =   do{ symbol "*"; return $ BinopExpr MulOp }
        <|> do{ symbol "/"; return $ BinopExpr DivOp }

addop   =   do{ symbol "+"; return $ BinopExpr AddOp }
        <|> do{ symbol "-"; return $ BinopExpr SubOp }

integer :: CharParser st Expr
integer  = lexeme $ do{ ds <- many1 digit ; return $ IntExpr (read ds) }

anti   = lexeme $
         do  symbol "$"
             c <- small
             cs <- many idchar
             return $ AntiIntExpr (c : cs)

parseExpr :: Monad m => TH.Loc -> String -> m Expr
parseExpr (Loc {loc_filename = file, loc_start = (line,col)}) s =
    case runParser p () "" s of
      Left err  -> fail $ show err
      Right e   -> return e
  where
    p = do  pos <- getPosition
            setPosition $ setSourceName (setSourceLine (setSourceColumn pos col) line) file
            spaces
            e <- _expr
            eof
            return e

expr = QuasiQuoter { quoteExp = parseExprExp, quotePat = parseExprPat }

parseExprExp :: String -> Q Exp
parseExprExp s =  do  loc <- location
                      expr <- parseExpr loc s
                      dataToExpQ (const Nothing `extQ` antiExprExp) expr

antiExprExp :: Expr -> Maybe (Q Exp)
antiExprExp  (AntiIntExpr v)  = Just $ appE  (conE (mkName "IntExpr"))
                                                (varE (mkName v))
antiExprExp  (AntiExpr v)     = Just $ varE  (mkName v)
antiExprExp  _                = Nothing

parseExprPat :: String -> Q Pat
parseExprPat s =  do  loc <- location
                      expr <- parseExpr loc s
                      dataToPatQ (const Nothing `extQ` antiExprPat) expr

antiExprPat :: Expr -> Maybe (Q Pat)
antiExprPat  (AntiIntExpr v)  = Just $ conP  (mkName "IntExpr")
                                                [varP (mkName v)]
antiExprPat  (AntiExpr v)     = Just $ varP  (mkName v)
antiExprPat  _                = Nothing
