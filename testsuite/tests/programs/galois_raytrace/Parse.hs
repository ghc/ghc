-- Copyright (c) 2000 Galois Connections, Inc.
-- All rights reserved.  This software is distributed as
-- free software under the license in the file "LICENSE",
-- which is included in the distribution.

module Parse where

import Data.Char
import Text.ParserCombinators.Parsec hiding (token)

import Data


program :: Parser Code
program =
  do { whiteSpace
     ; ts <- tokenList
     ; eof
     ; return ts
     }

tokenList :: Parser Code
tokenList = many token <?> "list of tokens"

token :: Parser GMLToken
token =
       do { ts <- braces   tokenList          ; return (TBody ts) }
  <|>  do { ts <- brackets tokenList          ; return (TArray ts) }
  <|> (do { s  <- gmlString                   ; return (TString s) } <?> "string")
  <|> (do { t <- pident False                 ; return t }           <?> "identifier")
  <|> (do { char '/'   -- No whitespace after slash
          ; t <- pident True                  ; return t } <?> "binding identifier")
  <|> (do { n <- number                       ; return n } <?> "number")

pident :: Bool -> Parser GMLToken
pident rebind =
  do { id <- ident
     ; case (lookup id opTable) of
       Nothing -> if rebind then return (TBind id) else return (TId id)
       Just t  -> if rebind then error ("Attempted rebinding of identifier " ++ id) else return t
     }

ident :: Parser String
ident = lexeme $
  do { l <- letter
     ; ls <- many (satisfy (\x -> isAlphaNum x || x == '-' || x == '_'))
     ; return (l:ls)
     }

gmlString :: Parser String
gmlString = lexeme $ between (char '"') (char '"') (many (satisfy (\x -> isPrint x && x /= '"')))

-- Tests for numbers
-- Hugs breaks on big exponents (> ~40)
test_number = "1234 -1234 1 -0 0" ++
              " 1234.5678 -1234.5678 1234.5678e12 1234.5678e-12 -1234.5678e-12" ++
              " -1234.5678e12 -1234.5678E-12 -1234.5678E12" ++
              " 1234e11 1234E33 -1234e33 1234e-33" ++
              " 123e 123.4e 123ee 123.4ee 123E 123.4E 123EE 123.4EE"


-- Always int or real
number :: Parser GMLToken
number = lexeme $
  do { s <- optSign
     ; n <- decimal
     ;     do { string "."
              ; m <- decimal
              ; e <- option "" exponent'
              ; return (TReal (read (s ++ n ++ "." ++ m ++ e)))  -- FIXME: Handle error conditions
              }
       <|> do { e <- exponent'
              ; return (TReal (read (s ++ n ++ ".0" ++ e)))
              }
       <|> do { return (TInt (read (s ++ n))) }
     }

exponent' :: Parser String
exponent' = try $
  do { e <- oneOf "eE"
     ; s <- optSign
     ; n <- decimal
     ; return (e:s ++ n)
     }

decimal = many1 digit

optSign :: Parser String
optSign = option "" (string "-")


------------------------------------------------------
-- Library for tokenizing.

braces   p = between (symbol "{") (symbol "}") p
brackets p = between (symbol "[") (symbol "]") p

symbol name = lexeme (string name)

lexeme p = do{ x <- p; whiteSpace; return x  }

whiteSpace  = skipMany (simpleSpace <|> oneLineComment <?> "")
  where simpleSpace = skipMany1 (oneOf " \t\n\r\v")
        oneLineComment =
            do{ string "%"
              ; skipMany (noneOf "\n\r\v")
              ; return ()
              }


------------------------------------------------------------------------------

rayParse :: String -> Code
rayParse is = case (parse program "<stdin>" is) of
              Left err -> error (show err)
              Right x  -> x

rayParseF :: String -> IO Code
rayParseF file =
  do { r <- parseFromFile program file
     ; case r of
       Left err -> error (show err)
       Right x  -> return x
     }

run :: String -> IO ()
run is = case (parse program "" is) of
         Left err -> print err
         Right x  -> print x

runF :: IO ()
runF =
  do { r <- parseFromFile program "simple.gml"
     ; case r of
       Left err -> print err
       Right x  -> print x
     }
