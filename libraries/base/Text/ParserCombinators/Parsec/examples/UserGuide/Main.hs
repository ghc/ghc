-----------------------------------------------------------
-- Daan Leijen (c) 2000, daan@cs.uu.nl
-----------------------------------------------------------
module Main where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Language 

		
    
-----------------------------------------------------------
-- 
-----------------------------------------------------------
run :: Show a => Parser a -> String -> IO ()
run p input
        = case (parse p "" input) of
            Left err -> do{ putStr "parse error at "
                          ; print err
                          }
            Right x  -> print x


runLex :: Show a => Parser a -> String -> IO ()
runLex p 
        = run (do{ whiteSpace lang
                 ; x <- p
                 ; eof
                 ; return x
                 }
              ) 

-----------------------------------------------------------
-- Sequence and choice 
-----------------------------------------------------------
simple  :: Parser Char
simple  = letter

openClose :: Parser Char
openClose = do{ char '('
              ; char ')'
              }
            
matching:: Parser ()
matching= do{ char '('
            ; matching
            ; char ')'
            ; matching
            }
        <|> return ()
        

-- Predictive parsing
testOr  =   do{ char '('; char 'a'; char ')' }
        <|> do{ char '('; char 'b'; char ')' }
        
testOr1 = do{ char '('
            ; char 'a' <|> char 'b'
            ; char ')'
            }
            
testOr2 =   try (do{ char '('; char 'a'; char ')' })
        <|> do{ char '('; char 'b'; char ')' }                    
        
   
-- Semantics        
nesting :: Parser Int
nesting = do{ char '('
            ; n <- nesting
            ; char ')'
            ; m <- nesting
            ; return (max (n+1) m)
            }
        <|> return 0        

word1   :: Parser String
word1   = do{ c  <- letter
            ; do{ cs <- word1
                ; return (c:cs)
                }
              <|> return [c]
            }  

-----------------------------------------------------------
-- 
-----------------------------------------------------------

word    :: Parser String
word    = many1 (letter <?> "") <?> "word"

sentence    :: Parser [String]
sentence    = do{ words <- sepBy1 word separator
                ; oneOf ".?!" <?> "end of sentence"
                ; return words
                }
                
separator   :: Parser ()
separator   = skipMany1 (space <|> char ',' <?> "")


-----------------------------------------------------------
-- Tokens
-----------------------------------------------------------
lang    = makeTokenParser 
            (haskellStyle{ reservedNames = ["return","total"]})


-----------------------------------------------------------
-- 
-----------------------------------------------------------
expr    = buildExpressionParser table factor
        <?> "expression"
        
table   = [[op "*" (*) AssocLeft, op "/" div AssocLeft]
          ,[op "+" (+) AssocLeft, op "-" (-) AssocLeft]
          ]
        where
          op s f assoc
             = Infix (do{ symbol lang s; return f} <?> "operator") assoc

factor  =   parens lang expr
        <|> natural lang
        <?> "simple expression"


test1   = do{ n <- natural lang
            ; do{ symbol lang "+"
                ; m <- natural lang
                ; return (n+m) 
                } 
            <|> return n
            }

-----------------------------------------------------------
--
-----------------------------------------------------------
{-
receipt ::= product* total
product ::= "return" price ";"
		  | identifier price ";"		  
total   ::= price "total"
price   ::= natural "." digit digit
-}

receipt :: Parser Bool
receipt	= do{ ps <- many produkt
	    ; p  <- total
	    ; return (sum ps == p)
	    }
			
produkt = do{ reserved lang "return"
	    ; p <- price
	    ; semi lang
	    ; return (-p)
	    }
      <|> do{ identifier lang
       	    ; p  <- price
      	    ; semi lang
      	    ; return p
      	    }
      <?> "product"

total	= do{ p <- price
	    ; reserved lang "total"
	    ; return p
	    }
	
price   :: Parser Int			
price   = lexeme lang (
	  do{ ds1 <- many1 digit
	    ; char '.'
	    ; ds2 <- count 2 digit
	    ; return (convert 0 (ds1 ++ ds2))			
	    })
	  <?> "price"
	  where
	    convert n []     = n
	    convert n (d:ds) = convert (10*n + digitToInt d) ds
		
			
