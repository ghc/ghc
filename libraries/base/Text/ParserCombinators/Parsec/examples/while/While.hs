-------------------------------------------------------------
-- Parser for WHILE from Nielson, Nielson and Hankin
-- and various other sources.
-------------------------------------------------------------

module While( prettyWhileFromFile ) where

import WhileAS
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language( javaStyle )


prettyWhileFromFile fname
  = do{ input <- readFile fname
      ; putStr input
      ; case parse program fname input of
           Left err -> do{ putStr "parse error at "
                           ; print err
                           }
           Right x  -> print x
      }

--renum :: Prog -> Prog
--renum p = rn (1,p)
--rn :: (Int, Stat) -> (Int, Stat)
--rn (x,s) = case s of
--            Assign vi ae _  -> (x+1,Assign vi ae x)
--            Skip _          -> (x+1, Skip x)
--            Seq [Stat]      -> 
--            If be _ s1 s2   -> do{ (newx, newthen) <- rn (x+1,s1)
--                                 ; (newerx, newelse) <- rn (newx,s2)
--                                 ; return (newerx, If be x newthen newelse)
--                                 }
--            While be _ s    -> do{ (newx, news) <- rn (x+1,s)
--                                 ; return (newx, While be x+1 news)
--                                 }

-----------------------------------------------------------
-- A program is simply an expression.
-----------------------------------------------------------
program 
    = do{ stats <- semiSep1 stat
        ; return (if length stats < 2 then head stats else Seq stats)
        }
        
stat :: Parser Stat
stat = choice 
       [ do { reserved "skip";
              return (Skip 0)
            }
       , ifStat
       , whileStat
       , sequenceStat
       , try assignStat
       ]


assignStat :: Parser Stat
assignStat = do{ id <- identifier
               ; symbol ":="
               ; s <- aritExpr
               ; return (Assign id s 0)
               }

ifStat :: Parser Stat
ifStat = do{ reserved "if"
             ; cond <- boolExpr
             ; reserved "then"
             ; thenpart <- stat
             ; reserved "else"
             ; elsepart <- stat
             ; return (If cond 0 thenpart elsepart)
             }
             
whileStat :: Parser Stat
whileStat = do{ reserved "while"
              ; cond <- boolExpr
              ; reserved "do"
              ; body <- stat
              ; return (While cond 0 body)
              }

sequenceStat :: Parser Stat
sequenceStat = do{ stats <- parens (semiSep1 stat)
                 ; return (if length stats < 2 then head stats else Seq stats)
                 }

boolExpr:: Parser BExp
boolExpr = buildExpressionParser boolOperators relExpr

relExpr :: Parser BExp
relExpr = do{ arg1 <- aritExpr
            ; op <- choice [string "=", try (string "<>"), try (string "<="), string "<", try (string ">="), string ">"]
            ; arg2 <- aritExpr
            ; return (RelOp op arg1 arg2)
            }

aritExpr :: Parser AExp
aritExpr = buildExpressionParser aritOperators simpleArit

-- Everything mapping bools to bools
boolOperators =
    [ [ prefix "not"]
    , [ opbb "and" AssocRight ] -- right for shortcircuit
    , [ opbb "or" AssocRight ] -- right for shortcircuit
    ]
    where
      opbb name assoc   = Infix (do{ reservedOp name
                                   ; return (\x y -> BOp name x y) 
                                   }) assoc
      prefix name       = Prefix  (do{ reservedOp name
                                  ; return (\x -> BUnOp name x)
                                  })                                      

-- Everything mapping pairs of ints to ints
aritOperators =
    [ [ op "*"  AssocLeft, op "/"  AssocLeft ]
    , [ op "+"  AssocLeft, op "-"  AssocLeft ]
    , [ op "&" AssocRight ] -- bitwise and delivering an int
    , [ op "|" AssocRight ] -- bitwise or delivering an int
    ]
    where
      op name assoc   = Infix (do{ reservedOp name
                                  ; return (\x y -> AOp name x y) 
                                  }) assoc


simpleArit = choice [ intLiteral
                    , parens aritExpr
                    , variable
                    ]

simpleBool = choice [ boolLiteral
                    , parens boolExpr
                    ]

boolLiteral = do{ reserved "false"
               ; return (BoolLit True)
               }
             <|>  
             do{ reserved "true"
               ; return (BoolLit False)
               }

intLiteral = do{ i <- integer; return (IntLit i) }
variable = do{ id <- identifier
             ; return (Var id)
             }
             

-----------------------------------------------------------
-- The lexer
-----------------------------------------------------------
lexer     = P.makeTokenParser whileDef

whileDef  = javaStyle
          { -- Kept the Java single line comments, but officially the language has no comments
            P.reservedNames  = [ "true", "false", "do", "else", "not",
                               "if", "then", "while", "skip"
                               -- , "begin", "proc", "is", "end", "val", "res", "malloc" 
                              ]
          , P.reservedOpNames= [ "and", "or", "not", "<", "<=", ">", ">=", ":=", "+", "&", "-", "/"]
          , P.opLetter       = oneOf (concat (P.reservedOpNames whileDef))
          , P.caseSensitive  = False
          }

parens          = P.parens lexer    
braces          = P.braces lexer    
semiSep1        = P.semiSep1 lexer    
whiteSpace      = P.whiteSpace lexer    
symbol          = P.symbol lexer    
identifier      = P.identifier lexer    
reserved        = P.reserved lexer    
reservedOp      = P.reservedOp lexer
integer         = P.integer lexer    
charLiteral     = P.charLiteral lexer    
stringLiteral   = P.stringLiteral lexer    
