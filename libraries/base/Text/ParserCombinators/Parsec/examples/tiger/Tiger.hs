-------------------------------------------------------------
-- Parser for Tiger from Appel's book on compilers.
-- Semantic checks have been omitted for now.
-- Scope rules and such are as a consequence not implemented.
-------------------------------------------------------------

module Tiger( prettyTigerFromFile ) where

import TigerAS
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language( javaStyle )
          

prettyTigerFromFile fname
  = do{ input <- readFile fname
      ; putStr input
      ; case parse program fname input of
           Left err -> do{ putStr "parse error at "
                           ; print err
                           }
           Right x  -> print x
      }

{-
main = do putStr "Parsec Tiger parser\n"
          putStr "Type filename (without suffix): "
          basename <- getLine
          tokens <- scanner False keywordstxt
                                  keywordsops
                                  specialchars
                                  opchars
                                  (basename ++ ".sl")
                                  Nothing
          let ((exprpp,proof), errors) = parse pRoot tokens
          putStr (if null errors then "" else "Errors:\n" ++ errors)
          putStr ("Result:\n" ++ (disp exprpp 140 ""))
          writeFile (basename ++ ".tex") (disp proof 500 "")
          putStr ("\nGenerated proof in file " ++ (basename ++ ".tex"))        
-}
          
-----------------------------------------------------------
-- A program is simply an expression.
-----------------------------------------------------------
program
    = do{ whiteSpace
        ; e <- expr
        ; return e
        }

----------------------------------------------------------------
-- Declarations for types, identifiers and functions
----------------------------------------------------------------
decs
    = many dec
    
dec 
    = tydec
    <|>
      vardec
    <|>
      fundec

----------------------------------------------------------------
-- Type declarations
-- int and string are predefined, but not reserved.
----------------------------------------------------------------
tydec :: Parser Declaration
tydec
    = do{ reserved "type"
	    ; tid  <- identifier
	    ; symbol "="
	    ; t <- ty
	    ; return (TypeDec tid t)
	    }

ty
    = do{ fields <- braces tyfields
        ; return (Record fields)
        }
    <|>
      do{ reserved "array"
        ; reserved "of"
        ; tid <- identifier
        ; return (Array tid)
        }
    <|>
      do{ id <- identifier
        ; return (Var id) 
        }
          
tyfields
    = commaSep field

noType = "*"
voidType = "void"
    
field
    = do{ id <- identifier
        ; symbol ":"
        ; tid <- identifier
        ; return (TypedVar id tid)
        }
        
----------------------------------------------------------------
-- identifier declarations
-- Lacks: 11, 12
----------------------------------------------------------------
vardec
    = do{ reserved "var"
        ; id <- identifier
        ; t <- option noType (try (do{ symbol ":"
                               ; identifier
                               }))
        ; symbol ":="
        ; e <- expr
        ; return (VarDec id t e)
        }
        
----------------------------------------------------------------
-- Function declarations
----------------------------------------------------------------
fundec
    = do{ reserved "function"
        ; name <- identifier
        ; parms <- parens tyfields
        ; rettype <- option voidType (do{ symbol ":"
                                        ; identifier
                                        })
        ; symbol "="
        ; body <- expr
        ; return (FunDec name parms rettype body)
        }

----------------------------------------------------------------
-- Lvalues
-- This may not be what we want. I parse lvalues as
-- a list of dot separated array indexings (where the indexing)
-- may be absent. Possibly, we'd want the . and [] 
----------------------------------------------------------------

-- This combinator does ab* in a leftassociative way.
-- Applicable when you have a cfg rule with left recursion
-- which you might rewrite into EBNF X -> YZ*.
lfact :: Parser a -> Parser (a -> a) -> Parser a
lfact p q = do{ a <- p
              ; fs <- many q
              ; return (foldl  (\x f -> f x) a fs)
              }              
{-
chainl op expr = lfact expr (do { o <- op
                                ; e <- expr
                                ; return (`o` e)
                                })
  -}                              
lvalue = lfact variable (recordref <|> subscripted)

recordref = do{ symbol "."
              ; id <- variable
              ; return (\x -> Dot x id)
              }
subscripted = do{ indexexpr <- brackets expr
                ; return (\x -> Sub x indexexpr)
                }
        
{-  Alternatively (an lvalue is then a sequence of, possibly (mutli-)indexed, identifiers separated by dots)
lvalue :: Parser Expr
lvalue = do{ flds <- sepBy1 subscripted (symbol ".")
           ; return (if length flds < 2 then head flds else Dots flds)
           }
subscripted :: Parser Expr
subscripted = do{ id <- identifier
                ; indexes <- many (brackets expr)
                ; return (if null indexes then Ident id 
                                          else Subscripted id indexes)
                }
-}

----------------------------------------------------------------
-- All types of expression(s)
----------------------------------------------------------------

exprs = many expr

expr :: Parser Expr
expr = choice 
       [ do{ reserved "break"
           ; return Break
           }
       , ifExpr
       , whileExpr
       , forExpr
       , letExpr 
       , sequenceExpr       
       , infixExpr
--       , sequenceExpr   -- I am not sure about this one.       
       ]

recordExpr :: Parser Expr
recordExpr = do{ tid <- identifier
               ; symbol "{"
               ; fields <- commaSep1 fieldAssign
               ; symbol "}"
               ; return (RecordVal tid fields)
               }

fieldAssign :: Parser AssignField
fieldAssign = do{ id <- identifier
                ; symbol "="
                ; e <- expr
                ; return (AssignField id e)
                }
               
arrayExpr :: Parser Expr
arrayExpr = do{ tid <- identifier
              ; size <- brackets expr
              ; reserved "of"
              ; initvalue <- expr
              ; return (ArrayVal tid size initvalue)
              }
               
assignExpr :: Parser Expr
assignExpr = do{ lv <- lvalue 
               ; symbol ":="
               ; e <- expr
               ; return (Assign lv e)
               }

ifExpr :: Parser Expr
ifExpr = do{ reserved "if"
             ; cond <- expr
             ; reserved "then"
             ; thenpart <- expr
             ; elsepart <- option Skip (do{ reserved "else"; expr})
             ; return (If cond thenpart elsepart)
             }
             
whileExpr :: Parser Expr
whileExpr = do{ reserved "while"
              ; cond <- expr
              ; reserved "do"
              ; body <- expr
              ; return (While cond body)
              }

forExpr :: Parser Expr
forExpr = do{ reserved "for"
            ; id <- identifier
            ; symbol ":="
            ; lowerbound <- expr
            ; reserved "to"
            ; upperbound <- expr
            ; reserved "do"
            ; body <- expr
            ; return (For id lowerbound upperbound body)
            }
           
letExpr :: Parser Expr
letExpr = do{ reserved "let"
            ; ds <- decs
            ; reserved "in"
            ; es <- semiSep expr
            ; reserved "end"
            ; return (Let ds es)
            }

sequenceExpr :: Parser Expr
sequenceExpr = do{ exps <- parens (semiSep1 expr)
                 ; return (if length exps < 2 then head exps else Seq exps)
                 }

infixExpr :: Parser Expr                 
infixExpr = buildExpressionParser operators simpleExpr

operators =
    [ [ prefix "-"]
    , [ op "*"  AssocLeft, op "/"  AssocLeft ]
    , [ op "+"  AssocLeft, op "-"  AssocLeft ]
    , [ op "=" AssocNone, op "<>" AssocNone, op "<="  AssocNone
      , op "<" AssocNone, op ">="  AssocNone, op ">" AssocNone ]
    , [ op "&" AssocRight ] -- Right for shortcircuiting
    , [ op "|" AssocRight ] -- Right for shortcircuiting
    , [ op ":=" AssocRight ]
    ]
    where
      op name assoc   = Infix (do{ reservedOp name
                                  ; return (\x y -> Op name x y) 
                                  }) assoc
      prefix name     = Prefix  (do{ reservedOp name
                                  ; return (\x -> UnOp name x)
                                  })                                  

simpleExpr = choice [ do{ reserved "nil"
                        ; return Nil
                        }
                    , intLiteral
                    , strLiteral
                    , parens expr
                    , try funCallExpr
                    , try recordExpr
                    , try arrayExpr
                    , lvalue
                    ]

funCallExpr = do{ id <- identifier
                 ; parms <- parens (commaSep expr)
                 ; return (Apply id parms)
                 }

intLiteral = do{ i <- integer; return (IntLit i) }
strLiteral = do{ s <- stringLiteral; return (StringLit s) }
variable = do{ id <- identifier
             ; return (Ident id)
             }
             

-----------------------------------------------------------
-- The lexer
-----------------------------------------------------------
lexer     = P.makeTokenParser tigerDef

tigerDef  = javaStyle
          { -- Kept the Java single line comments, but officially the language has no comments
            P.reservedNames  = [ "array", "break", "do", "else", "end", "for", "function", 
                                 "if", "in", "let", 
                                 "nil", "of", "then", "to", "type", "var", "while" ]
          , P.reservedOpNames= [ "<", "<=", ">", ">=", ":=", "+", "&", "-", "/"]
          , P.opLetter       = oneOf (concat (P.reservedOpNames tigerDef))
          , P.caseSensitive  = True   
          }

parens          = P.parens lexer    
braces          = P.braces lexer    
semiSep         = P.semiSep lexer  
semiSep1        = P.semiSep1 lexer    
commaSep        = P.commaSep lexer
commaSep1       = P.commaSep1 lexer
brackets        = P.brackets lexer
whiteSpace      = P.whiteSpace lexer    
symbol          = P.symbol lexer    
identifier      = P.identifier lexer    
reserved        = P.reserved lexer    
reservedOp      = P.reservedOp lexer
integer         = P.integer lexer    
charLiteral     = P.charLiteral lexer    
stringLiteral   = P.stringLiteral lexer    
