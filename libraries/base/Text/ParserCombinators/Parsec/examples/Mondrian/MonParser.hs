-----------------------------------------------------------
-- Daan Leijen (c) 1999-2000, daan@cs.uu.nl
-----------------------------------------------------------
module MonParser ( parseMondrian
                 , parseMondrianFromFile
                 , prettyFile
                 
                 , ParseError
                 ) where

import Char
import Monad
import Mondrian
import Utils        (groupLambdas)

-- Parsec
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language (mondrianDef)

--testing
import qualified SimpleMondrianPrinter as Pretty    




-----------------------------------------------------------
-- 
-----------------------------------------------------------
parseMondrianFromFile :: String -> IO (Either ParseError CompilationUnit)
parseMondrianFromFile fname =
    parseFromFile compilationUnit fname

parseMondrian sourceName source =
    parse compilationUnit sourceName source


       
-- testing
prettyFile fname
    = do{ result <- parseMondrianFromFile fname
        ; case result of
            Left err    -> putStr ("parse error at: " ++ show err)
            Right x     -> print (Pretty.compilationUnit x)
        }


-----------------------------------------------------------
-- GRAMMAR ELEMENTS
-----------------------------------------------------------    
compilationUnit :: Parser CompilationUnit    
compilationUnit =
    do{ whiteSpace
      ; reserved "package"
      ; name  <- option [""] packageName
      ; decls <- option []   declarations
      ; eof
      ; return $ Package name decls
      }

-----------------------------------------------------------
-- Declarations
-----------------------------------------------------------    
declarations =
    braces (semiSep1 declaration)
    
declaration =
        importDeclaration
    <|> classDeclaration
    <|> variableSignatureDeclaration         
    <?> "declaration"
    
variableSignatureDeclaration =
    do{ name <- variableName
      ; variableDeclaration name <|> signatureDeclaration name
      }    
    
variableDeclaration name =
    do{ symbol "=" 
      ; expr <- expression
      ; return $ VarDecl name expr
      }
    <?> "variable declaration"
 
importDeclaration =
    do{ reserved "import"
      ; name <- packageName
      ; star <- option [] (do{ symbol "."
                             ; symbol "*"
                             ; return ["*"] 
                             })
      ; return $ ImportDecl (name ++ star)
      }
      
classDeclaration =
    do{ reserved "class"
      ; name    <- className
      ; extends <- option [] (do{ reserved "extends"
                                ; n <- className
                                ; return [n]
                                })
      ; decls   <- option [] declarations
      ; return $ ClassDecl name extends decls
      }

signatureDeclaration name =
    do{ symbol "::"
      ; texpr  <- typeExpression
      ; return $ SigDecl name texpr
      }
    <?> "type declaration"  
    
      
-----------------------------------------------------------
-- Expressions
-----------------------------------------------------------    
expression :: Parser Expr      
expression =
        lambdaExpression 
    <|> letExpression 
    <|> newExpression 
    <|> infixExpression     
    <?> "expression"
    
lambdaExpression =
    do{ symbol "\\" 
      ; name <- variableName
      ; symbol "->"
      ; expr <- expression
      ; return $ groupLambdas (Lambda [name] expr)
      }

letExpression =
    do{ reserved "let"
      ; decls <- declarations
      ; reserved "in"
      ; expr <- expression
      ; return $ Let decls expr
      }

newExpression =
    do{ reserved "new"
      ; name  <- className
      ; decls <- option [] declarations
      ; return $ New name decls
      }


-----------------------------------------------------------
-- Infix expression
-----------------------------------------------------------
infixExpression = 
    buildExpressionParser operators applyExpression
    
operators =
    [ [ prefix "-", prefix "+" ]
    , [ op "^"  AssocRight ]
    , [ op "*"  AssocLeft, op "/"  AssocLeft ]
    , [ op "+"  AssocLeft, op "-"  AssocLeft ]
    , [ op "==" AssocNone, op "/=" AssocNone, op "<"  AssocNone
      , op "<=" AssocNone, op ">"  AssocNone, op ">=" AssocNone ]
    , [ op "&&" AssocNone ]
    , [ op "||" AssocNone ]
    ]
    where
      op name assoc   = Infix (do{ var <- try (symbol name)
                                  ; return (\x y -> App (App (Var [var]) x) y) 
                                  }) assoc
      prefix name     = Prefix  (do{ var <- try (symbol name)
                                  ; return (\x -> App (Var [var,"unary"]) x)
                                  })                                  
                                                
  

applyExpression =
    do{ exprs <- many1 simpleExpression
      ; return (foldl1 App exprs)
      }
      
{-
infixExpression =
    do{ (e,es) <- chain simpleExpression operator "infix expression"
      ; return $ if null es then e else (unChain (Chain e es))
      }
-}
    
simpleExpression :: Parser Expr    
simpleExpression =
        literal
    <|> parens expression
    <|> caseExpression
    <|> variable            
    <?> "simple expression"
  
          
-----------------------------------------------------------
-- Case expression
-----------------------------------------------------------
caseExpression =
    do{ reserved "case"
      ; expr <- variable
      ; reserved "of"
      ; alts <- alternatives
      ; return $ Case expr alts
      }

alternatives =
    braces (semiSep1 arm)

arm =
    do{ pat <- pattern
      ; symbol "->"      
      ; expr <- expression
      ; return (pat,expr)
      }
        
pattern =
        do{ reserved "default"
          ; return Default
          }    
    <|> do{ name  <- patternName                 
          ; decls <- option [] declarations
          ; return $ Pattern name decls
          }
    <?> "pattern"


-----------------------------------------------------------
-- Type expression
-----------------------------------------------------------

{-
typeExpression =
    do{ (e,es) <- chain simpleType typeOperator "type expression"
      ; return $ if null es then e else Chain e es
      }
    <?> "type expression"  
-}

typeExpression :: Parser Expr
typeExpression =
    do{ exprs <- sepBy1 simpleType (symbol "->")  
      ; return (foldl1 (\x y -> App (App (Var ["->"]) x) y) exprs)
      }
      
simpleType :: Parser Expr      
simpleType =
        parens typeExpression
    <|> variable
    <?> "simple type"



-----------------------------------------------------------
-- LEXICAL ELEMENTS
-----------------------------------------------------------


-----------------------------------------------------------
-- Identifiers & Reserved words
-----------------------------------------------------------
variable =
    do{ name <- variableName    
      ; return $ Var name
      }
                
patternName   = qualifiedName <?> "pattern variable"        
variableName  = qualifiedName <?> "identifier"
className     = qualifiedName <?> "class name"
packageName   = qualifiedName <?> "package name"         
        
qualifiedName =
    identifier `sepBy1` (symbol "." <?> "")


-----------------------------------------------------------
-- Literals
-----------------------------------------------------------
literal =
    do{ v <- intLiteral <|> chrLiteral <|> strLiteral
      ; return $ Lit v
      }
    <?> "literal"  

intLiteral  = do{ n <- natural; return (IntLit n) }
chrLiteral  = do{ c <- charLiteral; return (CharLit c) }
strLiteral  = do{ s <- stringLiteral; return (StringLit s) }



-----------------------------------------------------------
-- Tokens
-- Use qualified import to have token parsers on toplevel
-----------------------------------------------------------
mondrian        = P.makeTokenParser mondrianDef    
    
parens          = P.parens mondrian    
braces          = P.braces mondrian    
semiSep1        = P.semiSep1 mondrian    
whiteSpace      = P.whiteSpace mondrian    
symbol          = P.symbol mondrian    
identifier      = P.identifier mondrian    
reserved        = P.reserved mondrian    
natural         = P.natural mondrian    
charLiteral     = P.charLiteral mondrian    
stringLiteral   = P.stringLiteral mondrian    
