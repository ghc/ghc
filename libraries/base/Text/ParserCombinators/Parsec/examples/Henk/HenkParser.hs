----------------------------------------------------------------
-- the Henk Parser
-- Copyright 2000, Jan-Willem Roorda and Daan Leijen
----------------------------------------------------------------
module HenkParser where

import Text.ParserCombinators.Parsec.
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language

import HenkAS

----------------------------------------------------------------
-- the Henk Parser
--
-- anonymous variables are any identifiers starting with "_"
--
-- unknown types (those that need to be inferred) can explicitly
-- be given using "?"
--
-- instead of grammar: "var : aexpr" as in the henk paper,
-- we use "var : expr" instead. This means that variable
-- sequences as in \, |~|, \/ and /\ expressions need to
-- be comma seperated. Pattern variables are also comma
-- seperated. The case arrow (->) now needs to be (=>) in
-- order to distinguish the end of the pattern from function
-- arrows.
----------------------------------------------------------------
program
    = do{ whiteSpace
        ; ts <- semiSep tdecl
        ; vs <- semiSep vdecl
        ; eof
        ; return $ Program ts vs
        }

----------------------------------------------------------------
-- Type declarations
----------------------------------------------------------------
tdecl
    = do{ reserved "data"
        ; t  <- bindVar
        ; symbol "="
        ; ts <- braces (semiSep1 tvar)
        ; return $ Data t ts
        }

----------------------------------------------------------------
-- Value declarations
----------------------------------------------------------------
vdecl :: Parser ValueDecl
vdecl
    = do{ reserved "let"
        ; b <- bind 
        ; return $ Let b
        }
    <|>
      do{ reserved "letrec"
        ; bs <- braces (semiSep1 bind)
        ; return $ LetRec bs
        }


bind
    = do{ t <- tvar
        ; symbol "="
        ; e <- expr
        ; return $ Bind t e
        }

----------------------------------------------------------------
-- Expressions
----------------------------------------------------------------
expr :: Parser Expr
expr 
    = choice 
      [ letExpr 
      , forallExpr      -- forall before lambda! \/ vs. \
      , lambdaExpr
      , piExpr
      , caseExpr 

      , functionExpr
      , bigLamdaExpr
      ]
    <?> "expression"

letExpr
    = do{ vd <- vdecl
        ; reserved "in"
        ; e  <- expr
        ; return (In vd e)
        }

lambdaExpr
    = do{ symbol "\\"
        ; ts <- commaSep1 bindVar
        ; symbol "."
        ; e  <- expr
        ; return $ (foldr Lam e ts)
        }

piExpr
    = do{ symbol "|~|"
        ; ts <- commaSep1 bindVar
        ; symbol "."
        ; e  <- expr
        ; return (foldr Pi e ts)
        }

----------------------------------------------------------------
-- Case expressions
----------------------------------------------------------------
caseExpr
    = do{ reserved "case"
        ; e <- expr
        ; reserved "of"
        ; as <- braces (semiSep1 alt)
        ; es <- option [] (do{ reserved "at"
                             ; braces (semiSep expr)
                             })
        ; return (Case e as es)
        }
        
alt
    = do{ pat <- pattern
        ; symbol "=>"
        ; e   <- expr
        ; return (pat e)
        }
        
pattern
    =   do{ p <- atomPattern
          ; vs <- commaSep boundVar
          ; return (\e -> Alt p (foldr Lam e vs))
          }
          
atomPattern
    =   do{ v <- boundVar
          ; return (PatVar v)
          }
    <|> do{ l <- literal
          ; return (PatLit l)
          }
    <?> "pattern"    


----------------------------------------------------------------
-- Syntactic sugar: ->, \/, /\
----------------------------------------------------------------
functionExpr
    = chainr1 appExpr arrow
    where
      arrow  = do{ symbol "->"
                 ; return ((\x y -> 
                             Pi (TVar anonymous x) y))
                 }
             <?> ""

bigLamdaExpr
    = do{ symbol "/\\"
        ; ts <- commaSep1 bindVar
        ; symbol "."
        ; e  <- expr
        ; return (foldr Lam e ts)
        }

forallExpr
    = do{ try (symbol "\\/")          -- use "try" to try "\" (lambda) too.
        ; ts <- commaSep1 bindVar
        ; symbol "."
        ; e  <- expr
        ; return (foldr Pi e ts)
        }

----------------------------------------------------------------
-- Simple expressions
----------------------------------------------------------------
appExpr 
    = do{ es <- many1 atomExpr
        ; return (foldl1 App es)
        }

atomExpr
    =   parens expr
    <|> do{ v <- boundVar; return (Var v)    }
    <|> do{ l <- literal; return (Lit l)}
    <|> do{ symbol "*"; return Star     }
    <|> do{ symbol "[]"; return Box     }
    <|> do{ symbol "?"; return Unknown  }
    <?> "simple expression"


----------------------------------------------------------------
-- Variables & Literals
----------------------------------------------------------------
variable    
    = identifier

anonymousVar
    = lexeme $
      do{ c <- char '_'
        ; cs <- many (identLetter henkDef)
        ; return (c:cs)
        }
        
bindVar    
    = do{ i <- variable <|> anonymousVar
        ; do{ e <- varType
            ; return (TVar i e)
            }
          <|> return (TVar i Star)
        }
    <?> "variable"

boundVar    
    = do{ i <- variable
        ; do{ e <- varType
            ; return (TVar i e)
            }
          <|> return (TVar i Unknown)
        }
    <?> "variable"


tvar
    = do{ v <- variable
        ; t <- varType 
        ; return (TVar v t)
        }
    <?> "typed variable"
    
varType
    = do{ symbol ":"
        ; expr
        }
    <?> "variable type"

literal
    = do{ i <- natural
        ; return (LitInt i) 
        }
    <?> "literal"


----------------------------------------------------------------
-- Tokens
----------------------------------------------------------------
henk            = P.makeTokenParser henkDef

lexeme          = P.lexeme henk
parens          = P.parens henk    
braces          = P.braces henk    
semiSep         = P.semiSep henk    
semiSep1        = P.semiSep1 henk    
commaSep        = P.commaSep henk    
commaSep1       = P.commaSep1 henk    
whiteSpace      = P.whiteSpace henk    
symbol          = P.symbol henk    
identifier      = P.identifier henk    
reserved        = P.reserved henk    
natural         = P.natural henk    


henkDef
    = haskellStyle
    { identStart        = letter
    , identLetter       = alphaNum <|> oneOf "_'"
    , opStart           = opLetter henkDef
    , opLetter          = oneOf ":=\\->/|~.*[]"
    , reservedOpNames   = ["::","=","\\","->","=>","/\\","\\/"
                          ,"|~|",".",":","*","[]"]  
    , reservedNames     = [ "case", "data", "letrec", "type"
                          , "import", "in", "let", "of", "at"
                          ] 
    }
