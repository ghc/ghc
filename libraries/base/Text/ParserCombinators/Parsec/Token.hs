-----------------------------------------------------------------------------
-- |
-- Module      :  Text.ParserCombinators.Parsec.Token
-- Copyright   :  (c) Daan Leijen 1999-2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  daan@cs.uu.nl
-- Stability   :  provisional
-- Portability :  non-portable (uses existentially quantified data constructors)
--
-- A helper module to parse lexical elements (tokens).
-- 
-----------------------------------------------------------------------------

module Text.ParserCombinators.Parsec.Token
                  ( LanguageDef (..)
                  , TokenParser (..)
                  , makeTokenParser
                  ) where

import Data.Char (isAlpha,toLower,toUpper,isSpace,digitToInt)
import Data.List (nub,sort)
import Text.ParserCombinators.Parsec


-----------------------------------------------------------
-- Language Definition
-----------------------------------------------------------
data LanguageDef st  
    = LanguageDef 
    { commentStart   :: String
    , commentEnd     :: String
    , commentLine    :: String
    , nestedComments :: Bool                  
    , identStart     :: CharParser st Char
    , identLetter    :: CharParser st Char
    , opStart        :: CharParser st Char
    , opLetter       :: CharParser st Char
    , reservedNames  :: [String]
    , reservedOpNames:: [String]
    , caseSensitive  :: Bool
    }                           
           
-----------------------------------------------------------
-- A first class module: TokenParser
-----------------------------------------------------------
data TokenParser st
    = TokenParser{ identifier       :: CharParser st String
                 , reserved         :: String -> CharParser st ()
                 , operator         :: CharParser st String
                 , reservedOp       :: String -> CharParser st ()
                        
                 , charLiteral      :: CharParser st Char
                 , stringLiteral    :: CharParser st String
                 , natural          :: CharParser st Integer
                 , integer          :: CharParser st Integer
                 , float            :: CharParser st Double
                 , naturalOrFloat   :: CharParser st (Either Integer Double)
                 , decimal          :: CharParser st Integer
                 , hexadecimal      :: CharParser st Integer
                 , octal            :: CharParser st Integer
            
                 , symbol           :: String -> CharParser st String
                 , lexeme           :: forall a. CharParser st a -> CharParser st a
                 , whiteSpace       :: CharParser st ()     
             
                 , parens           :: forall a. CharParser st a -> CharParser st a 
                 , braces           :: forall a. CharParser st a -> CharParser st a
                 , angles           :: forall a. CharParser st a -> CharParser st a
                 , brackets         :: forall a. CharParser st a -> CharParser st a
                 -- "squares" is deprecated
                 , squares          :: forall a. CharParser st a -> CharParser st a 

                 , semi             :: CharParser st String
                 , comma            :: CharParser st String
                 , colon            :: CharParser st String
                 , dot              :: CharParser st String
                 , semiSep          :: forall a . CharParser st a -> CharParser st [a]
                 , semiSep1         :: forall a . CharParser st a -> CharParser st [a]
                 , commaSep         :: forall a . CharParser st a -> CharParser st [a]
                 , commaSep1        :: forall a . CharParser st a -> CharParser st [a]                
                 }

-----------------------------------------------------------
-- Given a LanguageDef, create a token parser.
-----------------------------------------------------------
makeTokenParser :: LanguageDef st -> TokenParser st
makeTokenParser languageDef
    = TokenParser{ identifier = identifier
                 , reserved = reserved
                 , operator = operator
                 , reservedOp = reservedOp
                        
                 , charLiteral = charLiteral
                 , stringLiteral = stringLiteral
                 , natural = natural
                 , integer = integer
                 , float = float
                 , naturalOrFloat = naturalOrFloat
                 , decimal = decimal
                 , hexadecimal = hexadecimal
                 , octal = octal
            
                 , symbol = symbol
                 , lexeme = lexeme
                 , whiteSpace = whiteSpace
             
                 , parens = parens
                 , braces = braces
                 , angles = angles
                 , brackets = brackets
                 , squares = brackets
                 , semi = semi
                 , comma = comma
                 , colon = colon
                 , dot = dot
                 , semiSep = semiSep
                 , semiSep1 = semiSep1
                 , commaSep = commaSep
                 , commaSep1 = commaSep1
                 }
    where
     
    -----------------------------------------------------------
    -- Bracketing
    -----------------------------------------------------------
    parens p        = between (symbol "(") (symbol ")") p
    braces p        = between (symbol "{") (symbol "}") p
    angles p        = between (symbol "<") (symbol ">") p
    brackets p      = between (symbol "[") (symbol "]") p

    semi            = symbol ";" 
    comma           = symbol ","
    dot             = symbol "."
    colon           = symbol ":"

    commaSep p      = sepBy p comma
    semiSep p       = sepBy p semi

    commaSep1 p     = sepBy1 p comma
    semiSep1 p      = sepBy1 p semi


    -----------------------------------------------------------
    -- Chars & Strings
    -----------------------------------------------------------
    -- charLiteral :: CharParser st Char
    charLiteral     = lexeme (between (char '\'') 
                                      (char '\'' <?> "end of character")
                                      characterChar )
                    <?> "character"

    characterChar   = charLetter <|> charEscape 
                    <?> "literal character"

    charEscape      = do{ char '\\'; escapeCode }
    charLetter      = satisfy (\c -> (c /= '\'') && (c /= '\\') && (c > '\026'))



    -- stringLiteral :: CharParser st String
    stringLiteral   = lexeme (
                      do{ str <- between (char '"')                   
                                         (char '"' <?> "end of string")
                                         (many stringChar) 
                        ; return (foldr (maybe id (:)) "" str)
                        }
                      <?> "literal string")

    -- stringChar :: CharParser st (Maybe Char)
    stringChar      =   do{ c <- stringLetter; return (Just c) }
                    <|> stringEscape 
                    <?> "string character"
                
    stringLetter    = satisfy (\c -> (c /= '"') && (c /= '\\') && (c > '\026'))

    stringEscape    = do{ char '\\'
                        ;     do{ escapeGap  ; return Nothing }
                          <|> do{ escapeEmpty; return Nothing }
                          <|> do{ esc <- escapeCode; return (Just esc) }
                        }
                        
    escapeEmpty     = char '&'
    escapeGap       = do{ many1 space
                        ; char '\\' <?> "end of string gap"
                        }
                        
                        
                        
    -- escape codes
    escapeCode      = charEsc <|> charNum <|> charAscii <|> charControl
                    <?> "escape code"

    -- charControl :: CharParser st Char
    charControl     = do{ char '^'
                        ; code <- upper
                        ; return (toEnum (fromEnum code - fromEnum 'A'))
                        }

    -- charNum :: CharParser st Char                    
    charNum         = do{ code <- decimal 
                                  <|> do{ char 'o'; number 8 octDigit }
                                  <|> do{ char 'x'; number 16 hexDigit }
                        ; return (toEnum (fromInteger code))
                        }

    charEsc         = choice (map parseEsc escMap)
                    where
                      parseEsc (c,code)     = do{ char c; return code }
                      
    charAscii       = choice (map parseAscii asciiMap)
                    where
                      parseAscii (asc,code) = try (do{ string asc; return code })


    -- escape code tables
    escMap          = zip ("abfnrtv\\\"\'") ("\a\b\f\n\r\t\v\\\"\'")
    asciiMap        = zip (ascii3codes ++ ascii2codes) (ascii3 ++ ascii2) 

    ascii2codes     = ["BS","HT","LF","VT","FF","CR","SO","SI","EM",
                       "FS","GS","RS","US","SP"]
    ascii3codes     = ["NUL","SOH","STX","ETX","EOT","ENQ","ACK","BEL",
                       "DLE","DC1","DC2","DC3","DC4","NAK","SYN","ETB",
                       "CAN","SUB","ESC","DEL"]

    ascii2          = ['\BS','\HT','\LF','\VT','\FF','\CR','\SO','\SI',
                       '\EM','\FS','\GS','\RS','\US','\SP']
    ascii3          = ['\NUL','\SOH','\STX','\ETX','\EOT','\ENQ','\ACK',
                       '\BEL','\DLE','\DC1','\DC2','\DC3','\DC4','\NAK',
                       '\SYN','\ETB','\CAN','\SUB','\ESC','\DEL']


    -----------------------------------------------------------
    -- Numbers
    -----------------------------------------------------------
    -- naturalOrFloat :: CharParser st (Either Integer Double)
    naturalOrFloat  = lexeme (natFloat) <?> "number"

    float           = lexeme floating   <?> "float"
    integer         = lexeme int        <?> "integer"
    natural         = lexeme nat        <?> "natural"


    -- floats
    floating        = do{ n <- decimal 
                        ; fractExponent n
                        }


    natFloat        = do{ char '0'
                        ; zeroNumFloat
                        }
                      <|> decimalFloat
                      
    zeroNumFloat    =  do{ n <- hexadecimal <|> octal
                         ; return (Left n)
                         }
                    <|> decimalFloat
                    <|> fractFloat 0
                    <|> return (Left 0)                  
                      
    decimalFloat    = do{ n <- decimal
                        ; option (Left n) 
                                 (fractFloat n)
                        }

    fractFloat n    = do{ f <- fractExponent n
                        ; return (Right f)
                        }
                        
    fractExponent n = do{ fract <- fraction
                        ; expo  <- option 1.0 exponent'
                        ; return ((fromInteger n + fract)*expo)
                        }
                    <|>
                      do{ expo <- exponent'
                        ; return ((fromInteger n)*expo)
                        }

    fraction        = do{ char '.'
                        ; digits <- many1 digit <?> "fraction"
                        ; return (foldr op 0.0 digits)
                        }
                      <?> "fraction"
                    where
                      op d f    = (f + fromIntegral (digitToInt d))/10.0
                        
    exponent'       = do{ oneOf "eE"
                        ; f <- sign
                        ; e <- decimal <?> "exponent"
                        ; return (power (f e))
                        }
                      <?> "exponent"
                    where
                       power e  | e < 0      = 1.0/power(-e)
                                | otherwise  = fromInteger (10^e)


    -- integers and naturals
    int             = do{ f <- lexeme sign
                        ; n <- nat
                        ; return (f n)
                        }
                        
    -- sign            :: CharParser st (Integer -> Integer)
    sign            =   (char '-' >> return negate) 
                    <|> (char '+' >> return id)     
                    <|> return id

    nat             = zeroNumber <|> decimal
        
    zeroNumber      = do{ char '0'
                        ; hexadecimal <|> octal <|> decimal <|> return 0
                        }
                      <?> ""       

    decimal         = number 10 digit        
    hexadecimal     = do{ oneOf "xX"; number 16 hexDigit }
    octal           = do{ oneOf "oO"; number 8 octDigit  }

    -- number :: Integer -> CharParser st Char -> CharParser st Integer
    number base baseDigit
        = do{ digits <- many1 baseDigit
            ; let n = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits
            ; seq n (return n)
            }          

    -----------------------------------------------------------
    -- Operators & reserved ops
    -----------------------------------------------------------
    reservedOp name =   
        lexeme $ try $
        do{ string name
          ; notFollowedBy (opLetter languageDef) <?> ("end of " ++ show name)
          }

    operator =
        lexeme $ try $
        do{ name <- oper
          ; if (isReservedOp name)
             then unexpected ("reserved operator " ++ show name)
             else return name
          }
          
    oper =
        do{ c <- (opStart languageDef)
          ; cs <- many (opLetter languageDef)
          ; return (c:cs)
          }
        <?> "operator"
        
    isReservedOp name =
        isReserved (sort (reservedOpNames languageDef)) name          
        
        
    -----------------------------------------------------------
    -- Identifiers & Reserved words
    -----------------------------------------------------------
    reserved name =
        lexeme $ try $
        do{ caseString name
          ; notFollowedBy (identLetter languageDef) <?> ("end of " ++ show name)
          }

    caseString name
        | caseSensitive languageDef  = string name
        | otherwise               = do{ walk name; return name }
        where
          walk []     = return ()
          walk (c:cs) = do{ caseChar c <?> msg; walk cs }
          
          caseChar c  | isAlpha c  = char (toLower c) <|> char (toUpper c)
                      | otherwise  = char c
          
          msg         = show name
          

    identifier =
        lexeme $ try $
        do{ name <- ident
          ; if (isReservedName name)
             then unexpected ("reserved word " ++ show name)
             else return name
          }
        
        
    ident           
        = do{ c <- identStart languageDef
            ; cs <- many (identLetter languageDef)
            ; return (c:cs)
            }
        <?> "identifier"

    isReservedName name
        = isReserved theReservedNames caseName
        where
          caseName      | caseSensitive languageDef  = name
                        | otherwise               = map toLower name

        
    isReserved names name    
        = scan names
        where
          scan []       = False
          scan (r:rs)   = case (compare r name) of
                            LT  -> scan rs
                            EQ  -> True
                            GT  -> False

    theReservedNames
        | caseSensitive languageDef  = sortedNames
        | otherwise               = map (map toLower) sortedNames
        where
          sortedNames   = sort (reservedNames languageDef)
                                 


    -----------------------------------------------------------
    -- White space & symbols
    -----------------------------------------------------------
    symbol name
        = lexeme (string name)

    lexeme p       
        = do{ x <- p; whiteSpace; return x  }
      
      
    --whiteSpace    
    whiteSpace 
        | noLine && noMulti  = skipMany (simpleSpace <?> "")
        | noLine             = skipMany (simpleSpace <|> multiLineComment <?> "")
        | noMulti            = skipMany (simpleSpace <|> oneLineComment <?> "")
        | otherwise          = skipMany (simpleSpace <|> oneLineComment <|> multiLineComment <?> "")
        where
          noLine  = null (commentLine languageDef)
          noMulti = null (commentStart languageDef)   
          
          
    simpleSpace =
        skipMany1 (satisfy isSpace)    
        
    oneLineComment =
        do{ try (string (commentLine languageDef))
          ; skipMany (satisfy (/= '\n'))
          ; return ()
          }

    multiLineComment =
        do { try (string (commentStart languageDef))
           ; inComment
           }

    inComment 
        | nestedComments languageDef  = inCommentMulti
        | otherwise                = inCommentSingle
        
    inCommentMulti 
        =   do{ try (string (commentEnd languageDef)) ; return () }
        <|> do{ multiLineComment                     ; inCommentMulti }
        <|> do{ skipMany1 (noneOf startEnd)          ; inCommentMulti }
        <|> do{ oneOf startEnd                       ; inCommentMulti }
        <?> "end of comment"  
        where
          startEnd   = nub (commentEnd languageDef ++ commentStart languageDef)

    inCommentSingle
        =   do{ try (string (commentEnd languageDef)); return () }
        <|> do{ skipMany1 (noneOf startEnd)         ; inCommentSingle }
        <|> do{ oneOf startEnd                      ; inCommentSingle }
        <?> "end of comment"
        where
          startEnd   = nub (commentEnd languageDef ++ commentStart languageDef)

