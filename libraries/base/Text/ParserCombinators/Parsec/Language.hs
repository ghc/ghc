-----------------------------------------------------------------------------
-- |
-- Module      :  Text.ParserCombinators.Parsec.Language
-- Copyright   :  (c) Daan Leijen 1999-2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  daan@cs.uu.nl
-- Stability   :  provisional
-- Portability :  non-portable (uses non-portable module Text.ParserCombinators.Parsec.Token)
--
-- A helper module that defines some language definitions that can be used
-- to instantiate a token parser (see "Text.ParserCombinators.Parsec.Token").
-- 
-----------------------------------------------------------------------------

module Text.ParserCombinators.Parsec.Language
                     ( haskellDef, haskell
                     , mondrianDef, mondrian
                   
                     , emptyDef
                     , haskellStyle
                     , javaStyle   
                     , LanguageDef (..)                
                     ) where
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token 

           
-----------------------------------------------------------
-- Styles: haskellStyle, javaStyle
-----------------------------------------------------------               
haskellStyle= emptyDef                      
                { commentStart   = "{-"
                , commentEnd     = "-}"
                , commentLine    = "--"
                , nestedComments = True
                , identStart     = letter
                , identLetter	 = alphaNum <|> oneOf "_'"
                , opStart	 = opLetter haskellStyle
                , opLetter	 = oneOf ":!#$%&*+./<=>?@\\^|-~"              
                , reservedOpNames= []
                , reservedNames  = []
                , caseSensitive  = True                                   
                }         
                           
javaStyle   = emptyDef
		{ commentStart	 = "/*"
		, commentEnd	 = "*/"
		, commentLine	 = "//"
		, nestedComments = True
		, identStart	 = letter
		, identLetter	 = alphaNum <|> oneOf "_'"		
		, reservedNames  = []
		, reservedOpNames= []	
                , caseSensitive  = False				  
		}

-----------------------------------------------------------
-- minimal language definition
-----------------------------------------------------------                
emptyDef    = LanguageDef 
               { commentStart   = ""
               , commentEnd     = ""
               , commentLine    = ""
               , nestedComments = True
               , identStart     = letter <|> char '_'
               , identLetter    = alphaNum <|> oneOf "_'"
               , opStart        = opLetter emptyDef
               , opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"
               , reservedOpNames= []
               , reservedNames  = []
               , caseSensitive  = True
               }
                


-----------------------------------------------------------
-- Haskell
-----------------------------------------------------------               
haskell :: TokenParser st
haskell      = makeTokenParser haskellDef

haskellDef   = haskell98Def
	        { identLetter	 = identLetter haskell98Def <|> char '#'
	        , reservedNames	 = reservedNames haskell98Def ++ 
    				   ["foreign","import","export","primitive"
    				   ,"_ccall_","_casm_"
    				   ,"forall"
    				   ]
                }
			    
haskell98Def = haskellStyle
                { reservedOpNames= ["::","..","=","\\","|","<-","->","@","~","=>"]
                , reservedNames  = ["let","in","case","of","if","then","else",
                                    "data","type",
                                    "class","default","deriving","do","import",
                                    "infix","infixl","infixr","instance","module",
                                    "newtype","where",
                                    "primitive"
                                    -- "as","qualified","hiding"
                                   ]
                }         
                
                
-----------------------------------------------------------
-- Mondrian
-----------------------------------------------------------               
mondrian :: TokenParser st
mondrian    = makeTokenParser mondrianDef

mondrianDef = javaStyle
		{ reservedNames = [ "case", "class", "default", "extends"
				  , "import", "in", "let", "new", "of", "package"
				  ]	
                , caseSensitive  = True				  
		}

				
