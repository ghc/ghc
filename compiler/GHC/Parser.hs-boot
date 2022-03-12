module GHC.Parser where

import GHC.Types.Name.Reader (RdrName)
import GHC.Parser.Lexer (P)
import GHC.Parser.Annotation (LocatedN)

parseIdentifier :: P (LocatedN RdrName)
