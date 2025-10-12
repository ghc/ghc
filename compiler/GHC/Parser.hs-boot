module GHC.Parser where

import GHC.Types.Name.Reader (RdrName)
import GHC.Parser.Lexer (P)
import GHC.Parser.Annotation (LocatedN)
import GHC.Parser.PreProcess (PpState)

parseIdentifier :: P PpState (LocatedN RdrName)
