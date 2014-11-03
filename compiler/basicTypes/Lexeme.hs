-- (c) The GHC Team
--
-- Functions to evaluate whether or not a string is a valid identifier.
-- There is considerable overlap between the logic here and the logic
-- in Lexer.x, but sadly there seems to be way to merge them.

module Lexeme (
          -- * Lexical characteristics of Haskell names
  
          -- | Use these functions to figure what kind of name a 'FastString'
          -- represents; these functions do /not/ check that the identifier
          -- is valid.
  
        isLexCon, isLexVar, isLexId, isLexSym,
        isLexConId, isLexConSym, isLexVarId, isLexVarSym,
        startsVarSym, startsVarId, startsConSym, startsConId,

          -- * Validating identifiers

          -- | These functions (working over plain old 'String's) check
          -- to make sure that the identifier is valid.
        okVarOcc, okConOcc, okTcOcc,
        okVarIdOcc, okVarSymOcc, okConIdOcc, okConSymOcc

        -- Some of the exports above are not used within GHC, but may
        -- be of value to GHC API users.

  ) where

import FastString

import Data.Char
import qualified Data.Set as Set

{-

************************************************************************
*                                                                      *
    Lexical categories
*                                                                      *
************************************************************************

These functions test strings to see if they fit the lexical categories
defined in the Haskell report.

Note [Classification of generated names]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Some names generated for internal use can show up in debugging output,
e.g.  when using -ddump-simpl. These generated names start with a $
but should still be pretty-printed using prefix notation. We make sure
this is the case in isLexVarSym by only classifying a name as a symbol
if all its characters are symbols, not just its first one.
-}

isLexCon,   isLexVar,    isLexId,    isLexSym    :: FastString -> Bool
isLexConId, isLexConSym, isLexVarId, isLexVarSym :: FastString -> Bool

isLexCon cs = isLexConId  cs || isLexConSym cs
isLexVar cs = isLexVarId  cs || isLexVarSym cs

isLexId  cs = isLexConId  cs || isLexVarId  cs
isLexSym cs = isLexConSym cs || isLexVarSym cs

-------------
isLexConId cs                           -- Prefix type or data constructors
  | nullFS cs          = False          --      e.g. "Foo", "[]", "(,)"
  | cs == (fsLit "[]") = True
  | otherwise          = startsConId (headFS cs)

isLexVarId cs                           -- Ordinary prefix identifiers
  | nullFS cs         = False           --      e.g. "x", "_x"
  | otherwise         = startsVarId (headFS cs)

isLexConSym cs                          -- Infix type or data constructors
  | nullFS cs          = False          --      e.g. ":-:", ":", "->"
  | cs == (fsLit "->") = True
  | otherwise          = startsConSym (headFS cs)

isLexVarSym fs                          -- Infix identifiers e.g. "+"
  | fs == (fsLit "~R#") = True
  | otherwise
  = case (if nullFS fs then [] else unpackFS fs) of
      [] -> False
      (c:cs) -> startsVarSym c && all isVarSymChar cs
        -- See Note [Classification of generated names]

-------------
startsVarSym, startsVarId, startsConSym, startsConId :: Char -> Bool
startsVarSym c = startsVarSymASCII c || (ord c > 0x7f && isSymbol c)  -- Infix Ids
startsConSym c = c == ':'               -- Infix data constructors
startsVarId c  = c == '_' || case generalCategory c of  -- Ordinary Ids
  LowercaseLetter -> True
  OtherLetter     -> True   -- See #1103
  _               -> False
startsConId c  = isUpper c || c == '('  -- Ordinary type constructors and data constructors

startsVarSymASCII :: Char -> Bool
startsVarSymASCII c = c `elem` "!#$%&*+./<=>?@\\^|~-"

isVarSymChar :: Char -> Bool
isVarSymChar c = c == ':' || startsVarSym c

{-

************************************************************************
*                                                                      *
    Detecting valid names for Template Haskell
*                                                                      *
************************************************************************

-}

----------------------
-- External interface 
----------------------

-- | Is this an acceptable variable name?
okVarOcc :: String -> Bool
okVarOcc str@(c:_)
  | startsVarId c
  = okVarIdOcc str
  | startsVarSym c
  = okVarSymOcc str
okVarOcc _ = False

-- | Is this an acceptable constructor name?
okConOcc :: String -> Bool
okConOcc str@(c:_)
  | startsConId c
  = okConIdOcc str
  | startsConSym c
  = okConSymOcc str
  | str == "[]"
  = True
okConOcc _ = False

-- | Is this an acceptable type name?
okTcOcc :: String -> Bool
okTcOcc "[]" = True
okTcOcc "->" = True
okTcOcc "~"  = True
okTcOcc str@(c:_)
  | startsConId c
  = okConIdOcc str
  | startsConSym c
  = okConSymOcc str
  | startsVarSym c
  = okVarSymOcc str
okTcOcc _ = False

-- | Is this an acceptable alphanumeric variable name, assuming it starts
-- with an acceptable letter?
okVarIdOcc :: String -> Bool
okVarIdOcc str = okIdOcc str &&
                 not (str `Set.member` reservedIds)

-- | Is this an acceptable symbolic variable name, assuming it starts
-- with an acceptable character?
okVarSymOcc :: String -> Bool
okVarSymOcc str = all okSymChar str &&
                  not (str `Set.member` reservedOps) &&
                  not (isDashes str)

-- | Is this an acceptable alphanumeric constructor name, assuming it
-- starts with an acceptable letter?
okConIdOcc :: String -> Bool
okConIdOcc str = okIdOcc str ||
                 is_tuple_name1 str
  where
    -- check for tuple name, starting at the beginning
    is_tuple_name1 ('(' : rest) = is_tuple_name2 rest
    is_tuple_name1 _            = False

    -- check for tuple tail
    is_tuple_name2 ")"          = True
    is_tuple_name2 (',' : rest) = is_tuple_name2 rest
    is_tuple_name2 (ws  : rest)
      | isSpace ws              = is_tuple_name2 rest
    is_tuple_name2 _            = False

-- | Is this an acceptable symbolic constructor name, assuming it
-- starts with an acceptable character?
okConSymOcc :: String -> Bool
okConSymOcc ":" = True
okConSymOcc str = all okSymChar str &&
                  not (str `Set.member` reservedOps)

----------------------
-- Internal functions
----------------------

-- | Is this string an acceptable id, possibly with a suffix of hashes,
-- but not worrying about case or clashing with reserved words?
okIdOcc :: String -> Bool
okIdOcc str
  = let hashes = dropWhile okIdChar str in
    all (== '#') hashes   -- -XMagicHash allows a suffix of hashes
                          -- of course, `all` says "True" to an empty list

-- | Is this character acceptable in an identifier (after the first letter)?
-- See alexGetByte in Lexer.x
okIdChar :: Char -> Bool
okIdChar c = case generalCategory c of
  UppercaseLetter -> True
  LowercaseLetter -> True
  OtherLetter     -> True
  TitlecaseLetter -> True
  DecimalNumber   -> True
  OtherNumber     -> True
  _               -> c == '\'' || c == '_'

-- | Is this character acceptable in a symbol (after the first char)?
-- See alexGetByte in Lexer.x
okSymChar :: Char -> Bool
okSymChar c
  | c `elem` specialSymbols
  = False
  | c `elem` "_\"'"
  = False
  | otherwise
  = case generalCategory c of
      ConnectorPunctuation -> True
      DashPunctuation      -> True
      OtherPunctuation     -> True
      MathSymbol           -> True
      CurrencySymbol       -> True
      ModifierSymbol       -> True
      OtherSymbol          -> True
      _                    -> False
    
-- | All reserved identifiers. Taken from section 2.4 of the 2010 Report.
reservedIds :: Set.Set String
reservedIds = Set.fromList [ "case", "class", "data", "default", "deriving"
                           , "do", "else", "foreign", "if", "import", "in"
                           , "infix", "infixl", "infixr", "instance", "let"
                           , "module", "newtype", "of", "then", "type", "where"
                           , "_" ]

-- | All punctuation that cannot appear in symbols. See $special in Lexer.x.
specialSymbols :: [Char]
specialSymbols = "(),;[]`{}"

-- | All reserved operators. Taken from section 2.4 of the 2010 Report.
reservedOps :: Set.Set String
reservedOps = Set.fromList [ "..", ":", "::", "=", "\\", "|", "<-", "->"
                           , "@", "~", "=>" ]

-- | Does this string contain only dashes and has at least 2 of them?
isDashes :: String -> Bool
isDashes ('-' : '-' : rest) = all (== '-') rest
isDashes _                  = False
