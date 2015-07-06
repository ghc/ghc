module Haddock.Backends.Hyperlinker.Types where


import qualified GHC


data Token = Token
    { tkType :: TokenType
    , tkValue :: String
    , tkSpan :: Span
    }

data Position = Position
    { posRow :: !Int
    , posCol :: !Int
    }

data Span = Span
    { spStart :: Position
    , spEnd :: Position
    }

data TokenType
    = TkIdentifier
    | TkKeyword
    | TkString
    | TkChar
    | TkNumber
    | TkOperator
    | TkGlyph
    | TkSpecial
    | TkSpace
    | TkComment
    | TkCpp
    | TkPragma
    | TkUnknown
    deriving (Show, Eq)


data RichToken = RichToken
    { rtkToken :: Token
    , rtkDetails :: Maybe TokenDetails
    }

data TokenDetails
    = RtkVar GHC.Name
    | RtkType GHC.Name
    | RtkBind GHC.Name
    | RtkDecl GHC.Name
    | RtkModule GHC.ModuleName
    deriving (Eq)


rtkName :: TokenDetails -> Either GHC.Name GHC.ModuleName
rtkName (RtkVar name) = Left name
rtkName (RtkType name) = Left name
rtkName (RtkBind name) = Left name
rtkName (RtkDecl name) = Left name
rtkName (RtkModule name) = Right name
