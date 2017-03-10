module Haddock.Backends.Hyperlinker.Types where


import qualified GHC

import Data.Map (Map)
import qualified Data.Map as Map


data Token = Token
    { tkType :: TokenType
    , tkValue :: String
    , tkSpan :: Span
    }
    deriving (Show)

data Position = Position
    { posRow :: !Int
    , posCol :: !Int
    }
    deriving (Show)

data Span = Span
    { spStart :: Position
    , spEnd :: Position
    }
    deriving (Show)

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


-- | Path for making cross-package hyperlinks in generated sources.
--
-- Used in 'SrcMap' to determine whether module originates in current package
-- or in an external package.
data SrcPath
    = SrcExternal FilePath
    | SrcLocal

-- | Mapping from modules to cross-package source paths.
--
-- This mapping is actually a pair of maps instead of just one map. The reason
-- for this is because when hyperlinking modules in import lists we have no
-- 'GHC.Module' available. On the other hand, we can't just use map with
-- 'GHC.ModuleName' as indices because certain modules may have common name
-- but originate in different packages. Hence, we use both /rich/ and /poor/
-- versions, where the /poor/ is just projection of /rich/ one cached in pair
-- for better performance.
type SrcMap = (Map GHC.Module SrcPath, Map GHC.ModuleName SrcPath)

mkSrcMap :: Map GHC.Module SrcPath -> SrcMap
mkSrcMap srcs = (srcs, Map.mapKeys GHC.moduleName srcs)
