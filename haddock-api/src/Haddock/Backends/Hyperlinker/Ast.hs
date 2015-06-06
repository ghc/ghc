module Haddock.Backends.Hyperlinker.Ast where

import qualified GHC

import Haddock.Backends.Hyperlinker.Parser

data RichToken = RichToken
    { rtkToken :: Token
    , rtkName :: Maybe GHC.Name
    }

enrich :: GHC.RenamedSource -> [Token] -> [RichToken]
enrich src =
    map $ \token -> RichToken
        { rtkToken = token
        , rtkName = lookupName src $ tkSpan token
        }

lookupName :: GHC.RenamedSource -> Span -> Maybe GHC.Name
lookupName = undefined
