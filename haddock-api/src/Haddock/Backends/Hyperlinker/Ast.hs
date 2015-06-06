{-# LANGUAGE RankNTypes #-}

module Haddock.Backends.Hyperlinker.Ast where

import qualified GHC
import Data.Data
import Control.Applicative

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

everything :: (r -> r -> r) -> (forall a. Data a => a -> r)
           -> (forall a. Data a => a -> r)
everything k f x = foldl k (f x) (gmapQ (everything k f) x)

variables :: GHC.RenamedSource -> [(GHC.SrcSpan, GHC.Name)]
variables =
    everything (<|>) var
  where
    var term = case cast term of
        (Just (GHC.L sspan (GHC.HsVar sid))) -> pure (sspan, sid)
        _ -> empty
