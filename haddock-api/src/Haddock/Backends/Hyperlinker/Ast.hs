{-# LANGUAGE RankNTypes #-}

module Haddock.Backends.Hyperlinker.Ast
    ( enrich
    , RichToken(..)
    ) where

import Haddock.Backends.Hyperlinker.Parser

import qualified GHC

import Control.Applicative
import Data.Data
import Data.Maybe

data RichToken = RichToken
    { rtkToken :: Token
    , rtkName :: Maybe GHC.Name
    }

enrich :: GHC.RenamedSource -> [Token] -> [RichToken]
enrich src =
    map $ \token -> RichToken
        { rtkToken = token
        , rtkName = lookupBySpan (tkSpan token) nameMap
        }
  where
    nameMap = variables src

type NameMap = [(GHC.SrcSpan, GHC.Name)]

lookupBySpan :: Span -> NameMap -> Maybe GHC.Name
lookupBySpan tspan = listToMaybe . map snd . filter (matches tspan . fst)

everything :: (r -> r -> r) -> (forall a. Data a => a -> r)
           -> (forall a. Data a => a -> r)
everything k f x = foldl k (f x) (gmapQ (everything k f) x)

variables :: GHC.RenamedSource -> NameMap
variables =
    everything (<|>) var
  where
    var term = case cast term of
        (Just (GHC.L sspan (GHC.HsVar sid))) -> pure (sspan, sid)
        _ -> empty

matches :: Span -> GHC.SrcSpan -> Bool
matches tspan (GHC.RealSrcSpan aspan)
    | rs && cs && re && ce = True
  where
    rs = (posRow . spStart) tspan == GHC.srcSpanStartLine aspan
    cs = (posCol . spStart) tspan == GHC.srcSpanStartCol aspan
    re = (posRow . spEnd) tspan == GHC.srcSpanEndLine aspan
    ce = (posCol . spEnd) tspan == GHC.srcSpanEndCol aspan
matches _ _ = False
