{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Haddock.Backends.Hyperlinker.Ast
    ( enrich
    , RichToken(..), RichTokenType(..), TokenDetails(..)
    ) where

import Haddock.Backends.Hyperlinker.Parser

import qualified GHC

import Control.Applicative
import Data.Data
import Data.Maybe

data RichToken = RichToken
    { rtkToken :: Token
    , rtkDetails :: Maybe TokenDetails
    }

data TokenDetails = TokenDetails
    { rtkType :: RichTokenType
    , rtkName :: GHC.Name
    }

data RichTokenType
    = RtkVar
    | RtkType
    | RtkBind

enrich :: GHC.RenamedSource -> [Token] -> [RichToken]
enrich src =
    map $ \token -> RichToken
        { rtkToken = token
        , rtkDetails = lookupBySpan (tkSpan token) detailsMap
        }
  where
    detailsMap = concat
        [ variables src
        , types src
        , binds src
        , imports src
        ]

type DetailsMap = [(GHC.SrcSpan, TokenDetails)]

lookupBySpan :: Span -> DetailsMap -> Maybe TokenDetails
lookupBySpan tspan = listToMaybe . map snd . filter (matches tspan . fst)

everything :: (r -> r -> r) -> (forall a. Data a => a -> r)
           -> (forall a. Data a => a -> r)
everything k f x = foldl k (f x) (gmapQ (everything k f) x)

combine :: Alternative f => (forall a. Data a => a -> f r) -> (forall a. Data a => a -> f r) -> (forall a. Data a => a -> f r)
combine f g x = f x <|> g x

variables :: GHC.RenamedSource -> DetailsMap
variables =
    everything (<|>) var
  where
    var term = case cast term of
        (Just (GHC.L sspan (GHC.HsVar name))) ->
            pure (sspan, TokenDetails RtkVar name)
        _ -> empty

types :: GHC.RenamedSource -> DetailsMap
types =
    everything (<|>) ty
  where
    ty term = case cast term of
        (Just (GHC.L sspan (GHC.HsTyVar name))) ->
            pure (sspan, TokenDetails RtkType name)
        _ -> empty

binds :: GHC.RenamedSource -> DetailsMap
binds =
    everything (<|>) (fun `combine` pat)
  where
    fun term = case cast term of
        (Just (GHC.FunBind (GHC.L sspan name) _ _ _ _ _ :: GHC.HsBind GHC.Name)) ->
            pure (sspan, TokenDetails RtkBind name)
        _ -> empty
    pat term = case cast term of
        (Just (GHC.L sspan (GHC.VarPat name))) ->
            pure (sspan, TokenDetails RtkBind name)
        _ -> empty

imports :: GHC.RenamedSource -> DetailsMap
imports =
    everything (<|>) ie
  where
    ie term = case cast term of
        (Just (GHC.IEVar v)) -> pure $ var v
        (Just (GHC.IEThingAbs t)) -> pure $ typ t
        (Just (GHC.IEThingAll t)) -> pure $ typ t
        (Just (GHC.IEThingWith t vs)) -> [typ t] ++ map var vs
        _ -> empty
    typ (GHC.L sspan name) = (sspan, TokenDetails RtkType name)
    var (GHC.L sspan name) = (sspan, TokenDetails RtkVar name)

matches :: Span -> GHC.SrcSpan -> Bool
matches tspan (GHC.RealSrcSpan aspan)
    | rs && cs && re && ce = True
  where
    rs = (posRow . spStart) tspan == GHC.srcSpanStartLine aspan
    cs = (posCol . spStart) tspan == GHC.srcSpanStartCol aspan
    re = (posRow . spEnd) tspan == GHC.srcSpanEndLine aspan
    ce = (posCol . spEnd) tspan == GHC.srcSpanEndCol aspan
matches _ _ = False
