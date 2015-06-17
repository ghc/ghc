{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Haddock.Backends.Hyperlinker.Ast
    ( enrich
    , RichToken(..), TokenDetails(..), rtkName
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

data TokenDetails
    = RtkVar GHC.Name
    | RtkType GHC.Name
    | RtkBind GHC.Name
    | RtkDecl GHC.Name

rtkName :: TokenDetails -> GHC.Name
rtkName (RtkVar name) = name
rtkName (RtkType name) = name
rtkName (RtkBind name) = name
rtkName (RtkDecl name) = name

enrich :: GHC.RenamedSource -> [Token] -> [RichToken]
enrich src =
    map $ \token -> RichToken
        { rtkToken = token
        , rtkDetails = enrichToken token detailsMap
        }
  where
    detailsMap = concatMap ($ src)
        [ variables
        , types
        , decls
        , binds
        , imports
        ]

type DetailsMap = [(GHC.SrcSpan, TokenDetails)]

enrichToken :: Token -> DetailsMap -> Maybe TokenDetails
enrichToken (Token typ _ spn) dm
    | typ `elem` [TkIdentifier, TkOperator] = lookupBySpan spn dm
enrichToken _ _ = Nothing

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
            pure (sspan, RtkVar name)
        _ -> empty

types :: GHC.RenamedSource -> DetailsMap
types =
    everything (<|>) ty
  where
    ty term = case cast term of
        (Just (GHC.L sspan (GHC.HsTyVar name))) ->
            pure (sspan, RtkType name)
        _ -> empty

binds :: GHC.RenamedSource -> DetailsMap
binds =
    everything (<|>) (fun `combine` pat)
  where
    fun term = case cast term of
        (Just (GHC.FunBind (GHC.L sspan name) _ _ _ _ _ :: GHC.HsBind GHC.Name)) ->
            pure (sspan, RtkBind name)
        _ -> empty
    pat term = case cast term of
        (Just (GHC.L sspan (GHC.VarPat name))) ->
            pure (sspan, RtkBind name)
        _ -> empty

decls :: GHC.RenamedSource -> DetailsMap
decls (group, _, _, _) = concatMap ($ group)
    [ map typ . concat . map GHC.group_tyclds . GHC.hs_tyclds
    , everything (<|>) fun
    ]
  where
    typ (GHC.L _ t) =
        let (GHC.L sspan name) = GHC.tcdLName t
        in (sspan, RtkDecl name)
    fun term = case cast term of
        (Just (GHC.FunBind (GHC.L sspan name) _ _ _ _ _ :: GHC.HsBind GHC.Name))
            | GHC.isExternalName name -> pure (sspan, RtkDecl name)
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
    typ (GHC.L sspan name) = (sspan, RtkType name)
    var (GHC.L sspan name) = (sspan, RtkVar name)

matches :: Span -> GHC.SrcSpan -> Bool
matches tspan (GHC.RealSrcSpan aspan)
    | saspan <= stspan && etspan <= easpan = True
  where
    stspan = (posRow . spStart $ tspan, posCol . spStart $ tspan)
    etspan = (posRow . spEnd $ tspan, posCol . spEnd $ tspan)
    saspan = (GHC.srcSpanStartLine aspan, GHC.srcSpanStartCol aspan)
    easpan = (GHC.srcSpanEndLine aspan, GHC.srcSpanEndCol aspan)
matches _ _ = False
