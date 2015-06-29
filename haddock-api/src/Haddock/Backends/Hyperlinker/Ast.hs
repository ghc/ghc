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
    | RtkModule GHC.ModuleName

rtkName :: TokenDetails -> Either GHC.Name GHC.ModuleName
rtkName (RtkVar name) = Left name
rtkName (RtkType name) = Left name
rtkName (RtkBind name) = Left name
rtkName (RtkDecl name) = Left name
rtkName (RtkModule name) = Right name

-- | Add more detailed information to token stream using GHC API.
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

-- | A map containing association between source locations and "details" of
-- this location.
--
-- For the time being, it is just a list of pairs. However, looking up things
-- in such structure has linear complexity. We cannot use any hashmap-like
-- stuff because source locations are not ordered. In the future, this should
-- be replaced with interval tree data structure.
type DetailsMap = [(GHC.SrcSpan, TokenDetails)]

lookupBySpan :: Span -> DetailsMap -> Maybe TokenDetails
lookupBySpan tspan = listToMaybe . map snd . filter (matches tspan . fst)

enrichToken :: Token -> DetailsMap -> Maybe TokenDetails
enrichToken (Token typ _ spn) dm
    | typ `elem` [TkIdentifier, TkOperator] = lookupBySpan spn dm
enrichToken _ _ = Nothing

-- | Obtain details map for variables ("normally" used identifiers).
variables :: GHC.RenamedSource -> DetailsMap
variables =
    everything (<|>) var
  where
    var term = case cast term of
        (Just (GHC.L sspan (GHC.HsVar name))) ->
            pure (sspan, RtkVar name)
        _ -> empty

-- | Obtain details map for types.
types :: GHC.RenamedSource -> DetailsMap
types =
    everything (<|>) ty
  where
    ty term = case cast term of
        (Just (GHC.L sspan (GHC.HsTyVar name))) ->
            pure (sspan, RtkType name)
        _ -> empty

-- | Obtain details map for identifier bindings.
--
-- That includes both identifiers bound by pattern matching or declared using
-- ordinary assignment (in top-level declarations, let-expressions and where
-- clauses).
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

-- | Obtain details map for top-level declarations.
decls :: GHC.RenamedSource -> DetailsMap
decls (group, _, _, _) = concatMap ($ group)
    [ concat . map typ . concat . map GHC.group_tyclds . GHC.hs_tyclds
    , everything (<|>) fun
    ]
  where
    typ (GHC.L _ t) = case t of
        GHC.DataDecl (GHC.L sspan name) _ defn _ ->
            [(sspan, RtkDecl name)] ++ concatMap con (GHC.dd_cons defn)
        _ ->
            let (GHC.L sspan name) = GHC.tcdLName t
            in pure (sspan, RtkDecl name)
    fun term = case cast term of
        (Just (GHC.FunBind (GHC.L sspan name) _ _ _ _ _ :: GHC.HsBind GHC.Name))
            | GHC.isExternalName name -> pure (sspan, RtkDecl name)
        _ -> empty
    con (GHC.L _ t) = flip map (GHC.con_names t) $
        \(GHC.L sspan name) -> (sspan, RtkDecl name)

-- | Obtain details map for import declarations.
--
-- This map also includes type and variable details for items in export and
-- import lists.
imports :: GHC.RenamedSource -> DetailsMap
imports src@(_, imps, _, _) =
    everything (<|>) ie src ++ map (imp . GHC.unLoc) imps
  where
    ie term = case cast term of
        (Just (GHC.IEVar v)) -> pure $ var v
        (Just (GHC.IEThingAbs t)) -> pure $ typ t
        (Just (GHC.IEThingAll t)) -> pure $ typ t
        (Just (GHC.IEThingWith t vs)) -> [typ t] ++ map var vs
        _ -> empty
    typ (GHC.L sspan name) = (sspan, RtkType name)
    var (GHC.L sspan name) = (sspan, RtkVar name)
    imp idecl =
        let (GHC.L sspan name) = GHC.ideclName idecl
        in (sspan, RtkModule name)

-- | Check whether token stream span matches GHC source span.
--
-- Currently, it is implemented as checking whether "our" span is contained
-- in GHC span. The reason for that is because GHC span are generally wider
-- and may spread across couple tokens. For example, @(>>=)@ consists of three
-- tokens: @(@, @>>=@, @)@, but GHC source span associated with @>>=@ variable
-- contains @(@ and @)@. Similarly, qualified identifiers like @Foo.Bar.quux@
-- are tokenized as @Foo@, @.@, @Bar@, @.@, @quux@ but GHC source span
-- associated with @quux@ contains all five elements.
matches :: Span -> GHC.SrcSpan -> Bool
matches tspan (GHC.RealSrcSpan aspan)
    | saspan <= stspan && etspan <= easpan = True
  where
    stspan = (posRow . spStart $ tspan, posCol . spStart $ tspan)
    etspan = (posRow . spEnd $ tspan, posCol . spEnd $ tspan)
    saspan = (GHC.srcSpanStartLine aspan, GHC.srcSpanStartCol aspan)
    easpan = (GHC.srcSpanEndLine aspan, GHC.srcSpanEndCol aspan)
matches _ _ = False

-- | Perform a query on each level of a tree.
--
-- This is stolen directly from SYB package and copied here to not introduce
-- additional dependencies.
everything :: (r -> r -> r) -> (forall a. Data a => a -> r)
           -> (forall a. Data a => a -> r)
everything k f x = foldl k (f x) (gmapQ (everything k f) x)

-- | Combine two queries into one using alternative combinator.
combine :: Alternative f => (forall a. Data a => a -> f r)
                         -> (forall a. Data a => a -> f r)
                         -> (forall a. Data a => a -> f r)
combine f g x = f x <|> g x
