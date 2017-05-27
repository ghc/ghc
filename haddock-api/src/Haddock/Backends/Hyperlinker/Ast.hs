{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}

module Haddock.Backends.Hyperlinker.Ast (enrich) where


import qualified Haddock.Syb as Syb
import Haddock.Backends.Hyperlinker.Types

import qualified GHC

import Control.Applicative
import Data.Data
import Data.Maybe

everythingInRenamedSource :: (Alternative f, Data x)
  => (forall a. Data a => a -> f r) -> x -> f r
everythingInRenamedSource f = Syb.everythingButType @GHC.Name (<|>) f

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
    everythingInRenamedSource (var `Syb.combine` rec)
  where
    var term = case cast term of
        (Just (GHC.L sspan (GHC.HsVar name))) ->
            pure (sspan, RtkVar (GHC.unLoc name))
        (Just (GHC.L _ (GHC.RecordCon (GHC.L sspan name) _ _ _))) ->
            pure (sspan, RtkVar name)
        _ -> empty
    rec term = case cast term of
        Just (GHC.HsRecField (GHC.L sspan name) (_ :: GHC.LHsExpr GHC.Name) _) ->
            pure (sspan, RtkVar name)
        _ -> empty

-- | Obtain details map for types.
types :: GHC.RenamedSource -> DetailsMap
types = everythingInRenamedSource ty
  where
    ty term = case cast term of
        (Just (GHC.L sspan (GHC.HsTyVar _ name))) ->
            pure (sspan, RtkType (GHC.unLoc name))
        _ -> empty

-- | Obtain details map for identifier bindings.
--
-- That includes both identifiers bound by pattern matching or declared using
-- ordinary assignment (in top-level declarations, let-expressions and where
-- clauses).

binds :: GHC.RenamedSource -> DetailsMap
binds = everythingInRenamedSource
      (fun `Syb.combine` pat `Syb.combine` tvar)
  where
    fun term = case cast term of
        (Just (GHC.FunBind (GHC.L sspan name) _ _ _ _ :: GHC.HsBind GHC.Name)) ->
            pure (sspan, RtkBind name)
        _ -> empty
    pat term = case cast term of
        (Just (GHC.L sspan (GHC.VarPat name))) ->
            pure (sspan, RtkBind (GHC.unLoc name))
        (Just (GHC.L _ (GHC.ConPatIn (GHC.L sspan name) recs))) ->
            [(sspan, RtkVar name)] ++ everythingInRenamedSource rec recs
        (Just (GHC.L _ (GHC.AsPat (GHC.L sspan name) _))) ->
            pure (sspan, RtkBind name)
        _ -> empty
    rec term = case cast term of
        (Just (GHC.HsRecField (GHC.L sspan name) (_ :: GHC.LPat GHC.Name) _)) ->
            pure (sspan, RtkVar name)
        _ -> empty
    tvar term = case cast term of
        (Just (GHC.L sspan (GHC.UserTyVar name))) ->
            pure (sspan, RtkBind (GHC.unLoc name))
        (Just (GHC.L _ (GHC.KindedTyVar (GHC.L sspan name) _))) ->
            pure (sspan, RtkBind name)
        _ -> empty

-- | Obtain details map for top-level declarations.
decls :: GHC.RenamedSource -> DetailsMap
decls (group, _, _, _) = concatMap ($ group)
    [ concat . map typ . concat . map GHC.group_tyclds . GHC.hs_tyclds
    , everythingInRenamedSource fun . GHC.hs_valds
    , everythingInRenamedSource (con `Syb.combine` ins)
    ]
  where
    typ (GHC.L _ t) = case t of
        GHC.DataDecl { tcdLName = name } -> pure . decl $ name
        GHC.SynDecl name _ _ _ _ -> pure . decl $ name
        GHC.FamDecl fam -> pure . decl $ GHC.fdLName fam
        GHC.ClassDecl{..} -> [decl tcdLName] ++ concatMap sig tcdSigs
    fun term = case cast term of
        (Just (GHC.FunBind (GHC.L sspan name) _ _ _ _ :: GHC.HsBind GHC.Name))
            | GHC.isExternalName name -> pure (sspan, RtkDecl name)
        _ -> empty
    con term = case cast term of
        (Just cdcl) ->
            map decl (GHC.getConNames cdcl)
              ++ everythingInRenamedSource fld cdcl
        Nothing -> empty
    ins term = case cast term of
        (Just (GHC.DataFamInstD inst)) -> pure . tyref $ GHC.dfid_tycon inst
        (Just (GHC.TyFamInstD (GHC.TyFamInstDecl (GHC.L _ eqn) _))) ->
            pure . tyref $ GHC.tfe_tycon eqn
        _ -> empty
    fld term = case cast term of
        Just (field :: GHC.ConDeclField GHC.Name)
          -> map (decl . fmap GHC.selectorFieldOcc) $ GHC.cd_fld_names field
        Nothing -> empty
    sig (GHC.L _ (GHC.TypeSig names _)) = map decl names
    sig _ = []
    decl (GHC.L sspan name) = (sspan, RtkDecl name)
    tyref (GHC.L sspan name) = (sspan, RtkType name)

-- | Obtain details map for import declarations.
--
-- This map also includes type and variable details for items in export and
-- import lists.
imports :: GHC.RenamedSource -> DetailsMap
imports src@(_, imps, _, _) =
    everythingInRenamedSource ie src ++ mapMaybe (imp . GHC.unLoc) imps
  where
    ie term = case cast term of
        (Just (GHC.IEVar v)) -> pure $ var $ GHC.ieLWrappedName v
        (Just (GHC.IEThingAbs t)) -> pure $ typ $ GHC.ieLWrappedName t
        (Just (GHC.IEThingAll t)) -> pure $ typ $ GHC.ieLWrappedName t
        (Just (GHC.IEThingWith t _ vs _fls)) ->
          [typ $ GHC.ieLWrappedName t] ++ map (var . GHC.ieLWrappedName) vs
        _ -> empty
    typ (GHC.L sspan name) = (sspan, RtkType name)
    var (GHC.L sspan name) = (sspan, RtkVar name)
    imp idecl | not . GHC.ideclImplicit $ idecl =
        let (GHC.L sspan name) = GHC.ideclName idecl
        in Just (sspan, RtkModule name)
    imp _ = Nothing

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
