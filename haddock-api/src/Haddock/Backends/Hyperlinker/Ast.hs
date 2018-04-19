{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}

module Haddock.Backends.Hyperlinker.Ast (enrich) where


import qualified Haddock.Syb as Syb
import Haddock.Backends.Hyperlinker.Types

import qualified GHC
import qualified Outputable as GHC

import Control.Applicative
import Control.Monad (guard)
import Data.Data
import qualified Data.Map.Strict as Map
import Data.Maybe

import Prelude hiding (span)

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
    detailsMap =
      mkDetailsMap (concatMap ($ src)
                     [ variables
                     , types
                     , decls
                     , binds
                     , imports
                     ])

type LTokenDetails = [(GHC.SrcSpan, TokenDetails)]

-- | A map containing association between source locations and "details" of
-- this location.
--
type DetailsMap = Map.Map Position (Span, TokenDetails)

mkDetailsMap :: [(GHC.SrcSpan, TokenDetails)] -> DetailsMap
mkDetailsMap xs =
  Map.fromListWith select_details [ (start, (token_span, token_details))
                                  | (ghc_span, token_details) <- xs
                                  , Just !token_span <- [ghcSrcSpanToSpan ghc_span]
                                  , let start = spStart token_span
                                  ]
  where
    -- favour token details which appear earlier in the list
    select_details _new old = old

lookupBySpan :: Span -> DetailsMap -> Maybe TokenDetails
lookupBySpan span details = do
  (_, (tok_span, tok_details)) <- Map.lookupLE (spStart span) details
  guard (tok_span `containsSpan` span )
  return tok_details

ghcSrcSpanToSpan :: GHC.SrcSpan -> Maybe Span
ghcSrcSpanToSpan (GHC.RealSrcSpan span) =
  Just (Span { spStart = Position (GHC.srcSpanStartLine span) (GHC.srcSpanStartCol span)
             , spEnd   = Position (GHC.srcSpanEndLine span) (GHC.srcSpanEndCol span)
             })
ghcSrcSpanToSpan _ = Nothing

enrichToken :: Token -> DetailsMap -> Maybe TokenDetails
enrichToken (Token typ _ spn) dm
    | typ `elem` [TkIdentifier, TkOperator] = lookupBySpan spn dm
enrichToken _ _ = Nothing

-- | Obtain details map for variables ("normally" used identifiers).
variables :: GHC.RenamedSource -> LTokenDetails
variables =
    everythingInRenamedSource (var `Syb.combine` rec)
  where
    var term = case cast term of
        (Just ((GHC.L sspan (GHC.HsVar _ name)) :: GHC.LHsExpr GHC.GhcRn)) ->
            pure (sspan, RtkVar (GHC.unLoc name))
        (Just (GHC.L _ (GHC.RecordCon _ (GHC.L sspan name) _))) ->
            pure (sspan, RtkVar name)
        _ -> empty
    rec term = case cast term of
        Just (GHC.HsRecField (GHC.L sspan name) (_ :: GHC.LHsExpr GHC.GhcRn) _) ->
            pure (sspan, RtkVar name)
        _ -> empty

-- | Obtain details map for types.
types :: GHC.RenamedSource -> LTokenDetails
types = everythingInRenamedSource ty
  where
    ty term = case cast term of
        (Just ((GHC.L sspan (GHC.HsTyVar _ _ name)) :: GHC.LHsType GHC.GhcRn)) ->
            pure (sspan, RtkType (GHC.unLoc name))
        _ -> empty

-- | Obtain details map for identifier bindings.
--
-- That includes both identifiers bound by pattern matching or declared using
-- ordinary assignment (in top-level declarations, let-expressions and where
-- clauses).

binds :: GHC.RenamedSource -> LTokenDetails
binds = everythingInRenamedSource
      (fun `Syb.combine` pat `Syb.combine` tvar)
  where
    fun term = case cast term of
        (Just (GHC.FunBind _ (GHC.L sspan name) _ _ _ :: GHC.HsBind GHC.GhcRn)) ->
            pure (sspan, RtkBind name)
        _ -> empty
    pat term = case cast term of
        (Just ((GHC.L sspan (GHC.VarPat _ name)) :: GHC.LPat GHC.GhcRn)) ->
            pure (sspan, RtkBind (GHC.unLoc name))
        (Just (GHC.L _ (GHC.ConPatIn (GHC.L sspan name) recs))) ->
            [(sspan, RtkVar name)] ++ everythingInRenamedSource rec recs
        (Just (GHC.L _ (GHC.AsPat _ (GHC.L sspan name) _))) ->
            pure (sspan, RtkBind name)
        _ -> empty
    rec term = case cast term of
        (Just (GHC.HsRecField (GHC.L sspan name) (_ :: GHC.LPat GHC.GhcRn) _)) ->
            pure (sspan, RtkVar name)
        _ -> empty
    tvar term = case cast term of
        (Just ((GHC.L sspan (GHC.UserTyVar _ name)) :: GHC.LHsTyVarBndr GHC.GhcRn)) ->
            pure (sspan, RtkBind (GHC.unLoc name))
        (Just (GHC.L _ (GHC.KindedTyVar _ (GHC.L sspan name) _))) ->
            pure (sspan, RtkBind name)
        _ -> empty

-- | Obtain details map for top-level declarations.
decls :: GHC.RenamedSource -> LTokenDetails
decls (group, _, _, _) = concatMap ($ group)
    [ concat . map typ . concat . map GHC.group_tyclds . GHC.hs_tyclds
    , everythingInRenamedSource fun . GHC.hs_valds
    , everythingInRenamedSource (con `Syb.combine` ins)
    ]
  where
    typ (GHC.L _ t) = case t of
        GHC.DataDecl { tcdLName = name } -> pure . decl $ name
        GHC.SynDecl _ name _ _ _ -> pure . decl $ name
        GHC.FamDecl _ fam -> pure . decl $ GHC.fdLName fam
        GHC.ClassDecl{..} -> [decl tcdLName] ++ concatMap sig tcdSigs
        GHC.XTyClDecl {} -> GHC.panic "haddock:decls"
    fun term = case cast term of
        (Just (GHC.FunBind _ (GHC.L sspan name) _ _ _ :: GHC.HsBind GHC.GhcRn))
            | GHC.isExternalName name -> pure (sspan, RtkDecl name)
        _ -> empty
    con term = case cast term of
        (Just (cdcl :: GHC.ConDecl GHC.GhcRn)) ->
            map decl (GHC.getConNames cdcl)
              ++ everythingInRenamedSource fld cdcl
        Nothing -> empty
    ins term = case cast term of
        (Just ((GHC.DataFamInstD _ (GHC.DataFamInstDecl eqn))
                :: GHC.InstDecl GHC.GhcRn))
          -> pure . tyref $ GHC.feqn_tycon $ GHC.hsib_body eqn
        (Just (GHC.TyFamInstD _ (GHC.TyFamInstDecl eqn))) ->
            pure . tyref $ GHC.feqn_tycon $ GHC.hsib_body eqn
        _ -> empty
    fld term = case cast term of
        Just (field :: GHC.ConDeclField GHC.GhcRn)
          -> map (decl . fmap GHC.extFieldOcc) $ GHC.cd_fld_names field
        Nothing -> empty
    sig (GHC.L _ (GHC.TypeSig _ names _)) = map decl names
    sig _ = []
    decl (GHC.L sspan name) = (sspan, RtkDecl name)
    tyref (GHC.L sspan name) = (sspan, RtkType name)

-- | Obtain details map for import declarations.
--
-- This map also includes type and variable details for items in export and
-- import lists.
imports :: GHC.RenamedSource -> LTokenDetails
imports src@(_, imps, _, _) =
    everythingInRenamedSource ie src ++ mapMaybe (imp . GHC.unLoc) imps
  where
    ie term = case cast term of
        (Just ((GHC.IEVar _ v) :: GHC.IE GHC.GhcRn)) -> pure $ var $ GHC.ieLWrappedName v
        (Just (GHC.IEThingAbs _ t)) -> pure $ typ $ GHC.ieLWrappedName t
        (Just (GHC.IEThingAll _ t)) -> pure $ typ $ GHC.ieLWrappedName t
        (Just (GHC.IEThingWith _ t _ vs _fls)) ->
          [typ $ GHC.ieLWrappedName t] ++ map (var . GHC.ieLWrappedName) vs
        _ -> empty
    typ (GHC.L sspan name) = (sspan, RtkType name)
    var (GHC.L sspan name) = (sspan, RtkVar name)
    imp idecl | not . GHC.ideclImplicit $ idecl =
        let (GHC.L sspan name) = GHC.ideclName idecl
        in Just (sspan, RtkModule name)
    imp _ = Nothing
