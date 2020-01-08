{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module GHC.Parser.PostProcess.Haddock where

import GHC.Prelude

import GHC.Hs

import GHC.Types.SrcLoc

import Control.Monad

-- -----------------------------------------------------------------------------
-- Adding documentation to record fields (used in parsing).

addFieldDoc :: LConDeclField GhcPs -> Maybe LHsDocString -> LConDeclField GhcPs
addFieldDoc (L l fld) doc
  = L l (fld { cd_fld_doc = cd_fld_doc fld `mplus` doc })

addFieldDocs :: [LConDeclField GhcPs] -> Maybe LHsDocString -> [LConDeclField GhcPs]
addFieldDocs [] _ = []
addFieldDocs (x:xs) doc = addFieldDoc x doc : xs


addConDoc :: LConDecl GhcPs -> Maybe LHsDocString -> LConDecl GhcPs
addConDoc decl    Nothing = decl
addConDoc (L p c) doc     = L p $ case c of
  ConDeclH98  { con_doc = old_doc } -> c { con_doc = old_doc `mplus` doc }
  ConDeclGADT { con_doc = old_doc } -> c { con_doc = old_doc `mplus` doc }
  XConDecl x@(ConDeclGADTPrefixPs { con_gp_doc = old_doc }) ->
    XConDecl (x { con_gp_doc = old_doc `mplus` doc })

addConDocs :: [LConDecl GhcPs] -> Maybe LHsDocString -> [LConDecl GhcPs]
addConDocs [] _ = []
addConDocs [x] doc = [addConDoc x doc]
addConDocs (x:xs) doc = x : addConDocs xs doc

addConDocFirst :: [LConDecl GhcPs] -> Maybe LHsDocString -> [LConDecl GhcPs]
addConDocFirst [] _ = []
addConDocFirst (x:xs) doc = addConDoc x doc : xs
