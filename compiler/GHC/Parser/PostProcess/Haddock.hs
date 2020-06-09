{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module GHC.Parser.PostProcess.Haddock where

import GHC.Prelude

import GHC.Hs

import Control.Monad

-- -----------------------------------------------------------------------------
-- Adding documentation to record fields (used in parsing).

addFieldDoc :: forall a. MapXRec a => LConDeclField a -> Maybe LHsDocString -> LConDeclField a
addFieldDoc lfld doc
  = mapXRec @a (\fld -> fld { cd_fld_doc = cd_fld_doc fld `mplus` doc }) lfld

addFieldDocs :: MapXRec a => [LConDeclField a] -> Maybe LHsDocString -> [LConDeclField a]
addFieldDocs [] _ = []
addFieldDocs (x:xs) doc = addFieldDoc x doc : xs


addConDoc :: forall pass. MapXRec pass => LConDecl pass -> Maybe LHsDocString -> LConDecl pass
addConDoc decl Nothing = decl
addConDoc lp   doc     = mapXRec @pass (\c -> c { con_doc = con_doc c `mplus` doc } ) lp

addConDocs :: MapXRec pass => [LConDecl pass] -> Maybe LHsDocString -> [LConDecl pass]
addConDocs [] _ = []
addConDocs [x] doc = [addConDoc x doc]
addConDocs (x:xs) doc = x : addConDocs xs doc

addConDocFirst :: MapXRec pass => [LConDecl pass] -> Maybe LHsDocString -> [LConDecl pass]
addConDocFirst [] _ = []
addConDocFirst (x:xs) doc = addConDoc x doc : xs
