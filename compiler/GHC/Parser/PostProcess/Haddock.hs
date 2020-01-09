{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

{-# LANGUAGE TypeFamilies #-}

module GHC.Parser.PostProcess.Haddock where

import GHC.Prelude

import GHC.Hs
import GHC.Types.SrcLoc

import Control.Monad

-- -----------------------------------------------------------------------------
-- Adding documentation to record fields (used in parsing).

addFieldDoc :: XRec a (ConDeclField a) ~ Located (ConDeclField a)
            => LConDeclField a -> Maybe LHsDocString -> LConDeclField a
addFieldDoc (L l fld) doc
  = L l (fld { cd_fld_doc = cd_fld_doc fld `mplus` doc })

addFieldDocs :: XRec a (ConDeclField a) ~ Located (ConDeclField a)
             => [LConDeclField a] -> Maybe LHsDocString -> [LConDeclField a]
addFieldDocs [] _ = []
addFieldDocs (x:xs) doc = addFieldDoc x doc : xs


addConDoc :: XRec pass (ConDecl pass) ~ Located (ConDecl pass) =>
             LConDecl pass -> Maybe LHsDocString -> LConDecl pass
addConDoc decl    Nothing = decl
addConDoc (L p c) doc     = L p ( c { con_doc = con_doc c `mplus` doc } )

addConDocs :: XRec pass (ConDecl pass) ~ Located (ConDecl pass) =>
              [LConDecl pass] -> Maybe LHsDocString -> [LConDecl pass]
addConDocs [] _ = []
addConDocs [x] doc = [addConDoc x doc]
addConDocs (x:xs) doc = x : addConDocs xs doc

addConDocFirst :: XRec pass (ConDecl pass) ~ Located (ConDecl pass) =>
                  [LConDecl pass] -> Maybe LHsDocString -> [LConDecl pass]
addConDocFirst [] _ = []
addConDocFirst (x:xs) doc = addConDoc x doc : xs
