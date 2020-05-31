{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

{-# LANGUAGE TypeFamilies #-}

module GHC.Parser.PostProcess.Haddock where

import GHC.Prelude

import GHC.Hs
import GHC.Types.SrcLoc

import Control.Monad

-- -----------------------------------------------------------------------------
-- Adding documentation to record fields (used in parsing).

addFieldDoc :: Located (ConDeclField a) -> Maybe LHsDocString -> Located (ConDeclField a)
addFieldDoc (L l fld) doc
  = L l (fld { cd_fld_doc = cd_fld_doc fld `mplus` doc })

addFieldDocs :: [Located (ConDeclField a)] -> Maybe LHsDocString -> [Located (ConDeclField a)]
addFieldDocs [] _ = []
addFieldDocs (x:xs) doc = addFieldDoc x doc : xs


addConDoc :: Located (ConDecl pass) -> Maybe LHsDocString -> Located (ConDecl pass)
addConDoc decl    Nothing = decl
addConDoc (L p c) doc     = L p ( c { con_doc = con_doc c `mplus` doc } )

addConDocs :: [Located (ConDecl pass)] -> Maybe LHsDocString -> [Located (ConDecl pass)]
addConDocs [] _ = []
addConDocs [x] doc = [addConDoc x doc]
addConDocs (x:xs) doc = x : addConDocs xs doc

addConDocFirst :: [Located (ConDecl pass)] -> Maybe LHsDocString -> [Located (ConDecl pass)]
addConDocFirst [] _ = []
addConDocFirst (x:xs) doc = addConDoc x doc : xs
