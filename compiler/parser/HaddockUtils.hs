
module HaddockUtils where

import GhcPrelude

import HsSyn
import SrcLoc

import Control.Monad

-- -----------------------------------------------------------------------------
-- Adding documentation to record fields (used in parsing).

addFieldDoc :: LConDeclField a -> Maybe LHsDocString -> LConDeclField a
addFieldDoc (L l fld) doc
  = L l (fld { cd_fld_doc = cd_fld_doc fld `mplus` doc })

addFieldDocs :: [LConDeclField a] -> Maybe LHsDocString -> [LConDeclField a]
addFieldDocs [] _ = []
addFieldDocs (x:xs) doc = addFieldDoc x doc : xs


addConDoc :: LConDecl a -> Maybe LHsDocString -> LConDecl a
addConDoc decl    Nothing = decl
addConDoc (L p c) doc     = L p ( c { con_doc = con_doc c `mplus` doc } )

addConDocs :: [LConDecl a] -> Maybe LHsDocString -> [LConDecl a]
addConDocs [] _ = []
addConDocs [x] doc = [addConDoc x doc]
addConDocs (x:xs) doc = x : addConDocs xs doc

addConDocFirst :: [LConDecl a] -> Maybe LHsDocString -> [LConDecl a]
addConDocFirst [] _ = []
addConDocFirst (x:xs) doc = addConDoc x doc : xs
