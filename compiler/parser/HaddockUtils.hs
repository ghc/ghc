
module HaddockUtils where

import HsSyn
import SrcLoc

import Control.Monad

-- -----------------------------------------------------------------------------
-- Adding documentation to record fields (used in parsing).

addFieldDoc :: ConDeclField a -> Maybe LHsDocString -> ConDeclField a
addFieldDoc fld doc = fld { cd_fld_doc = cd_fld_doc fld `mplus` doc }

addFieldDocs :: [ConDeclField a] -> Maybe LHsDocString -> [ConDeclField a]
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

addTyConDoc :: LTyConDecl a -> Maybe LHsDocString -> LTyConDecl a
addTyConDoc decl    Nothing = decl
addTyConDoc (L p c) doc     = L p ( c { tycon_doc = tycon_doc c `mplus` doc } )

addTyConDocs :: [LTyConDecl a] -> Maybe LHsDocString -> [LTyConDecl a]
addTyConDocs [] _ = []
addTyConDocs [x] doc = [addTyConDoc x doc]
addTyConDocs (x:xs) doc = x : addTyConDocs xs doc

addTyConDocFirst :: [LTyConDecl a] -> Maybe LHsDocString -> [LTyConDecl a]
addTyConDocFirst [] _ = []
addTyConDocFirst (x:xs) doc = addTyConDoc x doc : xs
