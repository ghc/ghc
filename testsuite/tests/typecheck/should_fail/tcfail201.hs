{-# LANGUAGE RankNTypes #-}


-- Claus reported by email that 
-- GHCi, version 6.9.20080217 loops on this program
-- http://www.haskell.org/pipermail/cvs-ghc/2008-June/043173.html
-- So I'm adding it to the test suite so that we'll see it if it happens again

module Foo where

data HsDoc id
  = DocEmpty
  | DocParagraph (HsDoc id)

gfoldl' :: (forall a b . c (a -> b) -> a -> c b) -> (forall g . g -> c g) -> a -> c a
gfoldl' k z hsDoc = case hsDoc of
			  DocEmpty                  -> z DocEmpty
			  (DocParagraph hsDoc)      -> z DocParagraph `k` hsDoc





