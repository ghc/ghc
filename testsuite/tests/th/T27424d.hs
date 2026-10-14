{-# LANGUAGE TemplateHaskell, GADTs #-}

module T27424d where

import Language.Haskell.TH

-- Some TH generators might emit nested ForallC, as in the splice below.
-- Record GADT constructors accept at most one context in source syntax, so
-- GHC.ThToHs merges the contexts to keep the -ddump-splices output valid:
--
--   MkR :: (Eq a, Show a) => {fld :: a} -> R a
--
-- See Note [Contexts of nested ForallC] in GHC.ThToHs.
$(do let r = mkName "R"
         a = mkName "a"
         bang = Bang NoSourceUnpackedness NoSourceStrictness
     return
       [DataD [] r [PlainTV a BndrReq] Nothing
          [ForallC [] [AppT (ConT ''Eq) (VarT a)] $
             ForallC [] [AppT (ConT ''Show) (VarT a)] $
               RecGadtC [mkName "MkR"] [(mkName "fld", bang, VarT a)] (AppT (ConT r) (VarT a))]
          []])

r :: R Int
r = MkR { fld = 42 }
