%
% (c) Adam Gundry 2013
%

This module defines the representation of FieldLabels as stored in
TyCons.  As well as a selector name, these have some extra structure
to support the OverloadedRecordFields extension.  For every field
label, regardless of whether the extension is enabled in the defining
module, we generate instances of the Has and Upd classes and FldTy and
UpdTy type families (all defined in base:GHC.Records).

In the normal case (with NoOverloadedRecordFields), a datatype like

    data T = MkT { foo :: Int }

has FieldLabel { flLabel = "foo"
               , flIsOverloaded = False
               , flSelector = foo
               , flHasDFun = $fHas:foo:T
               , flUpdDFun = $fUpd:foo:T
               , flFldTyAxiom = TFCo:FldTy:foo:T
               , flUpdTyAxiom = TFCo:UpdTy:foo:T }.

In particular, the Name of the selector has the same string
representation as the label.  If the OverloadedRecordFields extension
is enabled, however, the same declaration instead gives

               { flIsOverloaded = True
               , flSelector = $sel:foo:T }.

Now the name of the selector ($sel:foo:T) does not match the label of
the field (foo).  We must be careful not to show the selector name to
the user!  The point of mangling the selector name is to allow a
module to define the same field label in different datatypes:

    data T = MkT { foo :: Int }
    data U = MkU { foo :: Bool }

\begin{code}

{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module FieldLabel ( FieldLabelString
                  , FieldLabelEnv
                  , FieldLbl(..)
                  , FieldLabel
                  , mkFieldLabelOccs
                  ) where

import OccName
import Name

import Binary
import FastString
import FastStringEnv
import Outputable

import Data.Foldable
import Data.Traversable

-- | Field labels are just represented as strings;
-- they are not necessarily unique (even within a module)
type FieldLabelString = FastString

-- | A map from labels to all the auxiliary information
type FieldLabelEnv = FastStringEnv FieldLabel


type FieldLabel = FieldLbl Name

-- | Fields in an algebraic record type
data FieldLbl a = FieldLabel {
      flLabel        :: FieldLabelString, -- ^ Label of the field
      flIsOverloaded :: Bool,             -- ^ Is this field overloaded?
      flSelector     :: a,                -- ^ Record selector function
      flHasDFun      :: a,                -- ^ DFun for Has class instance
      flUpdDFun      :: a,                -- ^ DFun for Upd class instance
      flFldTyAxiom   :: a,                -- ^ Axiom for FldTy family instance
      flUpdTyAxiom   :: a                 -- ^ Axiom for UpdTy family instance
    }
  deriving (Functor, Foldable, Traversable)

instance Outputable a => Outputable (FieldLbl a) where
    ppr fl = ppr (flLabel fl) <> braces (ppr (flSelector fl))

instance Binary a => Binary (FieldLbl a) where
    put_ bh (FieldLabel aa ab ac ad ae af ag) = do
        put_ bh aa
        put_ bh ab
        put_ bh ac
        put_ bh ad
        put_ bh ae
        put_ bh af
        put_ bh ag

    get bh = do
        aa <- get bh
        ab <- get bh
        ac <- get bh
        ad <- get bh
        ae <- get bh
        af <- get bh
        ag <- get bh
        return (FieldLabel aa ab ac ad ae af ag)
\end{code}


Record selector OccNames are built from the underlying field name and
the name of the type constructor, to support overloaded record fields.

\begin{code}
mkFieldLabelOccs :: FieldLabelString -> OccName -> Bool -> FieldLbl OccName
mkFieldLabelOccs lbl tc is_overloaded
  = FieldLabel lbl is_overloaded sel_occ has_occ upd_occ get_occ set_occ
  where
    str     = ":" ++ unpackFS lbl ++ ":" ++ occNameString tc
    has_str = "Has"
    upd_str = "Upd"
    get_str = "FldTy"
    set_str = "UpdTy"

    sel_occ | is_overloaded = mkRecFldSelOcc str
            | otherwise     = mkVarOccFS lbl
    has_occ = mkRecFldDFunOcc (has_str ++ str)
    upd_occ = mkRecFldDFunOcc (upd_str ++ str)
    get_occ = mkRecFldAxiomOcc (get_str ++ str)
    set_occ = mkRecFldAxiomOcc (set_str ++ str)
\end{code}
