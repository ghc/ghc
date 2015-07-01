{-
%
% (c) Adam Gundry 2013-2015
%

This module defines the representation of FieldLabels as stored in
TyCons.  As well as a selector name, these have some extra structure
to support the AllowDuplicateRecordFields extension.

In the normal case (with NoAllowDuplicateRecordFields), a datatype like

    data T = MkT { foo :: Int }

has

    FieldLabel { flLabel        = "foo"
               , flIsOverloaded = False
               , flSelector     = foo }.

In particular, the Name of the selector has the same string
representation as the label.  If AllowDuplicateRecordFields
is enabled, however, the same declaration instead gives

    FieldLabel { flLabel        = "foo"
               , flIsOverloaded = True
               , flSelector     = $sel:foo:T }.

Now the name of the selector ($sel:foo:T) does not match the label of
the field (foo).  We must be careful not to show the selector name to
the user!  The point of mangling the selector name is to allow a
module to define the same field label in different datatypes:

    data T = MkT { foo :: Int }
    data U = MkU { foo :: Bool }

Now there will be two FieldLabel values for 'foo', one in T and one in
U.  They share the same label (FieldLabelString), but the selector and
dfuns/axioms differ.  Each FieldLabel value is unique to its type
constructor.
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}

module FieldLabel ( FieldLabelString
                  , FieldLabelEnv
                  , FieldLbl(..)
                  , FieldLabel
                  , mkFieldLabelOccs
                  ) where

import OccName
import Name

import FastString
import Outputable

import Data.Data

#if __GLASGOW_HASKELL__ < 709
import Data.Foldable ( Foldable )
import Data.Traversable ( Traversable )
#endif

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
      flSelector     :: a                 -- ^ Record selector function
    }
  deriving (Functor, Foldable, Traversable)
deriving instance Data a => Data (FieldLbl a)

instance Outputable a => Outputable (FieldLbl a) where
    ppr fl = ppr (flLabel fl) <> braces (ppr (flSelector fl))


-- | Record selector OccNames are built from the underlying field name and
-- the name of the type constructor, to support overloaded record fields.
mkFieldLabelOccs :: FieldLabelString -> OccName -> Bool -> FieldLbl OccName
mkFieldLabelOccs lbl tc is_overloaded
  = FieldLabel lbl is_overloaded sel_occ
  where
    str     = ":" ++ unpackFS lbl ++ ":" ++ occNameString tc
    sel_occ | is_overloaded = mkRecFldSelOcc str
            | otherwise     = mkVarOccFS lbl
