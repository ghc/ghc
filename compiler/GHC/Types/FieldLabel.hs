{-
%
% (c) Adam Gundry 2013-2015
%

This module defines the representation of FieldLabels as stored in
TyCons.  As well as a selector name, these have some extra structure
to support the DuplicateRecordFields extension.

In the normal case (with NoDuplicateRecordFields), a datatype like

    data T = MkT { foo :: Int }

has

    FieldLabel { flLabel        = "foo"
               , flIsOverloaded = False
               , flSelector     = foo }.

In particular, the Name of the selector has the same string
representation as the label.  If DuplicateRecordFields
is enabled, however, the same declaration instead gives

    FieldLabel { flLabel        = "foo"
               , flIsOverloaded = True
               , flSelector     = $sel:foo:MkT }.

Now the name of the selector ($sel:foo:MkT) does not match the label of
the field (foo).  We must be careful not to show the selector name to
the user!  The point of mangling the selector name is to allow a
module to define the same field label in different datatypes:

    data T = MkT { foo :: Int }
    data U = MkU { foo :: Bool }

Now there will be two FieldLabel values for 'foo', one in T and one in
U.  They share the same label (FieldLabelString), but the selector
functions differ.

See also Note [Representing fields in AvailInfo] in GHC.Types.Avail.

Note [Why selector names include data constructors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

As explained above, a selector name includes the name of the first
data constructor in the type, so that the same label can appear
multiple times in the same module.  (This is irrespective of whether
the first constructor has that field, for simplicity.)

We use a data constructor name, rather than the type constructor name,
because data family instances do not have a representation type
constructor name generated until relatively late in the typechecking
process.

Of course, datatypes with no constructors cannot have any fields.

-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}

module GHC.Types.FieldLabel
   ( FieldLabelString
   , FieldLabelEnv
   , FieldLbl(..)
   , FieldLabel
   , mkFieldLabelOccs
   )
where

import GHC.Prelude

import GHC.Types.Name.Occurrence
import GHC.Types.Name

import GHC.Data.FastString
import GHC.Data.FastString.Env
import GHC.Utils.Outputable
import GHC.Utils.Binary

import Data.Data

-- | Field labels are just represented as strings;
-- they are not necessarily unique (even within a module)
type FieldLabelString = FastString

-- | A map from labels to all the auxiliary information
type FieldLabelEnv = DFastStringEnv FieldLabel


type FieldLabel = FieldLbl Name

-- | Fields in an algebraic record type
data FieldLbl a = FieldLabel {
      flLabel        :: FieldLabelString, -- ^ User-visible label of the field
      flIsOverloaded :: Bool,             -- ^ Was DuplicateRecordFields on
                                          --   in the defining module for this datatype?
      flSelector     :: a                 -- ^ Record selector function
    }
  deriving (Eq, Functor, Foldable, Traversable)
deriving instance Data a => Data (FieldLbl a)

instance Outputable a => Outputable (FieldLbl a) where
    ppr fl = ppr (flLabel fl) <> braces (ppr (flSelector fl))

instance Binary a => Binary (FieldLbl a) where
    put_ bh (FieldLabel aa ab ac) = do
        put_ bh aa
        put_ bh ab
        put_ bh ac
    get bh = do
        ab <- get bh
        ac <- get bh
        ad <- get bh
        return (FieldLabel ab ac ad)


-- | Record selector OccNames are built from the underlying field name
-- and the name of the first data constructor of the type, to support
-- duplicate record field names.
-- See Note [Why selector names include data constructors].
mkFieldLabelOccs :: FieldLabelString -> OccName -> Bool -> FieldLbl OccName
mkFieldLabelOccs lbl dc is_overloaded
  = FieldLabel { flLabel = lbl, flIsOverloaded = is_overloaded
               , flSelector = sel_occ }
  where
    str     = ":" ++ unpackFS lbl ++ ":" ++ occNameString dc
    sel_occ | is_overloaded = mkRecFldSelOcc str
            | otherwise     = mkVarOccFS lbl
