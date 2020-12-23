{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE UndecidableInstances #-}

{-
%
% (c) Adam Gundry 2013-2015
%

Note [FieldLabel]
~~~~~~~~~~~~~~~~~

This module defines the representation of FieldLabels as stored in
TyCons.  As well as a selector name, these have some extra structure
to support the DuplicateRecordFields and NoFieldSelectors extensions.

In the normal case (with NoDuplicateRecordFields and FieldSelectors),
a datatype like

    data T = MkT { foo :: Int }

has

    FieldLabel { flLabel                    = "foo"
               , flHasDuplicateRecordFields = NoDuplicateRecordFields
               , flHasFieldSelector         = FieldSelectors
               , flSelector                 = foo }.

In particular, the Name of the selector has the same string
representation as the label.  If DuplicateRecordFields
is enabled, however, the same declaration instead gives

    FieldLabel { flLabel                    = "foo"
               , flHasDuplicateRecordFields = DuplicateRecordFields
               , flHasFieldSelector         = FieldSelectors
               , flSelector                 = $sel:foo:MkT }.

Similarly, the selector name will be mangled if NoFieldSelectors is used
(whether or not DuplicateRecordFields is enabled).  See Note [NoFieldSelectors]
in GHC.Rename.Env.

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

module GHC.Types.FieldLabel
   ( FieldLabelString
   , FieldLabelEnv
   , FieldLabel(..)
   , fieldSelectorOccName
   , fieldLabelPrintableName
   , DuplicateRecordFields(..)
   , FieldSelectors(..)
   , flIsOverloaded
   )
where

import GHC.Prelude

import {-# SOURCE #-} GHC.Types.Name.Occurrence
import {-# SOURCE #-} GHC.Types.Name

import GHC.Data.FastString
import GHC.Data.FastString.Env
import GHC.Utils.Outputable
import GHC.Utils.Binary

import Data.Bool
import Data.Data

-- | Field labels are just represented as strings;
-- they are not necessarily unique (even within a module)
type FieldLabelString = FastString

-- | A map from labels to all the auxiliary information
type FieldLabelEnv = DFastStringEnv FieldLabel

-- | Fields in an algebraic record type; see Note [FieldLabel].
data FieldLabel = FieldLabel {
      flLabel :: FieldLabelString,
      -- ^ User-visible label of the field
      flHasDuplicateRecordFields :: DuplicateRecordFields,
      -- ^ Was @DuplicateRecordFields@ on in the defining module for this datatype?
      flHasFieldSelector :: FieldSelectors,
      -- ^ Was @FieldSelectors@ enabled in the defining module for this datatype?
      -- See Note [NoFieldSelectors] in GHC.Rename.Env
      flSelector :: Name
      -- ^ Record selector function
    }
  deriving (Data, Eq)

instance HasOccName FieldLabel where
  occName = mkVarOccFS . flLabel

instance Outputable FieldLabel where
    ppr fl = ppr (flLabel fl) <> whenPprDebug (braces (ppr (flSelector fl))
                                                <> ppr (flHasDuplicateRecordFields fl)
                                                <> ppr (flHasFieldSelector fl))

-- | Flag to indicate whether the DuplicateRecordFields extension is enabled.
data DuplicateRecordFields
    = DuplicateRecordFields   -- ^ Fields may be duplicated in a single module
    | NoDuplicateRecordFields -- ^ Fields must be unique within a module (the default)
  deriving (Show, Eq, Typeable, Data)

instance Binary DuplicateRecordFields where
    put_ bh f = put_ bh (f == DuplicateRecordFields)
    get bh = bool NoDuplicateRecordFields DuplicateRecordFields <$> get bh

instance Outputable DuplicateRecordFields where
    ppr DuplicateRecordFields   = text "+dup"
    ppr NoDuplicateRecordFields = text "-dup"


-- | Flag to indicate whether the FieldSelectors extension is enabled.
data FieldSelectors
    = FieldSelectors   -- ^ Selector functions are available (the default)
    | NoFieldSelectors -- ^ Selector functions are not available
  deriving (Show, Eq, Typeable, Data)

instance Binary FieldSelectors where
    put_ bh f = put_ bh (f == FieldSelectors)
    get bh = bool NoFieldSelectors FieldSelectors <$> get bh

instance Outputable FieldSelectors where
    ppr FieldSelectors   = text "+sel"
    ppr NoFieldSelectors = text "-sel"


-- | We need the @Binary Name@ constraint here even though there is an instance
-- defined in "GHC.Types.Name", because the we have a SOURCE import, so the
-- instance is not in scope.  And the instance cannot be added to Name.hs-boot
-- because "GHC.Utils.Binary" itself depends on "GHC.Types.Name".
instance Binary Name => Binary FieldLabel where
    put_ bh (FieldLabel aa ab ac ad) = do
        put_ bh aa
        put_ bh ab
        put_ bh ac
        put_ bh ad
    get bh = do
        aa <- get bh
        ab <- get bh
        ac <- get bh
        ad <- get bh
        return (FieldLabel aa ab ac ad)


-- | Record selector OccNames are built from the underlying field name
-- and the name of the first data constructor of the type, to support
-- duplicate record field names.
-- See Note [Why selector names include data constructors].
fieldSelectorOccName :: FieldLabelString -> OccName -> DuplicateRecordFields -> FieldSelectors -> OccName
fieldSelectorOccName lbl dc dup_fields_ok has_sel
  | shouldMangleSelectorNames dup_fields_ok has_sel = mkRecFldSelOcc str
  | otherwise     = mkVarOccFS lbl
  where
    str     = ":" ++ unpackFS lbl ++ ":" ++ occNameString dc

-- | Undo the name mangling described in Note [FieldLabel] to produce a Name
-- that has the user-visible OccName (but the selector's unique).  This should
-- be used only when generating output, when we want to show the label, but may
-- need to qualify it with a module prefix.
fieldLabelPrintableName :: FieldLabel -> Name
fieldLabelPrintableName fl
  | flIsOverloaded fl = tidyNameOcc (flSelector fl) (mkVarOccFS (flLabel fl))
  | otherwise         = flSelector fl

-- | Selector name mangling should be used if either DuplicateRecordFields or
-- NoFieldSelectors is enabled, so that the OccName of the field can be used for
-- something else.  See Note [FieldLabel], and Note [NoFieldSelectors] in
-- GHC.Rename.Env.
shouldMangleSelectorNames :: DuplicateRecordFields -> FieldSelectors -> Bool
shouldMangleSelectorNames dup_fields_ok has_sel
    = dup_fields_ok == DuplicateRecordFields || has_sel == NoFieldSelectors

flIsOverloaded :: FieldLabel -> Bool
flIsOverloaded fl =
    shouldMangleSelectorNames (flHasDuplicateRecordFields fl) (flHasFieldSelector fl)
