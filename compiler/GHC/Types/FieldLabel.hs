{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-} -- Outputable FieldLabelString

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

    FieldLabel { flHasDuplicateRecordFields = NoDuplicateRecordFields
               , flHasFieldSelector         = FieldSelectors
               , flSelector                 = foo }.

If DuplicateRecordFields is enabled, however, the same declaration instead gives

    FieldLabel { flHasDuplicateRecordFields = DuplicateRecordFields
               , flHasFieldSelector         = FieldSelectors
               , flSelector                 = foo }.

We need to keep track of whether FieldSelectors or DuplicateRecordFields were
enabled when a record field was defined, as they affect name resolution and
shadowing of record fields, as explained in Note [NoFieldSelectors] in GHC.Types.Name.Reader
and Note [Reporting duplicate local declarations] in GHC.Rename.Names.
-}

module GHC.Types.FieldLabel
   ( FieldLabelEnv
   , FieldLabel(..), flLabel
   , DuplicateRecordFields(..)
   , FieldSelectors(..)
   , flIsOverloaded
   )
where

import GHC.Prelude

import {-# SOURCE #-} GHC.Types.Name

import GHC.Data.FastString.Env
import GHC.Types.Unique (Uniquable(..))
import GHC.Utils.Outputable
import GHC.Utils.Binary

import Language.Haskell.Syntax.Basic (FieldLabelString(..))

import Control.DeepSeq
import Data.Bool
import Data.Data

-- | A map from labels to all the auxiliary information
type FieldLabelEnv = DFastStringEnv FieldLabel

-- | Fields in an algebraic record type; see Note [FieldLabel].
data FieldLabel = FieldLabel {
      flHasDuplicateRecordFields :: DuplicateRecordFields,
      -- ^ Was @DuplicateRecordFields@ on in the defining module for this datatype?
      flHasFieldSelector :: FieldSelectors,
      -- ^ Was @FieldSelectors@ enabled in the defining module for this datatype?
      -- See Note [NoFieldSelectors] in GHC.Rename.Env
      flSelector :: Name
      -- ^ The 'Name' of the selector function, which uniquely identifies
      -- the field label.
    }
  deriving (Data, Eq)

-- | User-visible label of a field.
flLabel :: FieldLabel -> FieldLabelString
flLabel = FieldLabelString . occNameFS . nameOccName . flSelector

instance HasOccName FieldLabel where
  occName = nameOccName . flSelector

instance Outputable FieldLabel where
    ppr fl = ppr (flLabel fl) <> whenPprDebug (braces (ppr (flSelector fl))
                                                <> ppr (flHasDuplicateRecordFields fl)
                                                <> ppr (flHasFieldSelector fl))

instance Outputable FieldLabelString where
  ppr (FieldLabelString l) = ppr l

instance Uniquable FieldLabelString where
  getUnique (FieldLabelString fs) = getUnique fs

-- | Flag to indicate whether the DuplicateRecordFields extension is enabled.
data DuplicateRecordFields
    = DuplicateRecordFields   -- ^ Fields may be duplicated in a single module
    | NoDuplicateRecordFields -- ^ Fields must be unique within a module (the default)
  deriving (Show, Eq, Data)

instance Binary DuplicateRecordFields where
    put_ bh f = put_ bh (f == DuplicateRecordFields)
    get bh = bool NoDuplicateRecordFields DuplicateRecordFields <$> get bh

instance Outputable DuplicateRecordFields where
    ppr DuplicateRecordFields   = text "+dup"
    ppr NoDuplicateRecordFields = text "-dup"

instance NFData DuplicateRecordFields where
  rnf DuplicateRecordFields   = ()
  rnf NoDuplicateRecordFields = ()


-- | Flag to indicate whether the FieldSelectors extension is enabled.
data FieldSelectors
    = FieldSelectors   -- ^ Selector functions are available (the default)
    | NoFieldSelectors -- ^ Selector functions are not available
  deriving (Show, Eq, Data)

instance Binary FieldSelectors where
    put_ bh f = put_ bh (f == FieldSelectors)
    get bh = bool NoFieldSelectors FieldSelectors <$> get bh

instance Outputable FieldSelectors where
    ppr FieldSelectors   = text "+sel"
    ppr NoFieldSelectors = text "-sel"

instance NFData FieldSelectors where
  rnf FieldSelectors   = ()
  rnf NoFieldSelectors = ()

-- | We need the @Binary Name@ constraint here even though there is an instance
-- defined in "GHC.Types.Name", because the we have a SOURCE import, so the
-- instance is not in scope.  And the instance cannot be added to Name.hs-boot
-- because "GHC.Utils.Binary" itself depends on "GHC.Types.Name".
instance Binary Name => Binary FieldLabel where
    put_ bh (FieldLabel aa ab ac) = do
        put_ bh aa
        put_ bh ab
        put_ bh ac
    get bh = do
        aa <- get bh
        ab <- get bh
        ac <- get bh
        return (FieldLabel aa ab ac)

flIsOverloaded :: FieldLabel -> Bool
flIsOverloaded fl =
 flHasDuplicateRecordFields fl == DuplicateRecordFields
 || flHasFieldSelector fl == NoFieldSelectors
