--
-- (c) The University of Glasgow
--

{-# LANGUAGE DeriveDataTypeable #-}

module Avail (
    Avails, AvailFlds, AvailFld, AvailFields, AvailField,
    AvailInfo(..),
    availsToNameSet,
    availsToNameSetWithSelectors,
    availsToNameEnv,
    availName, availNames, availNonFldNames,
    availNamesWithSelectors,
    availFlds, availOverloadedFlds,
    stableAvailCmp, stableAvailFieldsCmp,
    availFieldsLabels,
    availFieldsNames, availFieldsNamesWithSelectors,
    fieldLabelsToAvailFields,
    pprAvailField
  ) where

import Name
import NameEnv
import NameSet

import FieldLabel
import Binary
import Outputable
import Util

import Data.Function

-- -----------------------------------------------------------------------------
-- The AvailInfo type

-- | Records what things are "available", i.e. in scope
data AvailInfo = Avail Name      -- ^ An ordinary identifier in scope
               | AvailTC Name
                         [Name]
                         AvailFields
                                 -- ^ A type or class in scope. Parameters:
                                 --
                                 --  1) The name of the type or class
                                 --  2) The available pieces of type or class.
                                 --  3) The record fields of the type.
                                 --
                                 -- The AvailTC Invariant:
                                 --   * If the type or class is itself
                                 --     to be in scope, it must be
                                 --     *first* in this list.  Thus,
                                 --     typically: @AvailTC Eq [Eq, ==, \/=]@
                deriving( Eq )
                        -- Equality used when deciding if the
                        -- interface has changed

-- | A collection of 'AvailInfo' - several things that are \"available\"
type Avails = [AvailInfo]

-- | Record fields in an 'AvailInfo'
-- See Note [Representing fields in AvailInfo]
type AvailFlds name = [AvailFld name]
type AvailFld name  = (name, Maybe FieldLabelString)
type AvailFields    = AvailFlds Name
type AvailField     = AvailFld Name

{-
Note [Representing fields in AvailInfo]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When -XOverloadedRecordFields is disabled (the normal case), a
datatype like

  data T = MkT { foo :: Int }

gives rise to the AvailInfo

  AvailTC T [T, MkT] [(foo, Nothing)],

whereas if -XOverloadedRecordFields is enabled it gives

  AvailTC T [T, MkT] [($sel:foo:T, Just "foo")]

since the label does not match the selector name.

The labels in an Overloaded field list are not necessarily unique:
data families allow the same parent (the family tycon) to have
multiple distinct fields with the same label. For example,

  data family F a
  data instance F Int  = MkFInt { foo :: Int }
  data instance F Bool = MkFBool { foo :: Bool}

gives rise to

  AvailTC F [F, MkFInt, MkFBool]
    [($sel:foo:R:FInt, Just "foo"), ($sel:foo:R:FBool, Just "foo")].
-}

-- | Compare lexicographically
stableAvailCmp :: AvailInfo -> AvailInfo -> Ordering
stableAvailCmp (Avail n1)         (Avail n2)     = n1 `stableNameCmp` n2
stableAvailCmp (Avail {})         (AvailTC {})   = LT
stableAvailCmp (AvailTC n ns nfs) (AvailTC m ms mfs) =
    (n `stableNameCmp` m) `thenCmp`
    (cmpList stableNameCmp ns ms) `thenCmp`
    (stableAvailFieldsCmp nfs mfs)
stableAvailCmp (AvailTC {})       (Avail {})     = GT

stableAvailFieldsCmp :: AvailFields -> AvailFields -> Ordering
stableAvailFieldsCmp = cmpList (stableNameCmp `on` fst)

-- -----------------------------------------------------------------------------
-- Operations on AvailInfo

availsToNameSet :: [AvailInfo] -> NameSet
availsToNameSet avails = foldr add emptyNameSet avails
      where add avail set = addListToNameSet set (availNames avail)

availsToNameSetWithSelectors :: [AvailInfo] -> NameSet
availsToNameSetWithSelectors avails = foldr add emptyNameSet avails
      where add avail set = addListToNameSet set (availNamesWithSelectors avail)

availsToNameEnv :: [AvailInfo] -> NameEnv AvailInfo
availsToNameEnv avails = foldr add emptyNameEnv avails
     where add avail env = extendNameEnvList env
                                (zip (availNames avail) (repeat avail))

-- | Just the main name made available, i.e. not the available pieces
-- of type or class brought into scope by the 'GenAvailInfo'
availName :: AvailInfo -> Name
availName (Avail n)       = n
availName (AvailTC n _ _) = n

-- | All names made available by the availability information (excluding selectors)
availNames :: AvailInfo -> [Name]
availNames (Avail n)         = [n]
availNames (AvailTC _ ns fs) = ns ++ availFieldsNames fs

-- | All names made available by the availability information (including selectors)
availNamesWithSelectors :: AvailInfo -> [Name]
availNamesWithSelectors (Avail n)         = [n]
availNamesWithSelectors (AvailTC _ ns fs) = ns ++ availFieldsNamesWithSelectors fs

-- | Names for non-fields made available by the availability information
availNonFldNames :: AvailInfo -> [Name]
availNonFldNames (Avail n)        = [n]
availNonFldNames (AvailTC _ ns _) = ns

-- | Fields made available by the availability information
availFlds :: AvailInfo -> AvailFields
availFlds (AvailTC _ _ fs) = fs
availFlds _                = []

-- | Overloaded fields made available by the availability information
availOverloadedFlds :: AvailInfo -> [(FieldLabelString, Name)]
availOverloadedFlds avail = [ (lbl, sel) | (sel, Just lbl) <- availFlds avail ]

-- -----------------------------------------------------------------------------
-- Operations on AvailFields

availFieldsLabels :: AvailFields -> [FieldLabelString]
availFieldsLabels = map help
  where
    help (_,   Just lbl) = lbl
    help (sel, Nothing)  = occNameFS $ nameOccName sel

availFieldsNames :: AvailFlds name -> [name]
availFieldsNames fs = [ n | (n, Nothing) <- fs ]

availFieldsNamesWithSelectors :: AvailFlds name -> [name]
availFieldsNamesWithSelectors = map fst

fieldLabelToAvailField :: FieldLabel -> AvailField
fieldLabelToAvailField fl = (flSelector fl, mb_lbl)
  where
    mb_lbl | flIsOverloaded fl = Just (flLabel fl)
           | otherwise         = Nothing

fieldLabelsToAvailFields :: [FieldLabel] -> AvailFields
fieldLabelsToAvailFields = map fieldLabelToAvailField


-- -----------------------------------------------------------------------------
-- Printing

instance Outputable AvailInfo where
   ppr = pprAvail

pprAvail :: AvailInfo -> SDoc
pprAvail (Avail n)         = ppr n
pprAvail (AvailTC n ns fs) = ppr n <> braces (hsep (punctuate comma (map ppr ns ++ map pprAvailField fs)))

pprAvailField :: Outputable name => AvailFld name -> SDoc
pprAvailField (n, Nothing)  = ppr n
pprAvailField (_, Just lbl) = ppr lbl

instance Binary AvailInfo where
    put_ bh (Avail aa) = do
            putByte bh 0
            put_ bh aa
    put_ bh (AvailTC ab ac ad) = do
            putByte bh 1
            put_ bh ab
            put_ bh ac
            put_ bh ad
    get bh = do
            h <- getByte bh
            case h of
              0 -> do aa <- get bh
                      return (Avail aa)
              _ -> do ab <- get bh
                      ac <- get bh
                      ad <- get bh
                      return (AvailTC ab ac ad)
