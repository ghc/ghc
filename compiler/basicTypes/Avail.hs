--
-- (c) The University of Glasgow
--

module Avail (
    Avails,
    AvailInfo(..),
    IsPatSyn(..),
    avail,
    patSynAvail,
    availsToNameSet,
    availsToNameSetWithSelectors,
    availsToNameEnv,
    availName, availNames, availNonFldNames,
    availNamesWithSelectors,
    availFlds,
    stableAvailCmp
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
data AvailInfo = Avail IsPatSyn Name      -- ^ An ordinary identifier in scope
               | AvailTC Name
                         [Name]
                         [FieldLabel]
                                 -- ^ A type or class in scope. Parameters:
                                 --
                                 --  1) The name of the type or class
                                 --  2) The available pieces of type or class,
                                 --     excluding field selectors.
                                 --  3) The record fields of the type
                                 --     (see Note [Representing fields in AvailInfo]).
                                 --
                                 -- The AvailTC Invariant:
                                 --   * If the type or class is itself
                                 --     to be in scope, it must be
                                 --     *first* in this list.  Thus,
                                 --     typically: @AvailTC Eq [Eq, ==, \/=]@
                deriving( Eq )
                        -- Equality used when deciding if the
                        -- interface has changed

data IsPatSyn = NotPatSyn | IsPatSyn deriving Eq

-- | A collection of 'AvailInfo' - several things that are \"available\"
type Avails = [AvailInfo]

{-
Note [Representing fields in AvailInfo]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When -XDuplicateRecordFields is disabled (the normal case), a
datatype like

  data T = MkT { foo :: Int }

gives rise to the AvailInfo

  AvailTC T [T, MkT] [FieldLabel "foo" False foo],

whereas if -XDuplicateRecordFields is enabled it gives

  AvailTC T [T, MkT] [FieldLabel "foo" True $sel:foo:MkT]

since the label does not match the selector name.

The labels in a field list are not necessarily unique:
data families allow the same parent (the family tycon) to have
multiple distinct fields with the same label. For example,

  data family F a
  data instance F Int  = MkFInt { foo :: Int }
  data instance F Bool = MkFBool { foo :: Bool}

gives rise to

  AvailTC F [F, MkFInt, MkFBool]
    [FieldLabel "foo" True $sel:foo:MkFInt, FieldLabel "foo" True $sel:foo:MkFBool].

Moreover, note that the flIsOverloaded flag need not be the same for
all the elements of the list.  In the example above, this occurs if
the two data instances are defined in different modules, one with
`-XDuplicateRecordFields` enabled and one with it disabled.  Thus it
is possible to have

  AvailTC F [F, MkFInt, MkFBool]
    [FieldLabel "foo" True $sel:foo:MkFInt, FieldLabel "foo" False foo].

If the two data instances are defined in different modules, both
without `-XDuplicateRecordFields`, it will be impossible to export
them from the same module (even with `-XDuplicateRecordfields`
enabled), because they would be represented identically.  The
workaround here is to enable `-XDuplicateRecordFields` on the defining
modules.
-}

-- | Compare lexicographically
stableAvailCmp :: AvailInfo -> AvailInfo -> Ordering
stableAvailCmp (Avail _ n1)       (Avail _ n2)   = n1 `stableNameCmp` n2
stableAvailCmp (Avail {})         (AvailTC {})   = LT
stableAvailCmp (AvailTC n ns nfs) (AvailTC m ms mfs) =
    (n `stableNameCmp` m) `thenCmp`
    (cmpList stableNameCmp ns ms) `thenCmp`
    (cmpList (stableNameCmp `on` flSelector) nfs mfs)
stableAvailCmp (AvailTC {})       (Avail {})     = GT

patSynAvail :: Name -> AvailInfo
patSynAvail n = Avail IsPatSyn n

avail :: Name -> AvailInfo
avail n = Avail NotPatSyn n

-- -----------------------------------------------------------------------------
-- Operations on AvailInfo

availsToNameSet :: [AvailInfo] -> NameSet
availsToNameSet avails = foldr add emptyNameSet avails
      where add avail set = extendNameSetList set (availNames avail)

availsToNameSetWithSelectors :: [AvailInfo] -> NameSet
availsToNameSetWithSelectors avails = foldr add emptyNameSet avails
      where add avail set = extendNameSetList set (availNamesWithSelectors avail)

availsToNameEnv :: [AvailInfo] -> NameEnv AvailInfo
availsToNameEnv avails = foldr add emptyNameEnv avails
     where add avail env = extendNameEnvList env
                                (zip (availNames avail) (repeat avail))

-- | Just the main name made available, i.e. not the available pieces
-- of type or class brought into scope by the 'GenAvailInfo'
availName :: AvailInfo -> Name
availName (Avail _ n)     = n
availName (AvailTC n _ _) = n

-- | All names made available by the availability information (excluding overloaded selectors)
availNames :: AvailInfo -> [Name]
availNames (Avail _ n)         = [n]
availNames (AvailTC _ ns fs) = ns ++ [ flSelector f | f <- fs, not (flIsOverloaded f) ]

-- | All names made available by the availability information (including overloaded selectors)
availNamesWithSelectors :: AvailInfo -> [Name]
availNamesWithSelectors (Avail _ n)         = [n]
availNamesWithSelectors (AvailTC _ ns fs) = ns ++ map flSelector fs

-- | Names for non-fields made available by the availability information
availNonFldNames :: AvailInfo -> [Name]
availNonFldNames (Avail _ n)        = [n]
availNonFldNames (AvailTC _ ns _) = ns

-- | Fields made available by the availability information
availFlds :: AvailInfo -> [FieldLabel]
availFlds (AvailTC _ _ fs) = fs
availFlds _                = []

-- -----------------------------------------------------------------------------
-- Printing

instance Outputable AvailInfo where
   ppr = pprAvail

pprAvail :: AvailInfo -> SDoc
pprAvail (Avail _ n)
  = ppr n
pprAvail (AvailTC n ns fs)
  = ppr n <> braces (sep [ fsep (punctuate comma (map ppr ns)) <> semi
                         , fsep (punctuate comma (map (ppr . flLabel) fs))])

instance Binary AvailInfo where
    put_ bh (Avail b aa) = do
            putByte bh 0
            put_ bh aa
            put_ bh b
    put_ bh (AvailTC ab ac ad) = do
            putByte bh 1
            put_ bh ab
            put_ bh ac
            put_ bh ad
    get bh = do
            h <- getByte bh
            case h of
              0 -> do aa <- get bh
                      b  <- get bh
                      return (Avail b aa)
              _ -> do ab <- get bh
                      ac <- get bh
                      ad <- get bh
                      return (AvailTC ab ac ad)

instance Binary IsPatSyn where
  put_ bh IsPatSyn = putByte bh 0
  put_ bh NotPatSyn = putByte bh 1
  get bh = do
    h <- getByte bh
    case h of
      0 -> return IsPatSyn
      _ -> return NotPatSyn
