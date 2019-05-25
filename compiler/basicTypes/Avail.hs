{-# LANGUAGE DeriveDataTypeable #-}
--
-- (c) The University of Glasgow
--

module Avail (
    Avails,
    AvailInfo(..),
    avail,
    availsToNameSet,
    availsToNameSetWithSelectors,
    availsToNameEnv,
    availName, availNames, availNonFldNames,
    availNamesWithSelectors,
    availFlds,
    availsNamesWithOccs,
    availNamesWithOccs,
    stableAvailCmp,
    filterAvail,
    filterAvails
  ) where

import GhcPrelude

import Name
import NameEnv
import NameSet

import FieldLabel
import Util

import Data.Data ( Data )
import Data.Function

-- -----------------------------------------------------------------------------
-- The AvailInfo type

-- | Records what things are \"available\", i.e. in scope
data AvailInfo

  -- | An ordinary identifier in scope
  = Avail Name

  -- | A type or class in scope
  --
  -- The __AvailTC Invariant__: If the type or class is itself to be in scope,
  -- it must be /first/ in this list.  Thus, typically:
  --
  -- > AvailTC Eq [Eq, ==, \/=] []
  | AvailTC
       Name         -- ^ The name of the type or class
       [Name]       -- ^ The available pieces of type or class,
                    -- excluding field selectors.
       [FieldLabel] -- ^ The record fields of the type
                    -- (see Note [Representing fields in AvailInfo]).

   deriving ( Eq    -- ^ Used when deciding if the interface has changed
            , Data )

-- | A collection of 'AvailInfo' - several things that are \"available\"
type Avails = [AvailInfo]

{-
Note [Representing fields in AvailInfo]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When -XDuplicateRecordFields is disabled (the normal case), a
datatype like

  data T = MkT { foo :: Int }

gives rise to the AvailInfo

  AvailTC T [T, MkT] [FieldLabel "foo" False foo]

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

  AvailTC F [ F, MkFInt, MkFBool ]
            [ FieldLabel "foo" True $sel:foo:MkFInt
            , FieldLabel "foo" True $sel:foo:MkFBool ]

Moreover, note that the flIsOverloaded flag need not be the same for
all the elements of the list.  In the example above, this occurs if
the two data instances are defined in different modules, one with
`-XDuplicateRecordFields` enabled and one with it disabled.  Thus it
is possible to have

  AvailTC F [ F, MkFInt, MkFBool ]
            [ FieldLabel "foo" True $sel:foo:MkFInt
            , FieldLabel "foo" False foo ]

If the two data instances are defined in different modules, both
without `-XDuplicateRecordFields`, it will be impossible to export
them from the same module (even with `-XDuplicateRecordfields`
enabled), because they would be represented identically.  The
workaround here is to enable `-XDuplicateRecordFields` on the defining
modules.
-}

-- | Compare lexicographically
stableAvailCmp :: AvailInfo -> AvailInfo -> Ordering
stableAvailCmp (Avail n1)       (Avail n2)   = n1 `stableNameCmp` n2
stableAvailCmp (Avail {})         (AvailTC {})   = LT
stableAvailCmp (AvailTC n ns nfs) (AvailTC m ms mfs) =
    (n `stableNameCmp` m) `thenCmp`
    (cmpList stableNameCmp ns ms) `thenCmp`
    (cmpList (stableNameCmp `on` flSelector) nfs mfs)
stableAvailCmp (AvailTC {})       (Avail {})     = GT

avail :: Name -> AvailInfo
avail n = Avail n

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
availName (Avail n)     = n
availName (AvailTC n _ _) = n

-- | All names made available by the availability information (excluding overloaded selectors)
availNames :: AvailInfo -> [Name]
availNames (Avail n)         = [n]
availNames (AvailTC _ ns fs) = ns ++ [ flSelector f | f <- fs, not (flIsOverloaded f) ]

-- | All names made available by the availability information (including overloaded selectors)
availNamesWithSelectors :: AvailInfo -> [Name]
availNamesWithSelectors (Avail n)         = [n]
availNamesWithSelectors (AvailTC _ ns fs) = ns ++ map flSelector fs

-- | Names for non-fields made available by the availability information
availNonFldNames :: AvailInfo -> [Name]
availNonFldNames (Avail n)        = [n]
availNonFldNames (AvailTC _ ns _) = ns

-- | Fields made available by the availability information
availFlds :: AvailInfo -> [FieldLabel]
availFlds (AvailTC _ _ fs) = fs
availFlds _                = []

availsNamesWithOccs :: [AvailInfo] -> [(Name, OccName)]
availsNamesWithOccs = concatMap availNamesWithOccs

-- | 'Name's made available by the availability information, paired with
-- the 'OccName' used to refer to each one.
--
-- When @DuplicateRecordFields@ is in use, the 'Name' may be the
-- mangled name of a record selector (e.g. @$sel:foo:MkT@) while the
-- 'OccName' will be the label of the field (e.g. @foo@).
--
-- See Note [Representing fields in AvailInfo].
availNamesWithOccs :: AvailInfo -> [(Name, OccName)]
availNamesWithOccs (Avail n) = [(n, nameOccName n)]
availNamesWithOccs (AvailTC _ ns fs)
  = [ (n, nameOccName n) | n <- ns ] ++
    [ (flSelector fl, mkVarOccFS (flLabel fl)) | fl <- fs ]

-- -----------------------------------------------------------------------------
-- Utility

-- | filters 'AvailInfo's by the given predicate
filterAvails  :: (Name -> Bool) -> [AvailInfo] -> [AvailInfo]
filterAvails keep avails = foldr (filterAvail keep) [] avails

-- | filters an 'AvailInfo' by the given predicate
filterAvail :: (Name -> Bool) -> AvailInfo -> [AvailInfo] -> [AvailInfo]
filterAvail keep ie rest =
  case ie of
    Avail n | keep n    -> ie : rest
            | otherwise -> rest
    AvailTC tc ns fs ->
        let ns' = filter keep ns
            fs' = filter (keep . flSelector) fs in
        if null ns' && null fs' then rest else AvailTC tc ns' fs' : rest
