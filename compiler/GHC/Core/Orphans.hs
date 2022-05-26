{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998
-}

{-# LANGUAGE DeriveDataTypeable, FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BangPatterns #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns   #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

-- | GHC.Core holds all the main data types for use by for the Glasgow Haskell Compiler midsection
module GHC.Core.Orphans (
        IsOrphan(..), isOrphan, notOrphan, chooseOrphanAnchor,
    ) where

import GHC.Prelude

import GHC.Types.Name
import GHC.Types.Name.Set
import GHC.Types.Unique.Set

import GHC.Utils.Binary

import Data.Data hiding (TyCon)

-- | Is this instance an orphan?  If it is not an orphan, contains an 'OccName'
-- witnessing the instance's non-orphanhood.
-- See Note [Orphans]
data IsOrphan
  = IsOrphan
  | NotOrphan !OccName -- The OccName 'n' witnesses the instance's non-orphanhood
                      -- In that case, the instance is fingerprinted as part
                      -- of the definition of 'n's definition
    deriving Data

-- | Returns true if 'IsOrphan' is orphan.
isOrphan :: IsOrphan -> Bool
isOrphan IsOrphan = True
isOrphan _ = False

-- | Returns true if 'IsOrphan' is not an orphan.
notOrphan :: IsOrphan -> Bool
notOrphan NotOrphan{} = True
notOrphan _ = False

chooseOrphanAnchor :: NameSet -> IsOrphan
-- Something (rule, instance) is relate to all the Names in this
-- list. Choose one of them to be an "anchor" for the orphan.  We make
-- the choice deterministic to avoid gratuitous changes in the ABI
-- hash (#4012).  Specifically, use lexicographic comparison of
-- OccName rather than comparing Uniques
--
-- NB: 'minimum' use Ord, and (Ord OccName) works lexicographically
--
chooseOrphanAnchor local_names
  | isEmptyNameSet local_names = IsOrphan
  | otherwise                  = NotOrphan (minimum occs)
  where
    occs = map nameOccName $ nonDetEltsUniqSet local_names
    -- It's OK to use nonDetEltsUFM here, see comments above

instance Binary IsOrphan where
    put_ bh IsOrphan = putByte bh 0
    put_ bh (NotOrphan n) = do
        putByte bh 1
        put_ bh n
    get bh = do
        h <- getByte bh
        case h of
            0 -> return IsOrphan
            _ -> do
                n <- get bh
                return $ NotOrphan n

{-
Note [Orphans]
~~~~~~~~~~~~~~
Class instances, rules, and family instances are divided into orphans
and non-orphans.  Roughly speaking, an instance/rule is an orphan if
its left hand side mentions nothing defined in this module.  Orphan-hood
has two major consequences

 * A module that contains orphans is called an "orphan module".  If
   the module being compiled depends (transitively) on an orphan
   module M, then M.hi is read in regardless of whether M is otherwise
   needed. This is to ensure that we don't miss any instance decls in
   M.  But it's painful, because it means we need to keep track of all
   the orphan modules below us.

 * A non-orphan is not finger-printed separately.  Instead, for
   fingerprinting purposes it is treated as part of the entity it
   mentions on the LHS.  For example
      data T = T1 | T2
      instance Eq T where ....
   The instance (Eq T) is incorporated as part of T's fingerprint.

   In contrast, orphans are all fingerprinted together in the
   mi_orph_hash field of the ModIface.

   See GHC.Iface.Recomp.addFingerprints.

Orphan-hood is computed
  * For class instances:
      when we make a ClsInst
    (because it is needed during instance lookup)

  * For rules and family instances:
       when we generate an IfaceRule (GHC.Iface.Make.coreRuleToIfaceRule)
                     or IfaceFamInst (GHC.Iface.Make.instanceToIfaceInst)
-}
