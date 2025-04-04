
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms #-}
--
-- (c) The University of Glasgow
--

module GHC.Types.Avail (
    Avails,
    AvailInfo(..),
    availsToNameSet,
    availsToNameEnv,
    availExportsDecl,
    availName,
    availNames,
    availSubordinateNames,
    stableAvailCmp,
    plusAvail,
    trimAvail,
    filterAvail,
    filterAvails,
    nubAvails,
    sortAvails,
    DetOrdAvails(DetOrdAvails, getDetOrdAvails, DefinitelyDeterministicAvails),
    emptyDetOrdAvails
  ) where

import GHC.Prelude

import GHC.Types.Name
import GHC.Types.Name.Env
import GHC.Types.Name.Set

import GHC.Utils.Binary
import GHC.Data.List.SetOps
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Constants (debugIsOn)

import Control.DeepSeq
import Data.Data ( Data )
import Data.Functor.Classes ( liftCompare )
import Data.List ( find, sortBy )
import qualified Data.Semigroup as S

-- -----------------------------------------------------------------------------
-- The AvailInfo type

-- | Records what things are \"available\", i.e. in scope
data AvailInfo

  -- | An ordinary identifier in scope, or a field label without a parent type
  -- (see Note [Representing pattern synonym fields in AvailInfo]).
  = Avail Name

  -- | A type or class in scope
  --
  -- The __AvailTC Invariant__: If the type or class is itself to be in scope,
  -- it must be /first/ in this list.  Thus, typically:
  --
  -- > AvailTC Eq [Eq, ==, \/=]
  | AvailTC
       Name      -- ^ The name of the type or class
       [Name]    -- ^ The available pieces of type or class

   deriving Data

-- | A collection of 'AvailInfo' - several things that are \"available\"
type Avails = [AvailInfo]

-- | Occurrences of Avails in interface files must be deterministically ordered
-- to guarantee interface file determinism.
--
-- We guarantee a deterministic order by either using the order explicitly
-- given by the user (e.g. in an explicit constructor export list) or instead
-- by sorting the avails with 'sortAvails'.
newtype DetOrdAvails = DefinitelyDeterministicAvails { getDetOrdAvails :: Avails }
  deriving newtype (Binary, Outputable, NFData)

instance Eq DetOrdAvails where
  a1 == a2 = compare a1 a2 == EQ
instance Ord DetOrdAvails where
  compare (DetOrdAvails a1) (DetOrdAvails a2) = go a1 a2
    where
      go [] [] = EQ
      go _  [] = LT
      go [] _  = GT
      go (a:as) (b:bs) =
        case (a, b) of
          (Avail {}, AvailTC {}) -> LT
          (AvailTC{}, Avail {}) -> GT
          (Avail n1, Avail n2) -> stableNameCmp n1 n2 S.<> go as bs
          (AvailTC n1 m1s, AvailTC n2 m2s) ->
            stableNameCmp n1 n2 S.<> foldMap (uncurry stableNameCmp) (zip m1s m2s) S.<> go as bs

-- | It's always safe to match on 'DetOrdAvails'
pattern DetOrdAvails :: Avails -> DetOrdAvails
pattern DetOrdAvails x <- DefinitelyDeterministicAvails x
{-# COMPLETE DetOrdAvails #-}

{- Note [Representing pattern synonym fields in AvailInfo]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Record pattern synonym fields cannot be represented using AvailTC like fields of
normal record types, because they do not always have a parent type constructor.
So we represent them using the Avail constructor.

Thus under -XDuplicateRecordFields -XPatternSynoynms, the declaration

  pattern MkFoo{f} = Bar f

gives rise to the AvailInfo

  Avail MkFoo, Avail f

However, if `f` is bundled with a type constructor `T` by using `T(MkFoo,f)` in
an export list, then whenever `f` is imported the parent will be `T`,
represented as

  AvailTC T [ T, MkFoo, f ]
-}

-- | Compare lexicographically
stableAvailCmp :: AvailInfo -> AvailInfo -> Ordering
stableAvailCmp (Avail c1)     (Avail c2)     = c1 `stableNameCmp` c2
stableAvailCmp (Avail {})     (AvailTC {})   = LT
stableAvailCmp (AvailTC n ns) (AvailTC m ms) = stableNameCmp n m S.<> liftCompare stableNameCmp ns ms
stableAvailCmp (AvailTC {})   (Avail {})     = GT

-- -----------------------------------------------------------------------------
-- Operations on AvailInfo

availsToNameSet :: [AvailInfo] -> NameSet
availsToNameSet avails = foldr add emptyNameSet avails
      where add avail set = extendNameSetList set (availNames avail)

availsToNameEnv :: [AvailInfo] -> NameEnv AvailInfo
availsToNameEnv avails = foldr add emptyNameEnv avails
     where add avail env = extendNameEnvList env
                                (zip (availNames avail) (repeat avail))

-- | Does this 'AvailInfo' export the parent decl?  This depends on the
-- invariant that the parent is first if it appears at all.
availExportsDecl :: AvailInfo -> Bool
availExportsDecl (AvailTC ty_name names)
  | n : _ <- names = ty_name == n
  | otherwise      = False
availExportsDecl _ = True

-- | Just the main name made available, i.e. not the available pieces
-- of type or class brought into scope by the 'AvailInfo'
availName :: AvailInfo -> Name
availName (Avail   n)   = n
availName (AvailTC n _) = n

-- | Names and fields made available by the availability information.
availNames :: AvailInfo -> [Name]
availNames (Avail c)      = [c]
availNames (AvailTC _ cs) = cs

-- | Names and fields made available by the availability information, other than
-- the main decl itself.
availSubordinateNames :: AvailInfo -> [Name]
availSubordinateNames (Avail {}) = []
availSubordinateNames avail@(AvailTC _ ns)
  | availExportsDecl avail = tail ns
  | otherwise              = ns

-- | Sort 'Avails'/'AvailInfo's
sortAvails :: Avails -> DetOrdAvails
sortAvails = DefinitelyDeterministicAvails . sortBy stableAvailCmp . map sort_subs
  where
    sort_subs :: AvailInfo -> AvailInfo
    sort_subs (Avail n) = Avail n
    sort_subs (AvailTC n []) = AvailTC n []
    sort_subs (AvailTC n (m:ms))
       | n == m
       = AvailTC n (m:sortBy stableNameCmp ms)
       | otherwise
       = AvailTC n (sortBy stableNameCmp (m:ms))
       -- Maintain the AvailTC Invariant

-- -----------------------------------------------------------------------------
-- Utility

plusAvail :: AvailInfo -> AvailInfo -> AvailInfo
plusAvail a1 a2
  | debugIsOn && availName a1 /= availName a2
  = pprPanic "GHC.Rename.Env.plusAvail names differ" (hsep [ppr a1,ppr a2])
plusAvail a1@(Avail {})         (Avail {})        = a1
plusAvail (AvailTC _ [])     a2@(AvailTC {})   = a2
plusAvail a1@(AvailTC {})       (AvailTC _ []) = a1
plusAvail (AvailTC n1 (s1:ss1)) (AvailTC n2 (s2:ss2))
  = case (n1 == s1, n2 == s2) of  -- Maintain invariant the parent is first
       (True,True)   -> AvailTC n1 (s1 : (ss1 `unionListsOrd` ss2))
       (True,False)  -> AvailTC n1 (s1 : (ss1 `unionListsOrd` (s2:ss2)))
       (False,True)  -> AvailTC n1 (s2 : ((s1:ss1) `unionListsOrd` ss2))
       (False,False) -> AvailTC n1 ((s1:ss1) `unionListsOrd` (s2:ss2))
plusAvail a1 a2 = pprPanic "GHC.Rename.Env.plusAvail" (hsep [ppr a1,ppr a2])

-- | trims an 'AvailInfo' to keep only a single name
trimAvail :: AvailInfo -> Name -> AvailInfo
trimAvail avail@(Avail {})         _ = avail
trimAvail avail@(AvailTC n ns) m = case find (== m) ns of
    Just c  -> AvailTC n [c]
    Nothing -> pprPanic "trimAvail" (hsep [ppr avail, ppr m])

-- | filters 'AvailInfo's by the given predicate
filterAvails  :: (Name -> Bool) -> [AvailInfo] -> [AvailInfo]
filterAvails keep avails = foldr (filterAvail keep) [] avails

-- | filters an 'AvailInfo' by the given predicate
filterAvail :: (Name -> Bool) -> AvailInfo -> [AvailInfo] -> [AvailInfo]
filterAvail keep ie rest =
  case ie of
    Avail c | keep c -> ie : rest
            | otherwise -> rest
    AvailTC tc cs ->
        let cs' = filter keep cs
        in if null cs' then rest else AvailTC tc cs' : rest


-- | Combines 'AvailInfo's from the same family
-- 'avails' may have several items with the same availName
-- E.g  import Ix( Ix(..), index )
-- will give Ix(Ix,index,range) and Ix(index)
-- We want to combine these; plusAvail does that
nubAvails :: [AvailInfo] -> [AvailInfo]
nubAvails avails = eltsDNameEnv (foldl' add emptyDNameEnv avails)
  where
    add env avail = extendDNameEnv_C plusAvail env (availName avail) avail

-- -----------------------------------------------------------------------------
-- Printing

instance Outputable AvailInfo where
   ppr = pprAvail

pprAvail :: AvailInfo -> SDoc
pprAvail (Avail n)
  = ppr n
pprAvail (AvailTC n ns)
  = ppr n <> braces (pprWithCommas ppr ns)

instance Binary AvailInfo where
    put_ bh (Avail aa) = do
            putByte bh 0
            put_ bh aa
    put_ bh (AvailTC ab ac) = do
            putByte bh 1
            put_ bh ab
            put_ bh ac
    get bh = do
            h <- getByte bh
            case h of
              0 -> do aa <- get bh
                      return (Avail aa)
              _ -> do ab <- get bh
                      ac <- get bh
                      return (AvailTC ab ac)

instance NFData AvailInfo where
  rnf (Avail n) = rnf n
  rnf (AvailTC a b) = rnf a `seq` rnf b

-- | Create an empty DetOrdAvails
emptyDetOrdAvails :: DetOrdAvails
emptyDetOrdAvails = DefinitelyDeterministicAvails []
