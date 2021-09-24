
{-# LANGUAGE DeriveDataTypeable #-}
--
-- (c) The University of Glasgow
--

module GHC.Types.Avail (
    Avails,
    AvailInfo(..),
    avail,
    availField,
    availTC,
    availsToNameSet,
    availsToNameSetWithSelectors,
    availsToNameEnv,
    availExportsDecl,
    availName, availGreName,
    availNames, availNonFldNames,
    availNamesWithSelectors,
    availFlds,
    availGreNames,
    availSubordinateGreNames,
    stableAvailCmp,
    plusAvail,
    trimAvail,
    filterAvail,
    filterAvails,
    nubAvails,

    GreName(..),
    greNameMangledName,
    greNamePrintableName,
    greNameSrcSpan,
    greNameFieldLabel,
    partitionGreNames,
    stableGreNameCmp,
  ) where

import GHC.Prelude

import GHC.Types.Name
import GHC.Types.Name.Env
import GHC.Types.Name.Set
import GHC.Types.SrcLoc

import GHC.Types.FieldLabel
import GHC.Utils.Binary
import GHC.Data.List.SetOps
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Misc
import GHC.Utils.Constants (debugIsOn)

import Data.Data ( Data )
import Data.Either ( partitionEithers )
import Data.List ( find )
import Data.Maybe

-- -----------------------------------------------------------------------------
-- The AvailInfo type

-- | Records what things are \"available\", i.e. in scope
data AvailInfo

  -- | An ordinary identifier in scope, or a field label without a parent type
  -- (see Note [Representing pattern synonym fields in AvailInfo]).
  = Avail GreName

  -- | A type or class in scope
  --
  -- The __AvailTC Invariant__: If the type or class is itself to be in scope,
  -- it must be /first/ in this list.  Thus, typically:
  --
  -- > AvailTC Eq [Eq, ==, \/=]
  | AvailTC
       Name         -- ^ The name of the type or class
       [GreName]      -- ^ The available pieces of type or class
                    -- (see Note [Representing fields in AvailInfo]).

   deriving ( Eq    -- ^ Used when deciding if the interface has changed
            , Data )

-- | A collection of 'AvailInfo' - several things that are \"available\"
type Avails = [AvailInfo]

{-
Note [Representing fields in AvailInfo]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See also Note [FieldLabel] in GHC.Types.FieldLabel.

When -XDuplicateRecordFields is disabled (the normal case), a
datatype like

  data T = MkT { foo :: Int }

gives rise to the AvailInfo

  AvailTC T [T, MkT, FieldLabel "foo" NoDuplicateRecordFields FieldSelectors foo]

whereas if -XDuplicateRecordFields is enabled it gives

  AvailTC T [T, MkT, FieldLabel "foo" DuplicateRecordFields FieldSelectors $sel:foo:MkT]

where the label foo does not match the selector name $sel:foo:MkT.

The labels in a field list are not necessarily unique:
data families allow the same parent (the family tycon) to have
multiple distinct fields with the same label. For example,

  data family F a
  data instance F Int  = MkFInt { foo :: Int }
  data instance F Bool = MkFBool { foo :: Bool}

gives rise to

  AvailTC F [ F, MkFInt, MkFBool
            , FieldLabel "foo" DuplicateRecordFields FieldSelectors $sel:foo:MkFInt
            , FieldLabel "foo" DuplicateRecordFields FieldSelectors $sel:foo:MkFBool ]

Moreover, note that the flHasDuplicateRecordFields or flFieldSelectors flags
need not be the same for all the elements of the list.  In the example above,
this occurs if the two data instances are defined in different modules, with
different states of the `-XDuplicateRecordFields` or `-XNoFieldSelectors`
extensions.  Thus it is possible to have

  AvailTC F [ F, MkFInt, MkFBool
            , FieldLabel "foo" DuplicateRecordFields FieldSelectors $sel:foo:MkFInt
            , FieldLabel "foo" NoDuplicateRecordFields FieldSelectors foo ]

If the two data instances are defined in different modules, both without
`-XDuplicateRecordFields` or `-XNoFieldSelectors`, it will be impossible to
export them from the same module (even with `-XDuplicateRecordfields` enabled),
because they would be represented identically.  The workaround here is to enable
`-XDuplicateRecordFields` or `-XNoFieldSelectors` on the defining modules.  See
also #13352.


Note [Representing pattern synonym fields in AvailInfo]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Record pattern synonym fields cannot be represented using AvailTC like fields of
normal record types (see Note [Representing fields in AvailInfo]), because they
do not always have a parent type constructor.  So we represent them using the
Avail constructor, with a NormalGreName that carries the underlying FieldLabel.

Thus under -XDuplicateRecordFields -XPatternSynoynms, the declaration

  pattern MkFoo{f} = Bar f

gives rise to the AvailInfo

  Avail (NormalGreName MkFoo)
  Avail (FieldGreName (FieldLabel "f" True $sel:f:MkFoo))

However, if `f` is bundled with a type constructor `T` by using `T(MkFoo,f)` in
an export list, then whenever `f` is imported the parent will be `T`,
represented as

  AvailTC T [ NormalGreName T
            , NormalGreName MkFoo
            , FieldGreName (FieldLabel "f" True $sel:f:MkFoo) ]

See also Note [GreNames] in GHC.Types.Name.Reader.
-}

-- | Compare lexicographically
stableAvailCmp :: AvailInfo -> AvailInfo -> Ordering
stableAvailCmp (Avail c1)     (Avail c2)     = c1 `stableGreNameCmp` c2
stableAvailCmp (Avail {})     (AvailTC {})   = LT
stableAvailCmp (AvailTC n ns) (AvailTC m ms) = (n `stableNameCmp` m) `thenCmp`
                                               (cmpList stableGreNameCmp ns ms)
stableAvailCmp (AvailTC {})   (Avail {})     = GT

stableGreNameCmp :: GreName -> GreName -> Ordering
stableGreNameCmp (NormalGreName n1) (NormalGreName n2) = n1 `stableNameCmp` n2
stableGreNameCmp (NormalGreName {}) (FieldGreName  {}) = LT
stableGreNameCmp (FieldGreName  f1) (FieldGreName  f2) = flSelector f1 `stableNameCmp` flSelector f2
stableGreNameCmp (FieldGreName  {}) (NormalGreName {}) = GT

avail :: Name -> AvailInfo
avail n = Avail (NormalGreName n)

availField :: FieldLabel -> AvailInfo
availField fl = Avail (FieldGreName fl)

availTC :: Name -> [Name] -> [FieldLabel] -> AvailInfo
availTC n ns fls = AvailTC n (map NormalGreName ns ++ map FieldGreName fls)


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

-- | Does this 'AvailInfo' export the parent decl?  This depends on the
-- invariant that the parent is first if it appears at all.
availExportsDecl :: AvailInfo -> Bool
availExportsDecl (AvailTC ty_name names)
  | n : _ <- names = NormalGreName ty_name == n
  | otherwise      = False
availExportsDecl _ = True

-- | Just the main name made available, i.e. not the available pieces
-- of type or class brought into scope by the 'AvailInfo'
availName :: AvailInfo -> Name
availName (Avail n)     = greNameMangledName n
availName (AvailTC n _) = n

availGreName :: AvailInfo -> GreName
availGreName (Avail c) = c
availGreName (AvailTC n _) = NormalGreName n

-- | All names made available by the availability information (excluding overloaded selectors)
availNames :: AvailInfo -> [Name]
availNames (Avail c) = childNonOverloadedNames c
availNames (AvailTC _ cs) = concatMap childNonOverloadedNames cs

childNonOverloadedNames :: GreName -> [Name]
childNonOverloadedNames (NormalGreName n) = [n]
childNonOverloadedNames (FieldGreName fl) = [ flSelector fl | not (flIsOverloaded fl) ]

-- | All names made available by the availability information (including overloaded selectors)
availNamesWithSelectors :: AvailInfo -> [Name]
availNamesWithSelectors (Avail c) = [greNameMangledName c]
availNamesWithSelectors (AvailTC _ cs) = map greNameMangledName cs

-- | Names for non-fields made available by the availability information
availNonFldNames :: AvailInfo -> [Name]
availNonFldNames (Avail (NormalGreName n)) = [n]
availNonFldNames (Avail (FieldGreName {})) = []
availNonFldNames (AvailTC _ ns) = mapMaybe f ns
  where
    f (NormalGreName n) = Just n
    f (FieldGreName {}) = Nothing

-- | Fields made available by the availability information
availFlds :: AvailInfo -> [FieldLabel]
availFlds (Avail c) = maybeToList (greNameFieldLabel c)
availFlds (AvailTC _ cs) = mapMaybe greNameFieldLabel cs

-- | Names and fields made available by the availability information.
availGreNames :: AvailInfo -> [GreName]
availGreNames (Avail c)      = [c]
availGreNames (AvailTC _ cs) = cs

-- | Names and fields made available by the availability information, other than
-- the main decl itself.
availSubordinateGreNames :: AvailInfo -> [GreName]
availSubordinateGreNames (Avail {}) = []
availSubordinateGreNames avail@(AvailTC _ ns)
  | availExportsDecl avail = tail ns
  | otherwise              = ns


-- | Used where we may have an ordinary name or a record field label.
-- See Note [GreNames] in GHC.Types.Name.Reader.
data GreName = NormalGreName Name
             | FieldGreName FieldLabel
    deriving (Data, Eq)

instance Outputable GreName where
  ppr (NormalGreName n) = ppr n
  ppr (FieldGreName fl) = ppr fl

instance HasOccName GreName where
  occName (NormalGreName n) = occName n
  occName (FieldGreName fl) = occName fl

-- | A 'Name' for internal use, but not for output to the user.  For fields, the
-- 'OccName' will be the selector.  See Note [GreNames] in GHC.Types.Name.Reader.
greNameMangledName :: GreName -> Name
greNameMangledName (NormalGreName n) = n
greNameMangledName (FieldGreName fl) = flSelector fl

-- | A 'Name' suitable for output to the user.  For fields, the 'OccName' will
-- be the field label.  See Note [GreNames] in GHC.Types.Name.Reader.
greNamePrintableName :: GreName -> Name
greNamePrintableName (NormalGreName n) = n
greNamePrintableName (FieldGreName fl) = fieldLabelPrintableName fl

greNameSrcSpan :: GreName -> SrcSpan
greNameSrcSpan (NormalGreName n) = nameSrcSpan n
greNameSrcSpan (FieldGreName fl) = nameSrcSpan (flSelector fl)

greNameFieldLabel :: GreName -> Maybe FieldLabel
greNameFieldLabel (NormalGreName {}) = Nothing
greNameFieldLabel (FieldGreName fl)  = Just fl

partitionGreNames :: [GreName] -> ([Name], [FieldLabel])
partitionGreNames = partitionEithers . map to_either
  where
    to_either (NormalGreName n) = Left n
    to_either (FieldGreName fl) = Right fl


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
  = case (NormalGreName n1==s1, NormalGreName n2==s2) of  -- Maintain invariant the parent is first
       (True,True)   -> AvailTC n1 (s1 : (ss1 `unionLists` ss2))
       (True,False)  -> AvailTC n1 (s1 : (ss1 `unionLists` (s2:ss2)))
       (False,True)  -> AvailTC n1 (s2 : ((s1:ss1) `unionLists` ss2))
       (False,False) -> AvailTC n1 ((s1:ss1) `unionLists` (s2:ss2))
plusAvail a1 a2 = pprPanic "GHC.Rename.Env.plusAvail" (hsep [ppr a1,ppr a2])

-- | trims an 'AvailInfo' to keep only a single name
trimAvail :: AvailInfo -> Name -> AvailInfo
trimAvail avail@(Avail {})         _ = avail
trimAvail avail@(AvailTC n ns) m = case find ((== m) . greNameMangledName) ns of
    Just c  -> AvailTC n [c]
    Nothing -> pprPanic "trimAvail" (hsep [ppr avail, ppr m])

-- | filters 'AvailInfo's by the given predicate
filterAvails  :: (Name -> Bool) -> [AvailInfo] -> [AvailInfo]
filterAvails keep avails = foldr (filterAvail keep) [] avails

-- | filters an 'AvailInfo' by the given predicate
filterAvail :: (Name -> Bool) -> AvailInfo -> [AvailInfo] -> [AvailInfo]
filterAvail keep ie rest =
  case ie of
    Avail c | keep (greNameMangledName c) -> ie : rest
            | otherwise -> rest
    AvailTC tc cs ->
        let cs' = filter (keep . greNameMangledName) cs
        in if null cs' then rest else AvailTC tc cs' : rest


-- | Combines 'AvailInfo's from the same family
-- 'avails' may have several items with the same availName
-- E.g  import Ix( Ix(..), index )
-- will give Ix(Ix,index,range) and Ix(index)
-- We want to combine these; addAvail does that
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
  = ppr n <> braces (fsep (punctuate comma (map ppr ns)))

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

instance Binary GreName where
    put_ bh (NormalGreName aa) = do
            putByte bh 0
            put_ bh aa
    put_ bh (FieldGreName ab) = do
            putByte bh 1
            put_ bh ab
    get bh = do
            h <- getByte bh
            case h of
              0 -> do aa <- get bh
                      return (NormalGreName aa)
              _ -> do ab <- get bh
                      return (FieldGreName ab)
