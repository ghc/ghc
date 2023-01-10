{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Warnings for a module
module GHC.Unit.Module.Warnings
   ( WarningCategory
   , mkWarningCategory
   , defaultWarningCategory
   , validWarningCategory

   , WarningCategorySet
   , emptyWarningCategorySet
   , completeWarningCategorySet
   , nullWarningCategorySet
   , elemWarningCategorySet
   , insertWarningCategorySet
   , deleteWarningCategorySet

   , Warnings (..)
   , WarningTxt (..)
   , warningTxtCategory
   , pprWarningTxtForMsg
   , mkIfaceWarnCache
   , emptyIfaceWarnCache
   , plusWarns
   )
where

import GHC.Prelude

import GHC.Data.FastString (FastString, mkFastString, unpackFS)
import GHC.Types.SourceText
import GHC.Types.Name.Occurrence
import GHC.Types.SrcLoc
import GHC.Types.Unique
import GHC.Types.Unique.Set
import GHC.Hs.Doc
import GHC.Hs.Extension

import GHC.Utils.Outputable
import GHC.Utils.Binary
import GHC.Unicode

import Language.Haskell.Syntax.Extension

import Data.Data
import Data.List (isPrefixOf)
import GHC.Generics ( Generic )


{-
Note [Warning categories]
~~~~~~~~~~~~~~~~~~~~~~~~~
See GHC Proposal 541 for the design of the warning categories feature:
https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0541-warning-pragmas-with-categories.rst

A WARNING pragma may be annotated with a category such as "x-partial" written
after the 'in' keyword, like this:

    {-# WARNING in "x-partial" head "This function is partial..." #-}

This is represented by the 'Maybe (Located WarningCategory)' field in
'WarningTxt'.  The parser will accept an arbitrary string as the category name,
then the renamer (in 'rnWarningTxt') will check it contains only valid
characters, so we can generate a nicer error message than a parse error.

The corresponding warnings can then be controlled with the -Wx-partial,
-Wno-x-partial, -Werror=x-partial and -Wwarn=x-partial flags.  Such a flag is
distinguished from an 'unrecognisedWarning' by the flag parser testing
'validWarningCategory'.  The 'x-' prefix means we can still usually report an
unrecognised warning where the user has made a mistake.

A DEPRECATED pragma may not have a user-defined category, and is always treated
as belonging to the special category 'deprecations'.  Similarly, a WARNING
pragma without a category belongs to the 'deprecations' category.
Thus the '-Wdeprecations' flag will enable all of the following:

    {-# WARNING in "deprecations" foo "This function is deprecated..." #-}
    {-# WARNING foo "This function is deprecated..." #-}
    {-# DEPRECATED foo "This function is deprecated..." #-}

The '-Wwarnings-deprecations' flag is supported for backwards compatibility
purposes as being equivalent to '-Wdeprecations'.

The '-Wextended-warnings' warning group collects together all warnings with
user-defined categories, so they can be enabled or disabled
collectively. Moreover they are treated as being part of other warning groups
such as '-Wdefault' (see 'warningGroupIncludesExtendedWarnings').

'DynFlags' and 'DiagOpts' each contain a set of enabled and a set of fatal
warning categories, just as they do for the finite enumeration of 'WarningFlag's
built in to GHC.  These are represented as 'WarningCategorySet's to allow for
the possibility of them being infinite.

-}



-- See Note [Warning categories]
newtype WarningCategory = WarningCategory FastString
  deriving (Binary, Data, Eq, Outputable, Show, Uniquable)

mkWarningCategory :: FastString -> WarningCategory
mkWarningCategory = WarningCategory

-- | The @deprecations@ category is used for all DEPRECATED pragmas and for
-- WARNING pragmas that do not specify a category.
defaultWarningCategory :: WarningCategory
defaultWarningCategory = mkWarningCategory (mkFastString "deprecations")

-- | Is this warning category allowed to appear in user-defined WARNING pragmas?
-- It must either be the known category @deprecations@, or be a custom category
-- that begins with @x-@ and contains only valid characters (letters, numbers,
-- apostrophes and dashes).
validWarningCategory :: WarningCategory -> Bool
validWarningCategory cat@(WarningCategory c) =
    cat == defaultWarningCategory || ("x-" `isPrefixOf` s && all is_allowed s)
  where
    s = unpackFS c
    is_allowed c = isAlphaNum c || c == '\'' || c == '-'


-- | A finite or infinite set of warning categories.
--
-- Unlike 'WarningFlag', there are (in principle) infinitely many warning
-- categories, so we cannot necessarily enumerate all of them. However the set
-- is constructed by adding or removing categories one at a time, so we can
-- represent it as either a finite set of categories, or a cofinite set (where
-- we store the complement).
data WarningCategorySet =
    FiniteWarningCategorySet   (UniqSet WarningCategory)
      -- ^ The set of warning categories is the given finite set.
  | CofiniteWarningCategorySet (UniqSet WarningCategory)
      -- ^ The set of warning categories is infinite, so the constructor stores
      -- its (finite) complement.

-- | The empty set of warning categories.
emptyWarningCategorySet :: WarningCategorySet
emptyWarningCategorySet = FiniteWarningCategorySet emptyUniqSet

-- | The set consisting of all possible warning categories.
completeWarningCategorySet :: WarningCategorySet
completeWarningCategorySet = CofiniteWarningCategorySet emptyUniqSet

-- | Is this set empty?
nullWarningCategorySet :: WarningCategorySet -> Bool
nullWarningCategorySet (FiniteWarningCategorySet s) = isEmptyUniqSet s
nullWarningCategorySet CofiniteWarningCategorySet{} = False

-- | Does this warning category belong to the set?
elemWarningCategorySet :: WarningCategory -> WarningCategorySet -> Bool
elemWarningCategorySet c (FiniteWarningCategorySet   s) =      c `elementOfUniqSet` s
elemWarningCategorySet c (CofiniteWarningCategorySet s) = not (c `elementOfUniqSet` s)

-- | Insert an element into a warning category set.
insertWarningCategorySet :: WarningCategory -> WarningCategorySet -> WarningCategorySet
insertWarningCategorySet c (FiniteWarningCategorySet   s) = FiniteWarningCategorySet   (addOneToUniqSet   s c)
insertWarningCategorySet c (CofiniteWarningCategorySet s) = CofiniteWarningCategorySet (delOneFromUniqSet s c)

-- | Delete an element from a warning category set.
deleteWarningCategorySet :: WarningCategory -> WarningCategorySet -> WarningCategorySet
deleteWarningCategorySet c (FiniteWarningCategorySet   s) = FiniteWarningCategorySet   (delOneFromUniqSet s c)
deleteWarningCategorySet c (CofiniteWarningCategorySet s) = CofiniteWarningCategorySet (addOneToUniqSet   s c)


-- | Warning Text
--
-- reason/explanation from a WARNING or DEPRECATED pragma
data WarningTxt pass
   = WarningTxt
      (Maybe (Located WarningCategory))
        -- ^ Warning category attached to this WARNING pragma, if any;
        -- see Note [Warning categories]
      (Located SourceText)
      [Located (WithHsDocIdentifiers StringLiteral pass)]
   | DeprecatedTxt
      (Located SourceText)
      [Located (WithHsDocIdentifiers StringLiteral pass)]
  deriving Generic

-- | To which warning category does this WARNING or DEPRECATED pragma belong?
-- See Note [Warning categories].
warningTxtCategory :: WarningTxt pass -> WarningCategory
warningTxtCategory (WarningTxt (Just (L _ cat)) _ _) = cat
warningTxtCategory _ = defaultWarningCategory

deriving instance Eq (IdP pass) => Eq (WarningTxt pass)
deriving instance (Data pass, Data (IdP pass)) => Data (WarningTxt pass)

instance Outputable (WarningTxt pass) where
    ppr (WarningTxt _ lsrc ws)
      = case unLoc lsrc of
          NoSourceText   -> pp_ws ws
          SourceText src -> text src <+> pp_ws ws <+> text "#-}"

    ppr (DeprecatedTxt lsrc  ds)
      = case unLoc lsrc of
          NoSourceText   -> pp_ws ds
          SourceText src -> text src <+> pp_ws ds <+> text "#-}"

instance Binary (WarningTxt GhcRn) where
    put_ bh (WarningTxt c s w) = do
            putByte bh 0
            put_ bh $ unLoc <$> c
            put_ bh $ unLoc s
            put_ bh $ unLoc <$> w
    put_ bh (DeprecatedTxt s d) = do
            putByte bh 1
            put_ bh $ unLoc s
            put_ bh $ unLoc <$> d

    get bh = do
            h <- getByte bh
            case h of
              0 -> do c <- fmap noLoc <$> get bh
                      s <- noLoc <$> get bh
                      w <- fmap noLoc  <$> get bh
                      return (WarningTxt c s w)
              _ -> do s <- noLoc <$> get bh
                      d <- fmap noLoc <$> get bh
                      return (DeprecatedTxt s d)


pp_ws :: [Located (WithHsDocIdentifiers StringLiteral pass)] -> SDoc
pp_ws [l] = ppr $ unLoc l
pp_ws ws
  = text "["
    <+> vcat (punctuate comma (map (ppr . unLoc) ws))
    <+> text "]"


pprWarningTxtForMsg :: WarningTxt p -> SDoc
pprWarningTxtForMsg (WarningTxt _ _ ws)
                     = doubleQuotes (vcat (map (ftext . sl_fs . hsDocString . unLoc) ws))
pprWarningTxtForMsg (DeprecatedTxt _ ds)
                     = text "Deprecated:" <+>
                       doubleQuotes (vcat (map (ftext . sl_fs . hsDocString . unLoc) ds))


-- | Warning information for a module
data Warnings pass
  = NoWarnings                          -- ^ Nothing deprecated
  | WarnAll (WarningTxt pass)                  -- ^ Whole module deprecated
  | WarnSome [(OccName,WarningTxt pass)]     -- ^ Some specific things deprecated

     -- Only an OccName is needed because
     --    (1) a deprecation always applies to a binding
     --        defined in the module in which the deprecation appears.
     --    (2) deprecations are only reported outside the defining module.
     --        this is important because, otherwise, if we saw something like
     --
     --        {-# DEPRECATED f "" #-}
     --        f = ...
     --        h = f
     --        g = let f = undefined in f
     --
     --        we'd need more information than an OccName to know to say something
     --        about the use of f in h but not the use of the locally bound f in g
     --
     --        however, because we only report about deprecations from the outside,
     --        and a module can only export one value called f,
     --        an OccName suffices.
     --
     --        this is in contrast with fixity declarations, where we need to map
     --        a Name to its fixity declaration.

deriving instance Eq (IdP pass) => Eq (Warnings pass)

instance Binary (Warnings GhcRn) where
    put_ bh NoWarnings     = putByte bh 0
    put_ bh (WarnAll t) = do
            putByte bh 1
            put_ bh t
    put_ bh (WarnSome ts) = do
            putByte bh 2
            put_ bh ts

    get bh = do
            h <- getByte bh
            case h of
              0 -> return NoWarnings
              1 -> do aa <- get bh
                      return (WarnAll aa)
              _ -> do aa <- get bh
                      return (WarnSome aa)

-- | Constructs the cache for the 'mi_warn_fn' field of a 'ModIface'
mkIfaceWarnCache :: Warnings p -> OccName -> Maybe (WarningTxt p)
mkIfaceWarnCache NoWarnings  = \_ -> Nothing
mkIfaceWarnCache (WarnAll t) = \_ -> Just t
mkIfaceWarnCache (WarnSome pairs) = lookupOccEnv (mkOccEnv pairs)

emptyIfaceWarnCache :: OccName -> Maybe (WarningTxt p)
emptyIfaceWarnCache _ = Nothing

plusWarns :: Warnings p -> Warnings p -> Warnings p
plusWarns d NoWarnings = d
plusWarns NoWarnings d = d
plusWarns _ (WarnAll t) = WarnAll t
plusWarns (WarnAll t) _ = WarnAll t
plusWarns (WarnSome v1) (WarnSome v2) = WarnSome (v1 ++ v2)

