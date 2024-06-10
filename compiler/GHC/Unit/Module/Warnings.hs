{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

-- | Warnings for a module
module GHC.Unit.Module.Warnings
   ( WarningCategory(..)
   , mkWarningCategory
   , defaultWarningCategory
   , validWarningCategory
   , InWarningCategory(..)
   , fromWarningCategory

   , WarningCategorySet
   , emptyWarningCategorySet
   , completeWarningCategorySet
   , nullWarningCategorySet
   , elemWarningCategorySet
   , insertWarningCategorySet
   , deleteWarningCategorySet

   , Warnings (..)
   , WarningTxt (..)
   , LWarningTxt
   , DeclWarnOccNames
   , ExportWarnNames
   , warningTxtCategory
   , warningTxtMessage
   , warningTxtSame
   , pprWarningTxtForMsg
   , emptyWarn
   , mkIfaceDeclWarnCache
   , mkIfaceExportWarnCache
   , emptyIfaceWarnCache
   , insertWarnDecls
   , insertWarnExports
   )
where

import GHC.Prelude

import GHC.Data.FastString (FastString, mkFastString, unpackFS)
import GHC.Types.SourceText
import GHC.Types.Name.Occurrence
import GHC.Types.Name.Env
import GHC.Types.Name (Name)
import GHC.Types.SrcLoc
import GHC.Types.Unique
import GHC.Types.Unique.Set
import GHC.Hs.Doc
import GHC.Hs.Extension
import GHC.Parser.Annotation

import GHC.Utils.Outputable
import GHC.Utils.Outputable.Instances ()
import GHC.Utils.Binary
import GHC.Unicode

import Language.Haskell.Syntax.Extension
import Language.Haskell.Syntax.Lit

import Data.Data
import Data.List (isPrefixOf)
import GHC.Generics ( Generic )
import Control.DeepSeq

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

data InWarningCategory
  = InWarningCategory
    { iwc_in :: !(EpToken "in"),
      iwc_st :: !SourceText,
      iwc_wc :: (LocatedE WarningCategory)
    } deriving Data

fromWarningCategory :: WarningCategory -> InWarningCategory
fromWarningCategory wc = InWarningCategory noAnn NoSourceText (noLocA wc)


-- See Note [Warning categories]
newtype WarningCategory = WarningCategory FastString
  deriving (Binary, Data, Eq, Outputable, Show, Uniquable, NFData)

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

type LWarningTxt pass = XRec pass (WarningTxt pass)

-- | Warning Text
--
-- reason/explanation from a WARNING or DEPRECATED pragma
data WarningTxt pass
   = WarningTxt
      (Maybe (LocatedE InWarningCategory))
        -- ^ Warning category attached to this WARNING pragma, if any;
        -- see Note [Warning categories]
      SourceText
      [LocatedE (WithHsDocIdentifiers (StringLit pass) pass)]
   | DeprecatedTxt
      SourceText
      [LocatedE (WithHsDocIdentifiers (StringLit pass) pass)]
  deriving Generic

-- | To which warning category does this WARNING or DEPRECATED pragma belong?
-- See Note [Warning categories].
warningTxtCategory :: WarningTxt pass -> WarningCategory
warningTxtCategory (WarningTxt (Just (L _ (InWarningCategory _  _ (L _ cat)))) _ _) = cat
warningTxtCategory _ = defaultWarningCategory

-- | The message that the WarningTxt was specified to output
warningTxtMessage :: WarningTxt p -> [LocatedE (WithHsDocIdentifiers (StringLit p) p)]
warningTxtMessage (WarningTxt _ _ m) = m
warningTxtMessage (DeprecatedTxt _ m) = m

-- | True if the 2 WarningTxts have the same category and messages
warningTxtSame :: WarningTxt p -> WarningTxt p -> Bool
warningTxtSame w1 w2
  = warningTxtCategory w1 == warningTxtCategory w2
  && literal_message w1 == literal_message w2
  && same_type
  where
    literal_message :: WarningTxt p -> [StringLit p]
    literal_message = map (hsDocString . unLoc) . warningTxtMessage
    same_type | DeprecatedTxt {} <- w1, DeprecatedTxt {} <- w2 = True
              | WarningTxt {} <- w1, WarningTxt {} <- w2       = True
              | otherwise                                      = False

deriving instance Eq InWarningCategory

deriving instance (Eq (IdP (GhcPass pass))) => Eq (WarningTxt (GhcPass pass))

type instance Anno (WarningTxt (GhcPass pass)) = SrcSpanAnnP

instance Outputable InWarningCategory where
  ppr (InWarningCategory _ _ wt) = text "in" <+> doubleQuotes (ppr wt)


instance
  (XStringLit (GhcPass p) ~ (SourceText, Maybe NoCommentsLocation))
  => Outputable (WarningTxt (GhcPass p)) where
    ppr (WarningTxt mcat lsrc ws)
      = case lsrc of
            NoSourceText   -> pp_ws ws
            SourceText src -> ftext src <+> ctg_doc <+> pp_ws ws <+> text "#-}"
        where
          ctg_doc = maybe empty (\ctg -> ppr ctg) mcat


    ppr (DeprecatedTxt lsrc  ds)
      = case lsrc of
          NoSourceText   -> pp_ws ds
          SourceText src -> ftext src <+> pp_ws ds <+> text "#-}"

pp_ws
  :: (XStringLit (GhcPass p) ~ (SourceText, Maybe NoCommentsLocation))
  => [LocatedE (WithHsDocIdentifiers (StringLit (GhcPass p)) (GhcPass p))] -> SDoc
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


-- | Warning information from a module
data Warnings pass
  = WarnSome (DeclWarnOccNames pass) -- ^ Names deprecated (may be empty)
             (ExportWarnNames pass)  -- ^ Exports deprecated (may be empty)
  | WarnAll (WarningTxt pass)        -- ^ Whole module deprecated

     -- For the module-specific names only an OccName is needed because
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
     --
     -- For export deprecations we need to know where the symbol comes from, since
     -- we need to be able to check if the deprecated export that was imported is
     -- the same thing as imported by another import, which would not trigger
     -- a deprecation message.

-- | Deprecated declarations
type DeclWarnOccNames pass = [(OccName, WarningTxt pass)]

-- | Names that are deprecated as exports
type ExportWarnNames pass = [(Name, WarningTxt pass)]

deriving instance Eq (IdP (GhcPass p)) => Eq (Warnings (GhcPass p))

emptyWarn :: Warnings p
emptyWarn = WarnSome [] []

-- | Constructs the cache for the 'mi_decl_warn_fn' field of a 'ModIface'
mkIfaceDeclWarnCache :: Warnings p -> OccName -> Maybe (WarningTxt p)
mkIfaceDeclWarnCache (WarnAll t) = \_ -> Just t
mkIfaceDeclWarnCache (WarnSome vs _) = lookupOccEnv (mkOccEnv vs)

-- | Constructs the cache for the 'mi_export_warn_fn' field of a 'ModIface'
mkIfaceExportWarnCache :: Warnings p -> Name -> Maybe (WarningTxt p)
mkIfaceExportWarnCache (WarnAll _) = const Nothing -- We do not want a double report of the module deprecation
mkIfaceExportWarnCache (WarnSome _ ds) = lookupNameEnv (mkNameEnv ds)

emptyIfaceWarnCache :: name -> Maybe (WarningTxt p)
emptyIfaceWarnCache _ = Nothing

insertWarnDecls :: Warnings p                -- ^ Existing warnings
                -> [(OccName, WarningTxt p)] -- ^ New declaration deprecations
                -> Warnings p                -- ^ Updated warnings
insertWarnDecls ws@(WarnAll _) _        = ws
insertWarnDecls (WarnSome wns wes) wns' = WarnSome (wns ++ wns') wes

insertWarnExports :: Warnings p             -- ^ Existing warnings
                  -> [(Name, WarningTxt p)] -- ^ New export deprecations
                  -> Warnings p             -- ^ Updated warnings
insertWarnExports ws@(WarnAll _) _ = ws
insertWarnExports (WarnSome wns wes) wes' = WarnSome wns (wes ++ wes')
