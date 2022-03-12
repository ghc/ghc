{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Warnings for a module
module GHC.Unit.Module.Warnings
   ( Warnings (..)
   , WarningTxt (..)
   , pprWarningTxtForMsg
   , mkIfaceWarnCache
   , emptyIfaceWarnCache
   , plusWarns
   )
where

import GHC.Prelude

import GHC.Types.SourceText
import GHC.Types.Name.Occurrence
import GHC.Types.SrcLoc
import GHC.Hs.Doc
import GHC.Hs.Extension

import GHC.Utils.Outputable
import GHC.Utils.Binary

import Language.Haskell.Syntax.Extension

import Data.Data

-- | Warning Text
--
-- reason/explanation from a WARNING or DEPRECATED pragma
data WarningTxt pass
   = WarningTxt
      (Located SourceText)
      [Located (WithHsDocIdentifiers StringLiteral pass)]
   | DeprecatedTxt
      (Located SourceText)
      [Located (WithHsDocIdentifiers StringLiteral pass)]

deriving instance Eq (IdP pass) => Eq (WarningTxt pass)
deriving instance (Data pass, Data (IdP pass)) => Data (WarningTxt pass)

instance Outputable (WarningTxt pass) where
    ppr (WarningTxt    lsrc ws)
      = case unLoc lsrc of
          NoSourceText   -> pp_ws ws
          SourceText src -> text src <+> pp_ws ws <+> text "#-}"

    ppr (DeprecatedTxt lsrc  ds)
      = case unLoc lsrc of
          NoSourceText   -> pp_ws ds
          SourceText src -> text src <+> pp_ws ds <+> text "#-}"

instance Binary (WarningTxt GhcRn) where
    put_ bh (WarningTxt s w) = do
            putByte bh 0
            put_ bh s
            put_ bh w
    put_ bh (DeprecatedTxt s d) = do
            putByte bh 1
            put_ bh s
            put_ bh d

    get bh = do
            h <- getByte bh
            case h of
              0 -> do s <- get bh
                      w <- get bh
                      return (WarningTxt s w)
              _ -> do s <- get bh
                      d <- get bh
                      return (DeprecatedTxt s d)


pp_ws :: [Located (WithHsDocIdentifiers StringLiteral pass)] -> SDoc
pp_ws [l] = ppr $ unLoc l
pp_ws ws
  = text "["
    <+> vcat (punctuate comma (map (ppr . unLoc) ws))
    <+> text "]"


pprWarningTxtForMsg :: WarningTxt p -> SDoc
pprWarningTxtForMsg (WarningTxt    _ ws)
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

