-- | Datatype definitions for the flag representation stored in interface files
module GHC.Iface.Flags (
     IfaceDynFlags(..)
   , IfaceGeneralFlag(..)
   , IfaceProfAuto(..)
   , IfaceExtension(..)
   , IfaceLanguage(..)
   , IfaceCppOptions(..)
   , pprIfaceDynFlags
   , missingExtraFlagInfo
   ) where

import GHC.Prelude

import GHC.Utils.Outputable
import Control.DeepSeq
import GHC.Utils.Fingerprint
import GHC.Utils.Binary

import GHC.Driver.DynFlags
import GHC.Types.SafeHaskell
import GHC.Core.Opt.CallerCC.Types

import qualified GHC.LanguageExtensions as LangExt

-- The part of DynFlags which recompilation information needs
data IfaceDynFlags = IfaceDynFlags
        { ifaceMainIs :: Maybe (Maybe String)
        , ifaceSafeMode :: IfaceTrustInfo
        , ifaceLang :: Maybe IfaceLanguage
        , ifaceExts :: [IfaceExtension]
        , ifaceCppOptions :: IfaceCppOptions
        , ifaceJsOptions  :: IfaceCppOptions
        , ifaceCmmOptions :: IfaceCppOptions
        , ifacePaths :: [String]
        , ifaceProf  :: Maybe IfaceProfAuto
        , ifaceTicky :: [IfaceGeneralFlag]
        , ifaceCodeGen :: [IfaceGeneralFlag]
        , ifaceFatIface :: Bool
        , ifaceDebugLevel :: Int
        , ifaceCallerCCFilters :: [CallerCcFilter]
        }

pprIfaceDynFlags :: (Fingerprint, Maybe IfaceDynFlags) -> SDoc
pprIfaceDynFlags (f, mflags) =
  vcat $
    [ text "fingerprint:" <+> (ppr f)
    ]
    ++ case mflags of
        Nothing -> [missingExtraFlagInfo]
        Just (IfaceDynFlags a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14) ->
          [ text "main-is:" <+> (ppr $ fmap (fmap (text @SDoc)) a1)
          , text "safe-mode:" <+> ppr a2
          , text "lang:" <+> ppr a3
          , text "exts:" <+> ppr a4
          , text "cpp-options:"
          , nest 2 $ ppr a5
          , text "js-options:"
          , nest 2 $ ppr a6
          , text "cmm-options:"
          , nest 2 $ ppr a7
          , text "paths:" <+> hcat (map text a8)
          , text "prof:"  <+> ppr a9
          , text "ticky:"
          , nest 2 $ vcat (map ppr a10)
          , text "codegen:"
          , nest 2 $ vcat (map ppr a11)
          , text "fat-iface:" <+> ppr a12
          , text "debug-level:" <+> ppr a13
          , text "caller-cc-filters:" <+> ppr a14
          ]

missingExtraFlagInfo :: SDoc
missingExtraFlagInfo = text "flags: no detailed info, recompile with -fwrite-if-self-recomp-flags"
  where
    -- If you modify the name of this flag, you have to modify this string.
    _placeholder = Opt_WriteSelfRecompFlags

instance Binary IfaceDynFlags where
  put_ bh (IfaceDynFlags a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14) = do
    put_ bh a1
    put_ bh a2
    put_ bh a3
    put_ bh a4
    put_ bh a5
    put_ bh a6
    put_ bh a7
    put_ bh a8
    put_ bh a9
    put_ bh a10
    put_ bh a11
    put_ bh a12
    put_ bh a13
    put_ bh a14
  get bh = IfaceDynFlags <$> get bh
                         <*> get bh
                         <*> get bh
                         <*> get bh
                         <*> get bh
                         <*> get bh
                         <*> get bh
                         <*> get bh
                         <*> get bh
                         <*> get bh
                         <*> get bh
                         <*> get bh
                         <*> get bh
                         <*> get bh

instance NFData IfaceDynFlags where
  rnf (IfaceDynFlags a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14) =
          rnf a1
    `seq` rnf a2
    `seq` rnf a3
    `seq` rnf a4
    `seq` rnf a5
    `seq` rnf a6
    `seq` rnf a7
    `seq` rnf a8
    `seq` rnf a9
    `seq` rnf a10
    `seq` rnf a11
    `seq` rnf a12
    `seq` rnf a13
    `seq` rnf a14

newtype IfaceGeneralFlag = IfaceGeneralFlag GeneralFlag

instance NFData IfaceGeneralFlag where
  rnf (IfaceGeneralFlag !_) = ()

instance Binary IfaceGeneralFlag where
  put_ bh (IfaceGeneralFlag f) = put_ bh (fromEnum f)
  get bh = IfaceGeneralFlag . toEnum <$> get bh

instance Outputable IfaceGeneralFlag where
  ppr (IfaceGeneralFlag f) = text (show f)

newtype IfaceProfAuto = IfaceProfAuto ProfAuto

instance NFData IfaceProfAuto where
  rnf (IfaceProfAuto !_) = ()

instance Binary IfaceProfAuto where
  put_ bh (IfaceProfAuto f) = put_ bh (fromEnum f)
  get bh = IfaceProfAuto . toEnum <$> get bh

instance Outputable IfaceProfAuto where
  ppr (IfaceProfAuto f) = text (show f)


newtype IfaceExtension = IfaceExtension LangExt.Extension

instance NFData IfaceExtension where
  rnf (IfaceExtension !_) = ()

instance Binary IfaceExtension where
  put_ bh (IfaceExtension f) = put_ bh (fromEnum f)
  get bh = IfaceExtension . toEnum <$> get bh

instance Outputable IfaceExtension where
  ppr (IfaceExtension f) = text (show f)

newtype IfaceLanguage = IfaceLanguage Language

instance NFData IfaceLanguage where
  rnf (IfaceLanguage !_) = ()

instance Binary IfaceLanguage where
  put_ bh (IfaceLanguage f) = put_ bh (fromEnum f)
  get bh = IfaceLanguage . toEnum <$> get bh

instance Outputable IfaceLanguage where
  ppr (IfaceLanguage f) = text (show f)

data IfaceCppOptions = IfaceCppOptions { ifaceCppIncludes :: [FilePath]
                                       , ifaceCppOpts :: [String]
                                       , ifaceCppSig :: ([String], Fingerprint)
                                       }

instance NFData IfaceCppOptions where
  rnf (IfaceCppOptions is os s) = rnf is `seq` rnf os `seq` rnf s

instance Binary IfaceCppOptions where
  put_ bh (IfaceCppOptions is os s) = do
     put_ bh is
     put_ bh os
     put_ bh s
  get bh = IfaceCppOptions <$> get bh <*> get bh <*> get bh

instance Outputable IfaceCppOptions where
  ppr (IfaceCppOptions is os (wos, fp)) =
        vcat [text "includes:"
             , nest 2 $ hcat (map text is)
             , text "opts:"
             , nest 2 $ hcat (map text os)
             , text "signature:"
             , nest 2 $ parens (ppr fp) <+> ppr (map (text @SDoc) wos)

             ]
