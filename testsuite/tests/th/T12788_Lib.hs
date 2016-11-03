module T12788_Lib where

import Language.Haskell.TH

data Options = Options
    { fieldLabelModifier :: String -> String
    , constructorTagModifier :: String -> String
    , allNullaryToStringTag :: Bool
    , omitNothingFields :: Bool
    , sumEncoding :: SumEncoding
    , unwrapUnaryRecords :: Bool
    }

data SumEncoding =
    TaggedObject { tagFieldName      :: String
                 , contentsFieldName :: String
                 }
  | ObjectWithSingleField
  | TwoElemArray

deriveJSON :: Options -> Name -> Q [Dec]
deriveJSON _ _ = return []
