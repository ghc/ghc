{-# LANGUAGE RecordWildCards #-}
module Haddock.Interface.Json (
    jsonInstalledInterface
  , jsonInterfaceFile
  , renderJson
  ) where

import BasicTypes
import Json
import Module
import Name
import Outputable

import Control.Arrow
import Data.Map (Map)
import Data.Bifunctor
import qualified Data.Map as Map

import Haddock.Types
import Haddock.InterfaceFile

jsonInterfaceFile :: InterfaceFile -> JsonDoc
jsonInterfaceFile InterfaceFile{..} =
  jsonObject [ ("link_env" , jsonMap nameStableString (jsonString . moduleNameString . moduleName) ifLinkEnv)
             , ("inst_ifaces", jsonArray (map jsonInstalledInterface ifInstalledIfaces))
             ]

jsonInstalledInterface :: InstalledInterface -> JsonDoc
jsonInstalledInterface InstalledInterface{..} = jsonObject properties
  where
    properties =
      [ ("module"          , jsonModule instMod)
      , ("is_sig"          , jsonBool instIsSig)
      , ("info"            , jsonHaddockModInfo instInfo)
      , ("doc_map"         , jsonMap nameStableString jsonMDoc instDocMap)
      , ("arg_map"         , jsonMap nameStableString (jsonMap show jsonMDoc) instArgMap)
      , ("exports"         , jsonArray (map jsonName instExports))
      , ("visible_exports" , jsonArray (map jsonName instVisibleExports))
      , ("options"         , jsonArray (map (jsonString . show) instOptions))
      , ("fix_map"         , jsonMap nameStableString jsonFixity instFixMap)
      ]

jsonHaddockModInfo :: HaddockModInfo Name -> JsonDoc
jsonHaddockModInfo HaddockModInfo{..} =
  jsonObject [ ("description" , jsonMaybe jsonDoc hmi_description)
             , ("copyright"   , jsonMaybe jsonString hmi_copyright)
             , ("maintainer"  , jsonMaybe jsonString hmi_maintainer)
             , ("stability"   , jsonMaybe jsonString hmi_stability)
             , ("protability" , jsonMaybe jsonString hmi_portability)
             , ("safety"      , jsonMaybe jsonString hmi_safety)
             , ("language"    , jsonMaybe (jsonString . show) hmi_language)
             , ("extensions"  , jsonArray (map (jsonString . show) hmi_extensions))
             ]

jsonMap :: (a -> String) -> (b -> JsonDoc) -> Map a b -> JsonDoc
jsonMap f g = jsonObject . map (f *** g) . Map.toList

jsonMDoc :: MDoc Name -> JsonDoc
jsonMDoc MetaDoc{..} =
  jsonObject [ ("meta", jsonObject [("version", jsonMaybe (jsonString . show) (_version _meta))])
             , ("doc",  jsonDoc _doc)
             ]

jsonDoc :: Doc Name -> JsonDoc
jsonDoc doc = jsonString (show (bimap (moduleNameString . fst) nameStableString doc))

jsonModule :: Module -> JsonDoc
jsonModule = JSString . moduleStableString

jsonName :: Name -> JsonDoc
jsonName = JSString . nameStableString

jsonFixity :: Fixity -> JsonDoc
jsonFixity (Fixity _ prec dir) =
  jsonObject [ ("prec"      , jsonInt prec)
             , ("direction" , jsonFixityDirection dir)
             ]

jsonFixityDirection :: FixityDirection -> JsonDoc
jsonFixityDirection InfixL = jsonString "infixl"
jsonFixityDirection InfixR = jsonString "infixr"
jsonFixityDirection InfixN = jsonString "infix"

renderJson :: JsonDoc -> SDoc
renderJson = renderJSON

jsonMaybe :: (a -> JsonDoc) -> Maybe a -> JsonDoc
jsonMaybe = maybe jsonNull

jsonString :: String -> JsonDoc
jsonString = JSString

jsonObject :: [(String, JsonDoc)] -> JsonDoc
jsonObject = JSObject

jsonArray :: [JsonDoc] -> JsonDoc
jsonArray = JSArray

jsonNull :: JsonDoc
jsonNull = JSNull

jsonInt :: Int -> JsonDoc
jsonInt = JSInt

jsonBool :: Bool -> JsonDoc
jsonBool = JSBool
