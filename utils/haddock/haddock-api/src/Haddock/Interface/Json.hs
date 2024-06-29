{-# LANGUAGE RecordWildCards #-}

module Haddock.Interface.Json
  ( jsonInstalledInterface
  , jsonInterfaceFile
  , renderJson
  ) where

import Control.Arrow
import Data.Map (Map)
import qualified Data.Map as Map
import GHC.Types.Fixity
import GHC.Types.Name
import GHC.Unit.Module
import GHC.Utils.Json
import GHC.Utils.Outputable

import Haddock.InterfaceFile
import Haddock.Types

jsonInterfaceFile :: InterfaceFile -> JsonDoc
jsonInterfaceFile InterfaceFile{..} =
  jsonObject
    [ ("package_info", jsonPackageInfo ifPackageInfo)
    , ("link_env", jsonMap nameStableString (jsonString . moduleNameString . moduleName) ifLinkEnv)
    , ("inst_ifaces", jsonArray (map jsonInstalledInterface ifInstalledIfaces))
    ]

jsonInstalledInterface :: InstalledInterface -> JsonDoc
jsonInstalledInterface InstalledInterface{..} = jsonObject properties
  where
    properties =
      [ ("module", jsonModule instMod)
      , ("is_sig", jsonBool instIsSig)
      , ("info", jsonHaddockModInfo instInfo)
      , ("doc_map", jsonMap nameStableString jsonMDoc instDocMap)
      , ("arg_map", jsonMap nameStableString (jsonMap show jsonMDoc) instArgMap)
      , ("exports", jsonArray (map jsonName instExports))
      , ("visible_exports", jsonArray (map jsonName instVisibleExports))
      , ("options", jsonArray (map (jsonString . show) instOptions))
      , ("fix_map", jsonMap nameStableString jsonFixity instFixMap)
      ]

jsonHaddockModInfo :: HaddockModInfo Name -> JsonDoc
jsonHaddockModInfo HaddockModInfo{..} =
  jsonObject
    [ ("description", jsonMaybe jsonDoc hmi_description)
    , ("copyright", jsonMaybe jsonString hmi_copyright)
    , ("maintainer", jsonMaybe jsonString hmi_maintainer)
    , ("stability", jsonMaybe jsonString hmi_stability)
    , ("protability", jsonMaybe jsonString hmi_portability)
    , ("safety", jsonMaybe jsonString hmi_safety)
    , ("language", jsonMaybe (jsonString . show) hmi_language)
    , ("extensions", jsonArray (map (jsonString . show) hmi_extensions))
    ]

jsonPackageInfo :: PackageInfo -> JsonDoc
jsonPackageInfo = jsonString . ppPackageInfo

jsonMap :: (a -> String) -> (b -> JsonDoc) -> Map a b -> JsonDoc
jsonMap f g = jsonObject . map (f *** g) . Map.toList

jsonMDoc :: MDoc Name -> JsonDoc
jsonMDoc MetaDoc{..} =
  jsonObject
    [ ("meta", jsonObject [("version", jsonMaybe (jsonString . show . sinceVersion) (_metaSince _meta))])
    , ("document", jsonDoc _doc)
    ]

showModName :: Wrap (ModuleName, OccName) -> String
showModName = showWrapped (moduleNameString . fst)

showName :: Wrap Name -> String
showName = showWrapped nameStableString

jsonDoc :: Doc Name -> JsonDoc
jsonDoc DocEmpty =
  jsonObject
    [("tag", jsonString "DocEmpty")]
jsonDoc (DocAppend x y) =
  jsonObject
    [ ("tag", jsonString "DocAppend")
    , ("first", jsonDoc x)
    , ("second", jsonDoc y)
    ]
jsonDoc (DocString s) =
  jsonObject
    [ ("tag", jsonString "DocString")
    , ("string", jsonString s)
    ]
jsonDoc (DocParagraph x) =
  jsonObject
    [ ("tag", jsonString "DocParagraph")
    , ("document", jsonDoc x)
    ]
jsonDoc (DocIdentifier name) =
  jsonObject
    [ ("tag", jsonString "DocIdentifier")
    , ("name", jsonString (showName name))
    ]
jsonDoc (DocIdentifierUnchecked modName) =
  jsonObject
    [ ("tag", jsonString "DocIdentifierUnchecked")
    , ("modName", jsonString (showModName modName))
    ]
jsonDoc (DocModule (ModLink m _l)) =
  jsonObject
    [ ("tag", jsonString "DocModule")
    , ("string", jsonString m)
    ]
jsonDoc (DocWarning x) =
  jsonObject
    [ ("tag", jsonString "DocWarning")
    , ("document", jsonDoc x)
    ]
jsonDoc (DocEmphasis x) =
  jsonObject
    [ ("tag", jsonString "DocEmphasis")
    , ("document", jsonDoc x)
    ]
jsonDoc (DocMonospaced x) =
  jsonObject
    [ ("tag", jsonString "DocMonospaced")
    , ("document", jsonDoc x)
    ]
jsonDoc (DocBold x) =
  jsonObject
    [ ("tag", jsonString "DocBold")
    , ("document", jsonDoc x)
    ]
jsonDoc (DocUnorderedList xs) =
  jsonObject
    [ ("tag", jsonString "DocUnorderedList")
    , ("documents", jsonArray (fmap jsonDoc xs))
    ]
jsonDoc (DocOrderedList xs) =
  jsonObject
    [ ("tag", jsonString "DocOrderedList")
    , ("items", jsonArray (fmap jsonItem xs))
    ]
  where
    jsonItem (index, a) = jsonObject [("document", jsonDoc a), ("seq", jsonInt index)]
jsonDoc (DocDefList xys) =
  jsonObject
    [ ("tag", jsonString "DocDefList")
    , ("definitions", jsonArray (fmap jsonDef xys))
    ]
  where
    jsonDef (x, y) = jsonObject [("document", jsonDoc x), ("y", jsonDoc y)]
jsonDoc (DocCodeBlock x) =
  jsonObject
    [ ("tag", jsonString "DocCodeBlock")
    , ("document", jsonDoc x)
    ]
jsonDoc (DocHyperlink hyperlink) =
  jsonObject
    [ ("tag", jsonString "DocHyperlink")
    , ("hyperlink", jsonHyperlink hyperlink)
    ]
  where
    jsonHyperlink Hyperlink{..} =
      jsonObject
        [ ("hyperlinkUrl", jsonString hyperlinkUrl)
        , ("hyperlinkLabel", jsonMaybe jsonDoc hyperlinkLabel)
        ]
jsonDoc (DocPic picture) =
  jsonObject
    [ ("tag", jsonString "DocPic")
    , ("picture", jsonPicture picture)
    ]
  where
    jsonPicture Picture{..} =
      jsonObject
        [ ("pictureUrl", jsonString pictureUri)
        , ("pictureLabel", jsonMaybe jsonString pictureTitle)
        ]
jsonDoc (DocMathInline s) =
  jsonObject
    [ ("tag", jsonString "DocMathInline")
    , ("string", jsonString s)
    ]
jsonDoc (DocMathDisplay s) =
  jsonObject
    [ ("tag", jsonString "DocMathDisplay")
    , ("string", jsonString s)
    ]
jsonDoc (DocAName s) =
  jsonObject
    [ ("tag", jsonString "DocAName")
    , ("string", jsonString s)
    ]
jsonDoc (DocProperty s) =
  jsonObject
    [ ("tag", jsonString "DocProperty")
    , ("string", jsonString s)
    ]
jsonDoc (DocExamples examples) =
  jsonObject
    [ ("tag", jsonString "DocExamples")
    , ("examples", jsonArray (fmap jsonExample examples))
    ]
  where
    jsonExample Example{..} =
      jsonObject
        [ ("exampleExpression", jsonString exampleExpression)
        , ("exampleResult", jsonArray (fmap jsonString exampleResult))
        ]
jsonDoc (DocHeader header) =
  jsonObject
    [ ("tag", jsonString "DocHeader")
    , ("header", jsonHeader header)
    ]
  where
    jsonHeader Header{..} =
      jsonObject
        [ ("headerLevel", jsonInt headerLevel)
        , ("headerTitle", jsonDoc headerTitle)
        ]
jsonDoc (DocTable table) =
  jsonObject
    [ ("tag", jsonString "DocTable")
    , ("table", jsonTable table)
    ]
  where
    jsonTable Table{..} =
      jsonObject
        [ ("tableHeaderRows", jsonArray (fmap jsonTableRow tableHeaderRows))
        , ("tableBodyRows", jsonArray (fmap jsonTableRow tableBodyRows))
        ]

    jsonTableRow TableRow{..} = jsonArray (fmap jsonTableCell tableRowCells)

    jsonTableCell TableCell{..} =
      jsonObject
        [ ("tableCellColspan", jsonInt tableCellColspan)
        , ("tableCellRowspan", jsonInt tableCellRowspan)
        , ("tableCellContents", jsonDoc tableCellContents)
        ]

jsonModule :: Module -> JsonDoc
jsonModule = JSString . moduleStableString

jsonName :: Name -> JsonDoc
jsonName = JSString . nameStableString

jsonFixity :: Fixity -> JsonDoc
jsonFixity (Fixity prec dir) =
  jsonObject
    [ ("prec", jsonInt prec)
    , ("direction", jsonFixityDirection dir)
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
