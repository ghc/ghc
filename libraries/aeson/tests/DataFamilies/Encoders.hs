{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module DataFamilies.Encoders (module DataFamilies.Encoders) where

import Prelude.Compat

import Data.Aeson.TH
import Data.Aeson.Types
import DataFamilies.Types
import Options

--------------------------------------------------------------------------------
-- Nullary encoders/decoders
--------------------------------------------------------------------------------

thNullaryToJSONString :: Nullary Int -> Value
thNullaryToJSONString = $(mkToJSON defaultOptions 'C1)

thNullaryToEncodingString :: Nullary Int -> Encoding
thNullaryToEncodingString = $(mkToEncoding defaultOptions 'C2)

thNullaryParseJSONString :: Value -> Parser (Nullary Int)
thNullaryParseJSONString = $(mkParseJSON defaultOptions 'C3)


thNullaryToJSON2ElemArray :: Nullary Int -> Value
thNullaryToJSON2ElemArray = $(mkToJSON opts2ElemArray 'C1)

thNullaryToEncoding2ElemArray :: Nullary Int -> Encoding
thNullaryToEncoding2ElemArray = $(mkToEncoding opts2ElemArray 'C2)

thNullaryParseJSON2ElemArray :: Value -> Parser (Nullary Int)
thNullaryParseJSON2ElemArray = $(mkParseJSON opts2ElemArray 'C3)


thNullaryToJSONTaggedObject :: Nullary Int -> Value
thNullaryToJSONTaggedObject = $(mkToJSON optsTaggedObject 'C1)

thNullaryToEncodingTaggedObject :: Nullary Int -> Encoding
thNullaryToEncodingTaggedObject = $(mkToEncoding optsTaggedObject 'C2)

thNullaryParseJSONTaggedObject :: Value -> Parser (Nullary Int)
thNullaryParseJSONTaggedObject = $(mkParseJSON optsTaggedObject 'C3)


thNullaryToJSONObjectWithSingleField :: Nullary Int -> Value
thNullaryToJSONObjectWithSingleField =
  $(mkToJSON optsObjectWithSingleField 'C1)

thNullaryToEncodingObjectWithSingleField :: Nullary Int -> Encoding
thNullaryToEncodingObjectWithSingleField =
  $(mkToEncoding optsObjectWithSingleField 'C2)

thNullaryParseJSONObjectWithSingleField :: Value -> Parser (Nullary Int)
thNullaryParseJSONObjectWithSingleField = $(mkParseJSON optsObjectWithSingleField 'C3)


--------------------------------------------------------------------------------
-- SomeType encoders/decoders
--------------------------------------------------------------------------------

thSomeTypeToJSON2ElemArray :: SomeType c () Int -> Value
thSomeTypeToJSON2ElemArray = $(mkToJSON opts2ElemArray 'Nullary)

thSomeTypeToEncoding2ElemArray :: SomeType c () Int -> Encoding
thSomeTypeToEncoding2ElemArray = $(mkToEncoding opts2ElemArray 'Unary)

thSomeTypeParseJSON2ElemArray :: Value -> Parser (SomeType c () Int)
thSomeTypeParseJSON2ElemArray = $(mkParseJSON opts2ElemArray 'Product)


thSomeTypeToJSONTaggedObject :: SomeType c () Int -> Value
thSomeTypeToJSONTaggedObject = $(mkToJSON optsTaggedObject 'Record)

thSomeTypeToEncodingTaggedObject :: SomeType c () Int -> Encoding
thSomeTypeToEncodingTaggedObject = $(mkToEncoding optsTaggedObject 'Nullary)

thSomeTypeParseJSONTaggedObject :: Value -> Parser (SomeType c () Int)
thSomeTypeParseJSONTaggedObject = $(mkParseJSON optsTaggedObject 'Unary)


thSomeTypeToJSONObjectWithSingleField :: SomeType c () Int -> Value
thSomeTypeToJSONObjectWithSingleField =
  $(mkToJSON optsObjectWithSingleField 'Product)

thSomeTypeToEncodingObjectWithSingleField :: SomeType c () Int -> Encoding
thSomeTypeToEncodingObjectWithSingleField =
  $(mkToEncoding optsObjectWithSingleField 'Record)

thSomeTypeParseJSONObjectWithSingleField :: Value -> Parser (SomeType c () Int)
thSomeTypeParseJSONObjectWithSingleField =
  $(mkParseJSON optsObjectWithSingleField 'Nullary)


--------------------------------------------------------------------------------
-- Approx encoders/decoders
--------------------------------------------------------------------------------

thApproxToJSONUnwrap :: Approx String -> Value
thApproxToJSONUnwrap = $(mkToJSON optsUnwrapUnaryRecords 'Approx)

thApproxToEncodingUnwrap :: Approx String -> Encoding
thApproxToEncodingUnwrap = $(mkToEncoding optsUnwrapUnaryRecords 'Approx)

thApproxParseJSONUnwrap :: Value -> Parser (Approx String)
thApproxParseJSONUnwrap = $(mkParseJSON optsUnwrapUnaryRecords 'Approx)


thApproxToJSONDefault :: Approx String -> Value
thApproxToJSONDefault = $(mkToJSON defaultOptions 'Approx)

thApproxToEncodingDefault :: Approx String -> Encoding
thApproxToEncodingDefault = $(mkToEncoding defaultOptions 'Approx)

thApproxParseJSONDefault :: Value -> Parser (Approx String)
thApproxParseJSONDefault = $(mkParseJSON defaultOptions 'Approx)

--------------------------------------------------------------------------------
-- GADT encoders/decoders
--------------------------------------------------------------------------------

thGADTToJSONUnwrap :: GADT String -> Value
thGADTToJSONUnwrap = $(mkToJSON optsUnwrapUnaryRecords 'GADT)

thGADTToEncodingUnwrap :: GADT String -> Encoding
thGADTToEncodingUnwrap = $(mkToEncoding optsUnwrapUnaryRecords 'GADT)

thGADTParseJSONUnwrap :: Value -> Parser (GADT String)
thGADTParseJSONUnwrap = $(mkParseJSON optsUnwrapUnaryRecords 'GADT)


thGADTToJSONDefault :: GADT String -> Value
thGADTToJSONDefault = $(mkToJSON defaultOptions 'GADT)

thGADTToEncodingDefault :: GADT String -> Encoding
thGADTToEncodingDefault = $(mkToEncoding defaultOptions 'GADT)

thGADTParseJSONDefault :: Value -> Parser (GADT String)
thGADTParseJSONDefault = $(mkParseJSON defaultOptions 'GADT)

--------------------------------------------------------------------------------
-- Generic encoders/decoders
--------------------------------------------------------------------------------

-- Nullary

gNullaryToJSONString :: Nullary Int -> Value
gNullaryToJSONString = genericToJSON defaultOptions

gNullaryToEncodingString :: Nullary Int -> Encoding
gNullaryToEncodingString = genericToEncoding defaultOptions

gNullaryParseJSONString :: Value -> Parser (Nullary Int)
gNullaryParseJSONString = genericParseJSON defaultOptions


gNullaryToJSON2ElemArray :: Nullary Int -> Value
gNullaryToJSON2ElemArray = genericToJSON opts2ElemArray

gNullaryToEncoding2ElemArray :: Nullary Int -> Encoding
gNullaryToEncoding2ElemArray = genericToEncoding opts2ElemArray

gNullaryParseJSON2ElemArray :: Value -> Parser (Nullary Int)
gNullaryParseJSON2ElemArray = genericParseJSON opts2ElemArray


gNullaryToJSONTaggedObject :: Nullary Int -> Value
gNullaryToJSONTaggedObject = genericToJSON optsTaggedObject

gNullaryToEncodingTaggedObject :: Nullary Int -> Encoding
gNullaryToEncodingTaggedObject = genericToEncoding optsTaggedObject

gNullaryParseJSONTaggedObject :: Value -> Parser (Nullary Int)
gNullaryParseJSONTaggedObject = genericParseJSON optsTaggedObject


gNullaryToJSONObjectWithSingleField :: Nullary Int -> Value
gNullaryToJSONObjectWithSingleField = genericToJSON optsObjectWithSingleField

gNullaryToEncodingObjectWithSingleField :: Nullary Int -> Encoding
gNullaryToEncodingObjectWithSingleField = genericToEncoding optsObjectWithSingleField

gNullaryParseJSONObjectWithSingleField :: Value -> Parser (Nullary Int)
gNullaryParseJSONObjectWithSingleField = genericParseJSON optsObjectWithSingleField

-- SomeType

gSomeTypeToJSON2ElemArray :: SomeType c () Int -> Value
gSomeTypeToJSON2ElemArray = genericToJSON opts2ElemArray

gSomeTypeToEncoding2ElemArray :: SomeType c () Int -> Encoding
gSomeTypeToEncoding2ElemArray = genericToEncoding opts2ElemArray

gSomeTypeParseJSON2ElemArray :: Value -> Parser (SomeType c () Int)
gSomeTypeParseJSON2ElemArray = genericParseJSON opts2ElemArray


gSomeTypeToJSONTaggedObject :: SomeType c () Int -> Value
gSomeTypeToJSONTaggedObject = genericToJSON optsTaggedObject

gSomeTypeToEncodingTaggedObject :: SomeType c () Int -> Encoding
gSomeTypeToEncodingTaggedObject = genericToEncoding optsTaggedObject

gSomeTypeParseJSONTaggedObject :: Value -> Parser (SomeType c () Int)
gSomeTypeParseJSONTaggedObject = genericParseJSON optsTaggedObject


gSomeTypeToJSONObjectWithSingleField :: SomeType c () Int -> Value
gSomeTypeToJSONObjectWithSingleField = genericToJSON optsObjectWithSingleField

gSomeTypeToEncodingObjectWithSingleField :: SomeType c () Int -> Encoding
gSomeTypeToEncodingObjectWithSingleField = genericToEncoding optsObjectWithSingleField

gSomeTypeParseJSONObjectWithSingleField :: Value -> Parser (SomeType c () Int)
gSomeTypeParseJSONObjectWithSingleField = genericParseJSON optsObjectWithSingleField

-- Approx

gApproxToJSONUnwrap :: Approx String -> Value
gApproxToJSONUnwrap = genericToJSON optsUnwrapUnaryRecords

gApproxToEncodingUnwrap :: Approx String -> Encoding
gApproxToEncodingUnwrap = genericToEncoding optsUnwrapUnaryRecords

gApproxParseJSONUnwrap :: Value -> Parser (Approx String)
gApproxParseJSONUnwrap = genericParseJSON optsUnwrapUnaryRecords


gApproxToJSONDefault :: Approx String -> Value
gApproxToJSONDefault = genericToJSON defaultOptions

gApproxToEncodingDefault :: Approx String -> Encoding
gApproxToEncodingDefault = genericToEncoding defaultOptions

gApproxParseJSONDefault :: Value -> Parser (Approx String)
gApproxParseJSONDefault = genericParseJSON defaultOptions
