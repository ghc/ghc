{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Encoders (module Encoders) where

import Prelude.Compat
import Data.Text (Text)

import Data.Aeson.TH
import Data.Aeson.Types
import Options
import Types

--------------------------------------------------------------------------------
-- Nullary encoders/decoders
--------------------------------------------------------------------------------

thNullaryToJSONString :: Nullary -> Value
thNullaryToJSONString = $(mkToJSON defaultOptions ''Nullary)

thNullaryToEncodingString :: Nullary -> Encoding
thNullaryToEncodingString = $(mkToEncoding defaultOptions ''Nullary)

thNullaryParseJSONString :: Value -> Parser Nullary
thNullaryParseJSONString = $(mkParseJSON defaultOptions ''Nullary)


thNullaryToJSON2ElemArray :: Nullary -> Value
thNullaryToJSON2ElemArray = $(mkToJSON opts2ElemArray ''Nullary)

thNullaryToEncoding2ElemArray :: Nullary -> Encoding
thNullaryToEncoding2ElemArray = $(mkToEncoding opts2ElemArray ''Nullary)

thNullaryParseJSON2ElemArray :: Value -> Parser Nullary
thNullaryParseJSON2ElemArray = $(mkParseJSON opts2ElemArray ''Nullary)


thNullaryToJSONTaggedObject :: Nullary -> Value
thNullaryToJSONTaggedObject = $(mkToJSON optsTaggedObject ''Nullary)

thNullaryToEncodingTaggedObject :: Nullary -> Encoding
thNullaryToEncodingTaggedObject = $(mkToEncoding optsTaggedObject ''Nullary)

thNullaryParseJSONTaggedObject :: Value -> Parser Nullary
thNullaryParseJSONTaggedObject = $(mkParseJSON optsTaggedObject ''Nullary)


thNullaryToJSONObjectWithSingleField :: Nullary -> Value
thNullaryToJSONObjectWithSingleField =
  $(mkToJSON optsObjectWithSingleField ''Nullary)

thNullaryToEncodingObjectWithSingleField :: Nullary -> Encoding
thNullaryToEncodingObjectWithSingleField =
  $(mkToEncoding optsObjectWithSingleField ''Nullary)

thNullaryParseJSONObjectWithSingleField :: Value -> Parser Nullary
thNullaryParseJSONObjectWithSingleField = $(mkParseJSON optsObjectWithSingleField ''Nullary)

gNullaryToJSONString :: Nullary -> Value
gNullaryToJSONString = genericToJSON defaultOptions

gNullaryToEncodingString :: Nullary -> Encoding
gNullaryToEncodingString = genericToEncoding defaultOptions

gNullaryParseJSONString :: Value -> Parser Nullary
gNullaryParseJSONString = genericParseJSON defaultOptions


gNullaryToJSON2ElemArray :: Nullary -> Value
gNullaryToJSON2ElemArray = genericToJSON opts2ElemArray

gNullaryToEncoding2ElemArray :: Nullary -> Encoding
gNullaryToEncoding2ElemArray = genericToEncoding opts2ElemArray

gNullaryParseJSON2ElemArray :: Value -> Parser Nullary
gNullaryParseJSON2ElemArray = genericParseJSON opts2ElemArray


gNullaryToJSONTaggedObject :: Nullary -> Value
gNullaryToJSONTaggedObject = genericToJSON optsTaggedObject

gNullaryToEncodingTaggedObject :: Nullary -> Encoding
gNullaryToEncodingTaggedObject = genericToEncoding optsTaggedObject

gNullaryParseJSONTaggedObject :: Value -> Parser Nullary
gNullaryParseJSONTaggedObject = genericParseJSON optsTaggedObject


gNullaryToJSONObjectWithSingleField :: Nullary -> Value
gNullaryToJSONObjectWithSingleField = genericToJSON optsObjectWithSingleField

gNullaryToEncodingObjectWithSingleField :: Nullary -> Encoding
gNullaryToEncodingObjectWithSingleField = genericToEncoding optsObjectWithSingleField

gNullaryParseJSONObjectWithSingleField :: Value -> Parser Nullary
gNullaryParseJSONObjectWithSingleField = genericParseJSON optsObjectWithSingleField

keyOptions :: JSONKeyOptions
keyOptions = defaultJSONKeyOptions { keyModifier = ('k' :) }

gNullaryToJSONKey :: Nullary -> Either String Text
gNullaryToJSONKey x = case genericToJSONKey keyOptions of
  ToJSONKeyText p _ -> Right (p x)
  _ -> Left "Should be a ToJSONKeyText"

gNullaryFromJSONKey :: Text -> Parser Nullary
gNullaryFromJSONKey t = case genericFromJSONKey keyOptions of
  FromJSONKeyTextParser p -> p t
  _ -> fail "Not a TextParser"

--------------------------------------------------------------------------------
-- SomeType encoders/decoders
--------------------------------------------------------------------------------

-- Unary types
type LiftToJSON f a =
    (a -> Value) -> ([a] -> Value) -> f a -> Value
type LiftToEncoding f a =
    (a -> Encoding) -> ([a] -> Encoding) -> f a -> Encoding
type LiftParseJSON f a =
    (Value -> Parser a) -> (Value -> Parser [a]) -> Value -> Parser (f a)

thSomeTypeToJSON2ElemArray :: SomeType Int -> Value
thSomeTypeToJSON2ElemArray = $(mkToJSON opts2ElemArray ''SomeType)

thSomeTypeToEncoding2ElemArray :: SomeType Int -> Encoding
thSomeTypeToEncoding2ElemArray = $(mkToEncoding opts2ElemArray ''SomeType)

thSomeTypeLiftToJSON2ElemArray :: LiftToJSON SomeType a
thSomeTypeLiftToJSON2ElemArray = $(mkLiftToJSON opts2ElemArray ''SomeType)

thSomeTypeLiftToEncoding2ElemArray :: LiftToEncoding SomeType a
thSomeTypeLiftToEncoding2ElemArray = $(mkLiftToEncoding opts2ElemArray ''SomeType)

thSomeTypeParseJSON2ElemArray :: Value -> Parser (SomeType Int)
thSomeTypeParseJSON2ElemArray = $(mkParseJSON opts2ElemArray ''SomeType)

thSomeTypeLiftParseJSON2ElemArray :: LiftParseJSON SomeType a
thSomeTypeLiftParseJSON2ElemArray = $(mkLiftParseJSON opts2ElemArray ''SomeType)


thSomeTypeToJSONTaggedObject :: SomeType Int -> Value
thSomeTypeToJSONTaggedObject = $(mkToJSON optsTaggedObject ''SomeType)

thSomeTypeToEncodingTaggedObject :: SomeType Int -> Encoding
thSomeTypeToEncodingTaggedObject = $(mkToEncoding optsTaggedObject ''SomeType)

thSomeTypeLiftToJSONTaggedObject :: LiftToJSON SomeType a
thSomeTypeLiftToJSONTaggedObject = $(mkLiftToJSON optsTaggedObject ''SomeType)

thSomeTypeLiftToEncodingTaggedObject :: LiftToEncoding SomeType a
thSomeTypeLiftToEncodingTaggedObject = $(mkLiftToEncoding optsTaggedObject ''SomeType)

thSomeTypeParseJSONTaggedObject :: Value -> Parser (SomeType Int)
thSomeTypeParseJSONTaggedObject = $(mkParseJSON optsTaggedObject ''SomeType)

thSomeTypeLiftParseJSONTaggedObject :: LiftParseJSON SomeType a
thSomeTypeLiftParseJSONTaggedObject = $(mkLiftParseJSON optsTaggedObject ''SomeType)


thSomeTypeToJSONObjectWithSingleField :: SomeType Int -> Value
thSomeTypeToJSONObjectWithSingleField = $(mkToJSON optsObjectWithSingleField ''SomeType)

thSomeTypeToEncodingObjectWithSingleField :: SomeType Int -> Encoding
thSomeTypeToEncodingObjectWithSingleField = $(mkToEncoding optsObjectWithSingleField ''SomeType)

thSomeTypeLiftToJSONObjectWithSingleField :: LiftToJSON SomeType a
thSomeTypeLiftToJSONObjectWithSingleField = $(mkLiftToJSON optsObjectWithSingleField ''SomeType)

thSomeTypeLiftToEncodingObjectWithSingleField :: LiftToEncoding SomeType a
thSomeTypeLiftToEncodingObjectWithSingleField = $(mkLiftToEncoding optsObjectWithSingleField ''SomeType)

thSomeTypeParseJSONObjectWithSingleField :: Value -> Parser (SomeType Int)
thSomeTypeParseJSONObjectWithSingleField = $(mkParseJSON optsObjectWithSingleField ''SomeType)

thSomeTypeLiftParseJSONObjectWithSingleField :: LiftParseJSON SomeType a
thSomeTypeLiftParseJSONObjectWithSingleField = $(mkLiftParseJSON optsObjectWithSingleField ''SomeType)


gSomeTypeToJSON2ElemArray :: SomeType Int -> Value
gSomeTypeToJSON2ElemArray = genericToJSON opts2ElemArray

gSomeTypeToEncoding2ElemArray :: SomeType Int -> Encoding
gSomeTypeToEncoding2ElemArray = genericToEncoding opts2ElemArray

gSomeTypeParseJSON2ElemArray :: Value -> Parser (SomeType Int)
gSomeTypeParseJSON2ElemArray = genericParseJSON opts2ElemArray

gSomeTypeLiftToEncoding2ElemArray :: LiftToEncoding SomeType a
gSomeTypeLiftToEncoding2ElemArray = genericLiftToEncoding opts2ElemArray

gSomeTypeLiftToJSON2ElemArray :: LiftToJSON SomeType a
gSomeTypeLiftToJSON2ElemArray = genericLiftToJSON opts2ElemArray

gSomeTypeLiftParseJSON2ElemArray :: LiftParseJSON SomeType a
gSomeTypeLiftParseJSON2ElemArray = genericLiftParseJSON opts2ElemArray


gSomeTypeToJSONTaggedObject :: SomeType Int -> Value
gSomeTypeToJSONTaggedObject = genericToJSON optsTaggedObject

gSomeTypeToEncodingTaggedObject :: SomeType Int -> Encoding
gSomeTypeToEncodingTaggedObject = genericToEncoding optsTaggedObject

gSomeTypeParseJSONTaggedObject :: Value -> Parser (SomeType Int)
gSomeTypeParseJSONTaggedObject = genericParseJSON optsTaggedObject

gSomeTypeLiftToEncodingTaggedObject :: LiftToEncoding SomeType a
gSomeTypeLiftToEncodingTaggedObject = genericLiftToEncoding optsTaggedObject

gSomeTypeLiftToJSONTaggedObject :: LiftToJSON SomeType a
gSomeTypeLiftToJSONTaggedObject = genericLiftToJSON optsTaggedObject

gSomeTypeLiftParseJSONTaggedObject :: LiftParseJSON SomeType a
gSomeTypeLiftParseJSONTaggedObject = genericLiftParseJSON optsTaggedObject


gSomeTypeToJSONObjectWithSingleField :: SomeType Int -> Value
gSomeTypeToJSONObjectWithSingleField = genericToJSON optsObjectWithSingleField

gSomeTypeToEncodingObjectWithSingleField :: SomeType Int -> Encoding
gSomeTypeToEncodingObjectWithSingleField = genericToEncoding optsObjectWithSingleField

gSomeTypeParseJSONObjectWithSingleField :: Value -> Parser (SomeType Int)
gSomeTypeParseJSONObjectWithSingleField = genericParseJSON optsObjectWithSingleField

gSomeTypeLiftToEncodingObjectWithSingleField :: LiftToEncoding SomeType a
gSomeTypeLiftToEncodingObjectWithSingleField = genericLiftToEncoding optsObjectWithSingleField

gSomeTypeLiftToJSONObjectWithSingleField :: LiftToJSON SomeType a
gSomeTypeLiftToJSONObjectWithSingleField = genericLiftToJSON optsObjectWithSingleField

gSomeTypeLiftParseJSONObjectWithSingleField :: LiftParseJSON SomeType a
gSomeTypeLiftParseJSONObjectWithSingleField = genericLiftParseJSON optsObjectWithSingleField


gSomeTypeToJSONOmitNothingFields :: SomeType Int -> Value
gSomeTypeToJSONOmitNothingFields = genericToJSON optsOmitNothingFields

gSomeTypeToEncodingOmitNothingFields :: SomeType Int -> Encoding
gSomeTypeToEncodingOmitNothingFields = genericToEncoding optsOmitNothingFields


thSomeTypeParseJSONRejectUnknownFields :: Value -> Parser (SomeType Int)
thSomeTypeParseJSONRejectUnknownFields = $(mkParseJSON optsRejectUnknownFields ''SomeType)

gSomeTypeParseJSONRejectUnknownFields :: Value -> Parser (SomeType Int)
gSomeTypeParseJSONRejectUnknownFields = genericParseJSON optsRejectUnknownFields


--------------------------------------------------------------------------------
-- Foo decoders
--------------------------------------------------------------------------------

thFooParseJSONRejectUnknownFields :: Value -> Parser Foo
thFooParseJSONRejectUnknownFields = $(mkParseJSON optsRejectUnknownFields ''Foo)

gFooParseJSONRejectUnknownFields :: Value -> Parser Foo
gFooParseJSONRejectUnknownFields = genericParseJSON optsRejectUnknownFields


thFooParseJSONRejectUnknownFieldsTagged :: Value -> Parser Foo
thFooParseJSONRejectUnknownFieldsTagged = $(mkParseJSON optsRejectUnknownFieldsTagged ''Foo)

gFooParseJSONRejectUnknownFieldsTagged :: Value -> Parser Foo
gFooParseJSONRejectUnknownFieldsTagged = genericParseJSON optsRejectUnknownFieldsTagged


--------------------------------------------------------------------------------
-- Option fields
--------------------------------------------------------------------------------

thOptionFieldToJSON :: OptionField -> Value
thOptionFieldToJSON = $(mkToJSON optsOptionField 'OptionField)

thOptionFieldToEncoding :: OptionField -> Encoding
thOptionFieldToEncoding = $(mkToEncoding optsOptionField 'OptionField)

thOptionFieldParseJSON :: Value -> Parser OptionField
thOptionFieldParseJSON = $(mkParseJSON optsOptionField 'OptionField)

gOptionFieldToJSON :: OptionField -> Value
gOptionFieldToJSON = genericToJSON optsOptionField

gOptionFieldToEncoding :: OptionField -> Encoding
gOptionFieldToEncoding = genericToEncoding optsOptionField

gOptionFieldParseJSON :: Value -> Parser OptionField
gOptionFieldParseJSON = genericParseJSON optsOptionField

thMaybeFieldToJSON :: MaybeField -> Value
thMaybeFieldToJSON = $(mkToJSON optsOptionField 'MaybeField)


--------------------------------------------------------------------------------
-- IncoherentInstancesNeeded
--------------------------------------------------------------------------------

-- | This test demonstrates the need for IncoherentInstances. See the definition
-- of 'IncoherentInstancesNeeded' for a discussion of the issue.
--
-- NOTE 1: We only need to compile this test. We do not need to run it.
--
-- NOTE 2: We actually only use the INCOHERENT pragma on specific instances
-- instead of the IncoherentInstances language extension. Therefore, this is
-- only supported on GHC versions >= 7.10.
#if __GLASGOW_HASKELL__ >= 710
incoherentInstancesNeededParseJSONString :: FromJSON a => Value -> Parser (IncoherentInstancesNeeded a)
incoherentInstancesNeededParseJSONString = case () of
  _ | True  -> $(mkParseJSON defaultOptions ''IncoherentInstancesNeeded)
    | False -> genericParseJSON defaultOptions

incoherentInstancesNeededToJSON :: ToJSON a => IncoherentInstancesNeeded a -> Value
incoherentInstancesNeededToJSON = case () of
  _ | True  -> $(mkToJSON defaultOptions ''IncoherentInstancesNeeded)
    | False -> genericToJSON defaultOptions
#endif

-------------------------------------------------------------------------------
-- EitherTextInt encoders/decodes
-------------------------------------------------------------------------------

thEitherTextIntToJSONUntaggedValue :: EitherTextInt -> Value
thEitherTextIntToJSONUntaggedValue = $(mkToJSON optsUntaggedValue ''EitherTextInt)

thEitherTextIntToEncodingUntaggedValue :: EitherTextInt -> Encoding
thEitherTextIntToEncodingUntaggedValue = $(mkToEncoding optsUntaggedValue ''EitherTextInt)

thEitherTextIntParseJSONUntaggedValue :: Value -> Parser EitherTextInt
thEitherTextIntParseJSONUntaggedValue = $(mkParseJSON optsUntaggedValue ''EitherTextInt)


gEitherTextIntToJSONUntaggedValue :: EitherTextInt -> Value
gEitherTextIntToJSONUntaggedValue = genericToJSON optsUntaggedValue

gEitherTextIntToEncodingUntaggedValue :: EitherTextInt -> Encoding
gEitherTextIntToEncodingUntaggedValue = genericToEncoding optsUntaggedValue

gEitherTextIntParseJSONUntaggedValue :: Value -> Parser EitherTextInt
gEitherTextIntParseJSONUntaggedValue = genericParseJSON optsUntaggedValue

--------------------------------------------------------------------------------
-- Approx encoders/decoders
--------------------------------------------------------------------------------

thApproxToJSONUnwrap :: Approx String -> Value
thApproxToJSONUnwrap = $(mkToJSON optsUnwrapUnaryRecords ''Approx)

thApproxToEncodingUnwrap :: Approx String -> Encoding
thApproxToEncodingUnwrap = $(mkToEncoding optsUnwrapUnaryRecords ''Approx)

thApproxParseJSONUnwrap :: Value -> Parser (Approx String)
thApproxParseJSONUnwrap = $(mkParseJSON optsUnwrapUnaryRecords ''Approx)


thApproxToJSONDefault :: Approx String -> Value
thApproxToJSONDefault = $(mkToJSON defaultOptions ''Approx)

thApproxToEncodingDefault :: Approx String -> Encoding
thApproxToEncodingDefault = $(mkToEncoding defaultOptions ''Approx)

thApproxParseJSONDefault :: Value -> Parser (Approx String)
thApproxParseJSONDefault = $(mkParseJSON defaultOptions ''Approx)

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

--------------------------------------------------------------------------------
-- GADT encoders/decoders
--------------------------------------------------------------------------------

thGADTToJSONUnwrap :: GADT String -> Value
thGADTToJSONUnwrap = $(mkToJSON optsUnwrapUnaryRecords ''GADT)

thGADTToEncodingUnwrap :: GADT String -> Encoding
thGADTToEncodingUnwrap = $(mkToEncoding optsUnwrapUnaryRecords ''GADT)

thGADTParseJSONUnwrap :: Value -> Parser (GADT String)
thGADTParseJSONUnwrap = $(mkParseJSON optsUnwrapUnaryRecords ''GADT)


thGADTToJSONDefault :: GADT String -> Value
thGADTToJSONDefault = $(mkToJSON defaultOptions ''GADT)

thGADTToEncodingDefault :: GADT String -> Encoding
thGADTToEncodingDefault = $(mkToEncoding defaultOptions ''GADT)

thGADTParseJSONDefault :: Value -> Parser (GADT String)
thGADTParseJSONDefault = $(mkParseJSON defaultOptions ''GADT)

--------------------------------------------------------------------------------
-- OneConstructor encoders/decoders
--------------------------------------------------------------------------------

thOneConstructorToJSONDefault :: OneConstructor -> Value
thOneConstructorToJSONDefault = $(mkToJSON defaultOptions ''OneConstructor)

thOneConstructorToEncodingDefault :: OneConstructor -> Encoding
thOneConstructorToEncodingDefault = $(mkToEncoding defaultOptions ''OneConstructor)

thOneConstructorParseJSONDefault :: Value -> Parser OneConstructor
thOneConstructorParseJSONDefault = $(mkParseJSON defaultOptions ''OneConstructor)

thOneConstructorToJSONTagged :: OneConstructor -> Value
thOneConstructorToJSONTagged = $(mkToJSON optsTagSingleConstructors ''OneConstructor)

thOneConstructorToEncodingTagged :: OneConstructor -> Encoding
thOneConstructorToEncodingTagged = $(mkToEncoding optsTagSingleConstructors ''OneConstructor)

thOneConstructorParseJSONTagged :: Value -> Parser OneConstructor
thOneConstructorParseJSONTagged = $(mkParseJSON optsTagSingleConstructors ''OneConstructor)


gOneConstructorToJSONDefault :: OneConstructor -> Value
gOneConstructorToJSONDefault = genericToJSON defaultOptions

gOneConstructorToEncodingDefault :: OneConstructor -> Encoding
gOneConstructorToEncodingDefault = genericToEncoding defaultOptions

gOneConstructorParseJSONDefault :: Value -> Parser OneConstructor
gOneConstructorParseJSONDefault = genericParseJSON defaultOptions

gOneConstructorToJSONTagged :: OneConstructor -> Value
gOneConstructorToJSONTagged = genericToJSON optsTagSingleConstructors

gOneConstructorToEncodingTagged :: OneConstructor -> Encoding
gOneConstructorToEncodingTagged = genericToEncoding optsTagSingleConstructors

gOneConstructorParseJSONTagged :: Value -> Parser OneConstructor
gOneConstructorParseJSONTagged = genericParseJSON optsTagSingleConstructors

--------------------------------------------------------------------------------
-- Product2 encoders/decoders
--------------------------------------------------------------------------------

thProduct2ParseJSON :: Value -> Parser (Product2 Int Bool)
thProduct2ParseJSON = $(mkParseJSON defaultOptions ''Product2)

gProduct2ParseJSON :: Value -> Parser (Product2 Int Bool)
gProduct2ParseJSON = genericParseJSON defaultOptions
