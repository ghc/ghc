{-# LANGUAGE NoImplicitPrelude #-}

module PropertyTH ( templateHaskellTests ) where

import Prelude.Compat

import Data.Semigroup (Option(..))
import Encoders
import Instances ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck ( (===) )
import Types
import PropUtils


templateHaskellTests :: TestTree
templateHaskellTests =
    testGroup "template-haskell" [
      testGroup "toJSON" [
        testGroup "Nullary" [
            testProperty "string" (isString . thNullaryToJSONString)
          , testProperty "2ElemArray" (is2ElemArray . thNullaryToJSON2ElemArray)
          , testProperty "TaggedObject" (isNullaryTaggedObject . thNullaryToJSONTaggedObject)
          , testProperty "ObjectWithSingleField" (isObjectWithSingleField . thNullaryToJSONObjectWithSingleField)

          , testGroup "roundTrip" [
              testProperty "string" (toParseJSON thNullaryParseJSONString thNullaryToJSONString)
            , testProperty "2ElemArray" (toParseJSON thNullaryParseJSON2ElemArray thNullaryToJSON2ElemArray)
            , testProperty "TaggedObject" (toParseJSON thNullaryParseJSONTaggedObject thNullaryToJSONTaggedObject)
            , testProperty "ObjectWithSingleField" (toParseJSON thNullaryParseJSONObjectWithSingleField thNullaryToJSONObjectWithSingleField)
            ]
        ]
      , testGroup "EitherTextInt" [
          testProperty "UntaggedValue" (isUntaggedValueETI . thEitherTextIntToJSONUntaggedValue)
        , testProperty "roundtrip" (toParseJSON thEitherTextIntParseJSONUntaggedValue thEitherTextIntToJSONUntaggedValue)
        ]
      , testGroup "SomeType" [
          testProperty "2ElemArray" (is2ElemArray . thSomeTypeToJSON2ElemArray)
        , testProperty "TaggedObject" (isTaggedObject . thSomeTypeToJSONTaggedObject)
        , testProperty "ObjectWithSingleField" (isObjectWithSingleField . thSomeTypeToJSONObjectWithSingleField)
        , testGroup "roundTrip" [
            testProperty "2ElemArray" (toParseJSON thSomeTypeParseJSON2ElemArray thSomeTypeToJSON2ElemArray)
          , testProperty "TaggedObject" (toParseJSON thSomeTypeParseJSONTaggedObject thSomeTypeToJSONTaggedObject)
          , testProperty "ObjectWithSingleField" (toParseJSON thSomeTypeParseJSONObjectWithSingleField thSomeTypeToJSONObjectWithSingleField)

          , testProperty "2ElemArray unary" (toParseJSON1 thSomeTypeLiftParseJSON2ElemArray thSomeTypeLiftToJSON2ElemArray)
          , testProperty "TaggedObject unary" (toParseJSON1 thSomeTypeLiftParseJSONTaggedObject thSomeTypeLiftToJSONTaggedObject)
          , testProperty "ObjectWithSingleField unary" (toParseJSON1 thSomeTypeLiftParseJSONObjectWithSingleField thSomeTypeLiftToJSONObjectWithSingleField)

          ]
        ]
      , testGroup "Approx" [
          testProperty "string"                (isString                . thApproxToJSONUnwrap)
        , testProperty "ObjectWithSingleField" (isObjectWithSingleField . thApproxToJSONDefault)
        , testGroup "roundTrip" [
            testProperty "string"                (toParseJSON thApproxParseJSONUnwrap  thApproxToJSONUnwrap)
          , testProperty "ObjectWithSingleField" (toParseJSON thApproxParseJSONDefault thApproxToJSONDefault)
          ]
        ]
      , testGroup "GADT" [
          testProperty "string"                (isString                . thGADTToJSONUnwrap)
        , testProperty "ObjectWithSingleField" (isObjectWithSingleField . thGADTToJSONDefault)
        , testGroup "roundTrip" [
            testProperty "string"                (toParseJSON thGADTParseJSONUnwrap  thGADTToJSONUnwrap)
          , testProperty "ObjectWithSingleField" (toParseJSON thGADTParseJSONDefault thGADTToJSONDefault)
          ]
        ]
      , testGroup "OneConstructor" [
          testProperty "default" (isEmptyArray . thOneConstructorToJSONDefault)
        , testProperty "Tagged"  (isTaggedObject . thOneConstructorToJSONTagged)
        , testGroup "roundTrip" [
            testProperty "default" (toParseJSON thOneConstructorParseJSONDefault thOneConstructorToJSONDefault)
          , testProperty "Tagged"  (toParseJSON thOneConstructorParseJSONTagged  thOneConstructorToJSONTagged)
          ]
        ]
      , testGroup "OptionField" [
          testProperty "like Maybe" $
          \x -> thOptionFieldToJSON (OptionField (Option x)) === thMaybeFieldToJSON (MaybeField x)
        , testProperty "roundTrip" (toParseJSON thOptionFieldParseJSON thOptionFieldToJSON)
        ]
      ]
    , testGroup "toEncoding" [
        testProperty "NullaryString" $
        thNullaryToJSONString `sameAs` thNullaryToEncodingString
      , testProperty "Nullary2ElemArray" $
        thNullaryToJSON2ElemArray `sameAs` thNullaryToEncoding2ElemArray
      , testProperty "NullaryTaggedObject" $
        thNullaryToJSONTaggedObject `sameAs` thNullaryToEncodingTaggedObject
      , testProperty "NullaryObjectWithSingleField" $
        thNullaryToJSONObjectWithSingleField `sameAs`
        thNullaryToEncodingObjectWithSingleField
      , testProperty "ApproxUnwrap" $
        thApproxToJSONUnwrap `sameAs` thApproxToEncodingUnwrap
      , testProperty "ApproxDefault" $
        thApproxToJSONDefault `sameAs` thApproxToEncodingDefault

      , testProperty "EitherTextInt UntaggedValue" $
        thEitherTextIntToJSONUntaggedValue `sameAs` thEitherTextIntToEncodingUntaggedValue

      , testProperty "SomeType2ElemArray" $
        thSomeTypeToJSON2ElemArray `sameAs` thSomeTypeToEncoding2ElemArray
      , testProperty "SomeType2ElemArray unary" $
        thSomeTypeLiftToJSON2ElemArray `sameAs1` thSomeTypeLiftToEncoding2ElemArray
      , testProperty "SomeType2ElemArray unary agree" $
        thSomeTypeToEncoding2ElemArray `sameAs1Agree` thSomeTypeLiftToEncoding2ElemArray

      , testProperty "SomeTypeTaggedObject" $
        thSomeTypeToJSONTaggedObject `sameAs` thSomeTypeToEncodingTaggedObject
      , testProperty "SomeTypeTaggedObject unary" $
        thSomeTypeLiftToJSONTaggedObject `sameAs1` thSomeTypeLiftToEncodingTaggedObject
      , testProperty "SomeTypeTaggedObject unary agree" $
        thSomeTypeToEncodingTaggedObject `sameAs1Agree` thSomeTypeLiftToEncodingTaggedObject

      , testProperty "SomeTypeObjectWithSingleField" $
        thSomeTypeToJSONObjectWithSingleField `sameAs` thSomeTypeToEncodingObjectWithSingleField
      , testProperty "SomeTypeObjectWithSingleField unary" $
        thSomeTypeLiftToJSONObjectWithSingleField `sameAs1` thSomeTypeLiftToEncodingObjectWithSingleField
      , testProperty "SomeTypeObjectWithSingleField unary agree" $
        thSomeTypeToEncodingObjectWithSingleField `sameAs1Agree` thSomeTypeLiftToEncodingObjectWithSingleField

      , testProperty "OneConstructorDefault" $
        thOneConstructorToJSONDefault `sameAs` thOneConstructorToEncodingDefault
      , testProperty "OneConstructorTagged" $
        thOneConstructorToJSONTagged `sameAs` thOneConstructorToEncodingTagged

      , testProperty "OptionField" $
        thOptionFieldToJSON `sameAs` thOptionFieldToEncoding
      ]
    ]
