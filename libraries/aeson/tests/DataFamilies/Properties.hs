{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}

module DataFamilies.Properties (tests) where

import Prelude.Compat

import DataFamilies.Encoders
import DataFamilies.Instances ()
import PropUtils


import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

--------------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "data families" [
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
      , testGroup "SomeType" [
          testProperty "2ElemArray" (is2ElemArray . thSomeTypeToJSON2ElemArray)
        , testProperty "TaggedObject" (isTaggedObject . thSomeTypeToJSONTaggedObject)
        , testProperty "ObjectWithSingleField" (isObjectWithSingleField . thSomeTypeToJSONObjectWithSingleField)
        , testGroup "roundTrip" [
            testProperty "2ElemArray" (toParseJSON thSomeTypeParseJSON2ElemArray thSomeTypeToJSON2ElemArray)
          , testProperty "TaggedObject" (toParseJSON thSomeTypeParseJSONTaggedObject thSomeTypeToJSONTaggedObject)
          , testProperty "ObjectWithSingleField" (toParseJSON thSomeTypeParseJSONObjectWithSingleField thSomeTypeToJSONObjectWithSingleField)
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
      , testProperty "SomeType2ElemArray" $
        thSomeTypeToJSON2ElemArray `sameAs` thSomeTypeToEncoding2ElemArray
      , testProperty "SomeTypeTaggedObject" $
        thSomeTypeToJSONTaggedObject `sameAs` thSomeTypeToEncodingTaggedObject
      , testProperty "SomeTypeObjectWithSingleField" $
        thSomeTypeToJSONObjectWithSingleField `sameAs`
        thSomeTypeToEncodingObjectWithSingleField
      ]
    ]

  , testGroup "generics" [
      testGroup "toJSON" [
        testGroup "Nullary" [
            testProperty "string" (isString . gNullaryToJSONString)
          , testProperty "2ElemArray" (is2ElemArray . gNullaryToJSON2ElemArray)
          , testProperty "TaggedObject" (isNullaryTaggedObject . gNullaryToJSONTaggedObject)
          , testProperty "ObjectWithSingleField" (isObjectWithSingleField . gNullaryToJSONObjectWithSingleField)
          , testGroup "roundTrip" [
              testProperty "string" (toParseJSON gNullaryParseJSONString gNullaryToJSONString)
            , testProperty "2ElemArray" (toParseJSON gNullaryParseJSON2ElemArray gNullaryToJSON2ElemArray)
            , testProperty "TaggedObject" (toParseJSON gNullaryParseJSONTaggedObject gNullaryToJSONTaggedObject)
            , testProperty "ObjectWithSingleField" (toParseJSON gNullaryParseJSONObjectWithSingleField gNullaryToJSONObjectWithSingleField)
            ]
        ]
      , testGroup "SomeType" [
          testProperty "2ElemArray" (is2ElemArray . gSomeTypeToJSON2ElemArray)
        , testProperty "TaggedObject" (isTaggedObject . gSomeTypeToJSONTaggedObject)
        , testProperty "ObjectWithSingleField" (isObjectWithSingleField . gSomeTypeToJSONObjectWithSingleField)
        , testGroup "roundTrip" [
            testProperty "2ElemArray" (toParseJSON gSomeTypeParseJSON2ElemArray gSomeTypeToJSON2ElemArray)
          , testProperty "TaggedObject" (toParseJSON gSomeTypeParseJSONTaggedObject gSomeTypeToJSONTaggedObject)
          , testProperty "ObjectWithSingleField" (toParseJSON gSomeTypeParseJSONObjectWithSingleField gSomeTypeToJSONObjectWithSingleField)
          ]
        ]
      , testGroup "Approx" [
          testProperty "string"                (isString                . gApproxToJSONUnwrap)
        , testProperty "ObjectWithSingleField" (isObjectWithSingleField . gApproxToJSONDefault)
        , testGroup "roundTrip" [
            testProperty "string"                (toParseJSON gApproxParseJSONUnwrap  gApproxToJSONUnwrap)
          , testProperty "ObjectWithSingleField" (toParseJSON gApproxParseJSONDefault gApproxToJSONDefault)
          ]
        ]
      ]
    , testGroup "toEncoding" [
        testProperty "NullaryString" $
        gNullaryToJSONString `sameAs` gNullaryToEncodingString
      , testProperty "Nullary2ElemArray" $
        gNullaryToJSON2ElemArray `sameAs` gNullaryToEncoding2ElemArray
      , testProperty "NullaryTaggedObject" $
        gNullaryToJSONTaggedObject `sameAs` gNullaryToEncodingTaggedObject
      , testProperty "NullaryObjectWithSingleField" $
        gNullaryToJSONObjectWithSingleField `sameAs`
        gNullaryToEncodingObjectWithSingleField
      , testProperty "ApproxUnwrap" $
        gApproxToJSONUnwrap `sameAs` gApproxToEncodingUnwrap
      , testProperty "ApproxDefault" $
        gApproxToJSONDefault `sameAs` gApproxToEncodingDefault
      , testProperty "SomeType2ElemArray" $
        gSomeTypeToJSON2ElemArray `sameAs` gSomeTypeToEncoding2ElemArray
      , testProperty "SomeTypeTaggedObject" $
        gSomeTypeToJSONTaggedObject `sameAs` gSomeTypeToEncodingTaggedObject
      , testProperty "SomeTypeObjectWithSingleField" $
        gSomeTypeToJSONObjectWithSingleField `sameAs`
        gSomeTypeToEncodingObjectWithSingleField
      ]
    ]
  ]
