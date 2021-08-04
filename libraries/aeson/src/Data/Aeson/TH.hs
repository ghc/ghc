{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UndecidableInstances #-}
#if __GLASGOW_HASKELL__ >= 800
-- a) THQ works on cross-compilers and unregisterised GHCs
-- b) may make compilation faster as no dynamic loading is ever needed (not sure about this)
-- c) removes one hindrance to have code inferred as SafeHaskell safe
{-# LANGUAGE TemplateHaskellQuotes #-}
#else
{-# LANGUAGE TemplateHaskell #-}
#endif

#include "incoherent-compat.h"
#include "overlapping-compat.h"

{-|
Module:      Data.Aeson.TH
Copyright:   (c) 2011-2016 Bryan O'Sullivan
             (c) 2011 MailRank, Inc.
License:     BSD3
Stability:   experimental
Portability: portable

Functions to mechanically derive 'ToJSON' and 'FromJSON' instances. Note that
you need to enable the @TemplateHaskell@ language extension in order to use this
module.

An example shows how instances are generated for arbitrary data types. First we
define a data type:

@
data D a = Nullary
         | Unary Int
         | Product String Char a
         | Record { testOne   :: Double
                  , testTwo   :: Bool
                  , testThree :: D a
                  } deriving Eq
@

Next we derive the necessary instances. Note that we make use of the
feature to change record field names. In this case we drop the first 4
characters of every field name. We also modify constructor names by
lower-casing them:

@
$('deriveJSON' 'defaultOptions'{'fieldLabelModifier' = 'drop' 4, 'constructorTagModifier' = map toLower} ''D)
@

Now we can use the newly created instances.

@
d :: D 'Int'
d = Record { testOne = 3.14159
           , testTwo = 'True'
           , testThree = Product \"test\" \'A\' 123
           }
@

@
fromJSON (toJSON d) == Success d
@

This also works for data family instances, but instead of passing in the data
family name (with double quotes), we pass in a data family instance
constructor (with a single quote):

@
data family DF a
data instance DF Int = DF1 Int
                     | DF2 Int Int
                     deriving Eq

$('deriveJSON' 'defaultOptions' 'DF1)
-- Alternatively, one could pass 'DF2 instead
@

Please note that you can derive instances for tuples using the following syntax:

@
-- FromJSON and ToJSON instances for 4-tuples.
$('deriveJSON' 'defaultOptions' ''(,,,))
@

-}
module Data.Aeson.TH
    (
      -- * Encoding configuration
      Options(..)
    , SumEncoding(..)
    , defaultOptions
    , defaultTaggedObject

     -- * FromJSON and ToJSON derivation
    , deriveJSON
    , deriveJSON1
    , deriveJSON2

    , deriveToJSON
    , deriveToJSON1
    , deriveToJSON2
    , deriveFromJSON
    , deriveFromJSON1
    , deriveFromJSON2

    , mkToJSON
    , mkLiftToJSON
    , mkLiftToJSON2
    , mkToEncoding
    , mkLiftToEncoding
    , mkLiftToEncoding2
    , mkParseJSON
    , mkLiftParseJSON
    , mkLiftParseJSON2
    ) where

import Prelude.Compat hiding (fail)

-- We don't have MonadFail Q, so we should use `fail` from real `Prelude`
import Prelude (fail)

import Control.Applicative ((<|>))
import Data.Aeson (Object, (.:), FromJSON(..), FromJSON1(..), FromJSON2(..), ToJSON(..), ToJSON1(..), ToJSON2(..))
import Data.Aeson.Types (Options(..), Parser, SumEncoding(..), Value(..), defaultOptions, defaultTaggedObject)
import Data.Aeson.Types.Internal ((<?>), JSONPathElement(Key))
import Data.Aeson.Types.FromJSON (parseOptionalFieldWith)
import Data.Aeson.Types.ToJSON (fromPairs, pair)
import Control.Monad (liftM2, unless, when)
import Data.Foldable (foldr')
#if MIN_VERSION_template_haskell(2,8,0) && !MIN_VERSION_template_haskell(2,10,0)
import Data.List (nub)
#endif
import Data.List (foldl', genericLength, intercalate, partition, union)
import Data.List.NonEmpty ((<|), NonEmpty((:|)))
import Data.Map (Map)
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import qualified Data.Monoid as Monoid
import Data.Set (Set)
import Language.Haskell.TH hiding (Arity)
import Language.Haskell.TH.Datatype
#if MIN_VERSION_template_haskell(2,8,0) && !(MIN_VERSION_template_haskell(2,10,0))
import Language.Haskell.TH.Syntax (mkNameG_tc)
#endif
import Text.Printf (printf)
import qualified Data.Aeson.Encoding.Internal as E
import qualified Data.Foldable as F (all)
import qualified Data.HashMap.Strict as H (difference, fromList, keys, lookup, toList)
import qualified Data.List.NonEmpty as NE (length, reverse)
import qualified Data.Map as M (fromList, keys, lookup , singleton, size)
import qualified Data.Semigroup as Semigroup (Option(..))
import qualified Data.Set as Set (empty, insert, member)
import qualified Data.Text as T (Text, pack, unpack)
import qualified Data.Vector as V (unsafeIndex, null, length, create, empty)
import qualified Data.Vector.Mutable as VM (unsafeNew, unsafeWrite)

--------------------------------------------------------------------------------
-- Convenience
--------------------------------------------------------------------------------

-- | Generates both 'ToJSON' and 'FromJSON' instance declarations for the given
-- data type or data family instance constructor.
--
-- This is a convienience function which is equivalent to calling both
-- 'deriveToJSON' and 'deriveFromJSON'.
deriveJSON :: Options
           -- ^ Encoding options.
           -> Name
           -- ^ Name of the type for which to generate 'ToJSON' and 'FromJSON'
           -- instances.
           -> Q [Dec]
deriveJSON = deriveJSONBoth deriveToJSON deriveFromJSON

-- | Generates both 'ToJSON1' and 'FromJSON1' instance declarations for the given
-- data type or data family instance constructor.
--
-- This is a convienience function which is equivalent to calling both
-- 'deriveToJSON1' and 'deriveFromJSON1'.
deriveJSON1 :: Options
            -- ^ Encoding options.
            -> Name
            -- ^ Name of the type for which to generate 'ToJSON1' and 'FromJSON1'
            -- instances.
            -> Q [Dec]
deriveJSON1 = deriveJSONBoth deriveToJSON1 deriveFromJSON1

-- | Generates both 'ToJSON2' and 'FromJSON2' instance declarations for the given
-- data type or data family instance constructor.
--
-- This is a convienience function which is equivalent to calling both
-- 'deriveToJSON2' and 'deriveFromJSON2'.
deriveJSON2 :: Options
            -- ^ Encoding options.
            -> Name
            -- ^ Name of the type for which to generate 'ToJSON2' and 'FromJSON2'
            -- instances.
            -> Q [Dec]
deriveJSON2 = deriveJSONBoth deriveToJSON2 deriveFromJSON2

--------------------------------------------------------------------------------
-- ToJSON
--------------------------------------------------------------------------------

{-
TODO: Don't constrain phantom type variables.

data Foo a = Foo Int
instance (ToJSON a) ⇒ ToJSON Foo where ...

The above (ToJSON a) constraint is not necessary and perhaps undesirable.
-}

-- | Generates a 'ToJSON' instance declaration for the given data type or
-- data family instance constructor.
deriveToJSON :: Options
             -- ^ Encoding options.
             -> Name
             -- ^ Name of the type for which to generate a 'ToJSON' instance
             -- declaration.
             -> Q [Dec]
deriveToJSON = deriveToJSONCommon toJSONClass

-- | Generates a 'ToJSON1' instance declaration for the given data type or
-- data family instance constructor.
deriveToJSON1 :: Options
              -- ^ Encoding options.
              -> Name
              -- ^ Name of the type for which to generate a 'ToJSON1' instance
              -- declaration.
              -> Q [Dec]
deriveToJSON1 = deriveToJSONCommon toJSON1Class

-- | Generates a 'ToJSON2' instance declaration for the given data type or
-- data family instance constructor.
deriveToJSON2 :: Options
              -- ^ Encoding options.
              -> Name
              -- ^ Name of the type for which to generate a 'ToJSON2' instance
              -- declaration.
              -> Q [Dec]
deriveToJSON2 = deriveToJSONCommon toJSON2Class

deriveToJSONCommon :: JSONClass
                   -- ^ The ToJSON variant being derived.
                   -> Options
                   -- ^ Encoding options.
                   -> Name
                   -- ^ Name of the type for which to generate an instance.
                   -> Q [Dec]
deriveToJSONCommon = deriveJSONClass [ (ToJSON,     \jc _ -> consToValue Value jc)
                                     , (ToEncoding, \jc _ -> consToValue Encoding jc)
                                     ]

-- | Generates a lambda expression which encodes the given data type or
-- data family instance constructor as a 'Value'.
mkToJSON :: Options -- ^ Encoding options.
         -> Name -- ^ Name of the type to encode.
         -> Q Exp
mkToJSON = mkToJSONCommon toJSONClass

-- | Generates a lambda expression which encodes the given data type or
-- data family instance constructor as a 'Value' by using the given encoding
-- function on occurrences of the last type parameter.
mkLiftToJSON :: Options -- ^ Encoding options.
             -> Name -- ^ Name of the type to encode.
             -> Q Exp
mkLiftToJSON = mkToJSONCommon toJSON1Class

-- | Generates a lambda expression which encodes the given data type or
-- data family instance constructor as a 'Value' by using the given encoding
-- functions on occurrences of the last two type parameters.
mkLiftToJSON2 :: Options -- ^ Encoding options.
              -> Name -- ^ Name of the type to encode.
              -> Q Exp
mkLiftToJSON2 = mkToJSONCommon toJSON2Class

mkToJSONCommon :: JSONClass -- ^ Which class's method is being derived.
               -> Options -- ^ Encoding options.
               -> Name -- ^ Name of the encoded type.
               -> Q Exp
mkToJSONCommon = mkFunCommon (\jc _ -> consToValue Value jc)

-- | Generates a lambda expression which encodes the given data type or
-- data family instance constructor as a JSON string.
mkToEncoding :: Options -- ^ Encoding options.
             -> Name -- ^ Name of the type to encode.
             -> Q Exp
mkToEncoding = mkToEncodingCommon toJSONClass

-- | Generates a lambda expression which encodes the given data type or
-- data family instance constructor as a JSON string by using the given encoding
-- function on occurrences of the last type parameter.
mkLiftToEncoding :: Options -- ^ Encoding options.
                 -> Name -- ^ Name of the type to encode.
                 -> Q Exp
mkLiftToEncoding = mkToEncodingCommon toJSON1Class

-- | Generates a lambda expression which encodes the given data type or
-- data family instance constructor as a JSON string by using the given encoding
-- functions on occurrences of the last two type parameters.
mkLiftToEncoding2 :: Options -- ^ Encoding options.
                  -> Name -- ^ Name of the type to encode.
                  -> Q Exp
mkLiftToEncoding2 = mkToEncodingCommon toJSON2Class

mkToEncodingCommon :: JSONClass -- ^ Which class's method is being derived.
                   -> Options -- ^ Encoding options.
                   -> Name -- ^ Name of the encoded type.
                   -> Q Exp
mkToEncodingCommon = mkFunCommon (\jc _ -> consToValue Encoding jc)

-- | Helper function used by both 'deriveToJSON' and 'mkToJSON'. Generates
-- code to generate a 'Value' or 'Encoding' of a number of constructors. All
-- constructors must be from the same type.
consToValue :: ToJSONFun
            -- ^ The method ('toJSON' or 'toEncoding') being derived.
            -> JSONClass
            -- ^ The ToJSON variant being derived.
            -> Options
            -- ^ Encoding options.
            -> [Type]
            -- ^ The types from the data type/data family instance declaration
            -> [ConstructorInfo]
            -- ^ Constructors for which to generate JSON generating code.
            -> Q Exp

consToValue _ _ _ _ [] = error $ "Data.Aeson.TH.consToValue: "
                             ++ "Not a single constructor given!"

consToValue target jc opts instTys cons = do
    value <- newName "value"
    tjs   <- newNameList "_tj"  $ arityInt jc
    tjls  <- newNameList "_tjl" $ arityInt jc
    let zippedTJs      = zip tjs tjls
        interleavedTJs = interleave tjs tjls
        lastTyVars     = map varTToName $ drop (length instTys - arityInt jc) instTys
        tvMap          = M.fromList $ zip lastTyVars zippedTJs
    lamE (map varP $ interleavedTJs ++ [value]) $
        caseE (varE value) (matches tvMap)
  where
    matches tvMap = case cons of
      -- A single constructor is directly encoded. The constructor itself may be
      -- forgotten.
      [con] | not (tagSingleConstructors opts) -> [argsToValue target jc tvMap opts False con]
      _ | allNullaryToStringTag opts && all isNullary cons ->
              [ match (conP conName []) (normalB $ conStr target opts conName) []
              | con <- cons
              , let conName = constructorName con
              ]
        | otherwise -> [argsToValue target jc tvMap opts True con | con <- cons]

-- | Name of the constructor as a quoted 'Value' or 'Encoding'.
conStr :: ToJSONFun -> Options -> Name -> Q Exp
conStr Value opts = appE [|String|] . conTxt opts
conStr Encoding opts = appE [|E.text|] . conTxt opts

-- | Name of the constructor as a quoted 'Text'.
conTxt :: Options -> Name -> Q Exp
conTxt opts = appE [|T.pack|] . stringE . conString opts

-- | Name of the constructor.
conString :: Options -> Name -> String
conString opts = constructorTagModifier opts . nameBase

-- | If constructor is nullary.
isNullary :: ConstructorInfo -> Bool
isNullary ConstructorInfo { constructorVariant = NormalConstructor
                          , constructorFields  = tys } = null tys
isNullary _ = False

-- | Wrap fields of a non-record constructor. See 'sumToValue'.
opaqueSumToValue :: ToJSONFun -> Options -> Bool -> Bool -> Name -> ExpQ -> ExpQ
opaqueSumToValue target opts multiCons nullary conName value =
  sumToValue target opts multiCons nullary conName
    value
    pairs
  where
    pairs contentsFieldName = pairE contentsFieldName value

-- | Wrap fields of a record constructor. See 'sumToValue'.
recordSumToValue :: ToJSONFun -> Options -> Bool -> Bool -> Name -> ExpQ -> ExpQ
recordSumToValue target opts multiCons nullary conName pairs =
  sumToValue target opts multiCons nullary conName
    (fromPairsE pairs)
    (const pairs)

-- | Wrap fields of a constructor.
sumToValue
  :: ToJSONFun
  -- ^ The method being derived.
  -> Options
  -- ^ Deriving options.
  -> Bool
  -- ^ Does this type have multiple constructors.
  -> Bool
  -- ^ Is this constructor nullary.
  -> Name
  -- ^ Constructor name.
  -> ExpQ
  -- ^ Fields of the constructor as a 'Value' or 'Encoding'.
  -> (String -> ExpQ)
  -- ^ Representation of an 'Object' fragment used for the 'TaggedObject'
  -- variant; of type @[(Text,Value)]@ or @[Encoding]@, depending on the method
  -- being derived.
  --
  -- - For non-records, produces a pair @"contentsFieldName":value@,
  --   given a @contentsFieldName@ as an argument. See 'opaqueSumToValue'.
  -- - For records, produces the list of pairs corresponding to fields of the
  --   encoded value (ignores the argument). See 'recordSumToValue'.
  -> ExpQ
sumToValue target opts multiCons nullary conName value pairs
    | multiCons =
        case sumEncoding opts of
          TwoElemArray ->
            array target [conStr target opts conName, value]
          TaggedObject{tagFieldName, contentsFieldName} ->
            -- TODO: Maybe throw an error in case
            -- tagFieldName overwrites a field in pairs.
            let tag = pairE tagFieldName (conStr target opts conName)
                content = pairs contentsFieldName
            in fromPairsE $
              if nullary then tag else infixApp tag [|(Monoid.<>)|] content
          ObjectWithSingleField ->
            objectE [(conString opts conName, value)]
          UntaggedValue | nullary -> conStr target opts conName
          UntaggedValue -> value
    | otherwise = value

-- | Generates code to generate the JSON encoding of a single constructor.
argsToValue :: ToJSONFun -> JSONClass -> TyVarMap -> Options -> Bool -> ConstructorInfo -> Q Match

-- Polyadic constructors with special case for unary constructors.
argsToValue target jc tvMap opts multiCons
  ConstructorInfo { constructorName    = conName
                  , constructorVariant = NormalConstructor
                  , constructorFields  = argTys } = do
    argTys' <- mapM resolveTypeSynonyms argTys
    let len = length argTys'
    args <- newNameList "arg" len
    let js = case [ dispatchToJSON target jc conName tvMap argTy
                      `appE` varE arg
                  | (arg, argTy) <- zip args argTys'
                  ] of
               -- Single argument is directly converted.
               [e] -> e
               -- Zero and multiple arguments are converted to a JSON array.
               es -> array target es

    match (conP conName $ map varP args)
          (normalB $ opaqueSumToValue target opts multiCons (null argTys') conName js)
          []

-- Records.
argsToValue target jc tvMap opts multiCons
  info@ConstructorInfo { constructorName    = conName
                       , constructorVariant = RecordConstructor fields
                       , constructorFields  = argTys } =
    case (unwrapUnaryRecords opts, not multiCons, argTys) of
      (True,True,[_]) -> argsToValue target jc tvMap opts multiCons
                                     (info{constructorVariant = NormalConstructor})
      _ -> do
        argTys' <- mapM resolveTypeSynonyms argTys
        args <- newNameList "arg" $ length argTys'
        let pairs | omitNothingFields opts = infixApp maybeFields
                                                      [|(Monoid.<>)|]
                                                      restFields
                  | otherwise = mconcatE (map pureToPair argCons)

            argCons = zip3 (map varE args) argTys' fields

            maybeFields = mconcatE (map maybeToPair maybes)

            restFields = mconcatE (map pureToPair rest)

            (maybes0, rest0) = partition isMaybe argCons
            (options, rest) = partition isOption rest0
            maybes = maybes0 ++ map optionToMaybe options

            maybeToPair = toPairLifted True
            pureToPair = toPairLifted False

            toPairLifted lifted (arg, argTy, field) =
              let toValue = dispatchToJSON target jc conName tvMap argTy
                  fieldName = fieldLabel opts field
                  e arg' = pairE fieldName (toValue `appE` arg')
              in if lifted
                then do
                  x <- newName "x"
                  [|maybe mempty|] `appE` lam1E (varP x) (e (varE x)) `appE` arg
                else e arg

        match (conP conName $ map varP args)
              (normalB $ recordSumToValue target opts multiCons (null argTys) conName pairs)
              []

-- Infix constructors.
argsToValue target jc tvMap opts multiCons
  ConstructorInfo { constructorName    = conName
                  , constructorVariant = InfixConstructor
                  , constructorFields  = argTys } = do
    [alTy, arTy] <- mapM resolveTypeSynonyms argTys
    al <- newName "argL"
    ar <- newName "argR"
    match (infixP (varP al) conName (varP ar))
          ( normalB
          $ opaqueSumToValue target opts multiCons False conName
          $ array target
              [ dispatchToJSON target jc conName tvMap aTy
                  `appE` varE a
              | (a, aTy) <- [(al,alTy), (ar,arTy)]
              ]
          )
          []

isMaybe :: (a, Type, b) -> Bool
isMaybe (_, AppT (ConT t) _, _) = t == ''Maybe
isMaybe _                       = False

isOption :: (a, Type, b) -> Bool
isOption (_, AppT (ConT t) _, _) = t == ''Semigroup.Option
isOption _                       = False

optionToMaybe :: (ExpQ, b, c) -> (ExpQ, b, c)
optionToMaybe (a, b, c) = ([|Semigroup.getOption|] `appE` a, b, c)

(<^>) :: ExpQ -> ExpQ -> ExpQ
(<^>) a b = infixApp a [|(E.><)|] b
infixr 6 <^>

(<%>) :: ExpQ -> ExpQ -> ExpQ
(<%>) a b = a <^> [|E.comma|] <^> b
infixr 4 <%>

-- | Wrap a list of quoted 'Value's in a quoted 'Array' (of type 'Value').
array :: ToJSONFun -> [ExpQ] -> ExpQ
array Encoding [] = [|E.emptyArray_|]
array Value [] = [|Array V.empty|]
array Encoding es = [|E.wrapArray|] `appE` foldr1 (<%>) es
array Value es = do
  mv <- newName "mv"
  let newMV = bindS (varP mv)
                    ([|VM.unsafeNew|] `appE`
                      litE (integerL $ fromIntegral (length es)))
      stmts = [ noBindS $
                  [|VM.unsafeWrite|] `appE`
                    varE mv `appE`
                      litE (integerL ix) `appE`
                        e
              | (ix, e) <- zip [(0::Integer)..] es
              ]
      ret = noBindS $ [|return|] `appE` varE mv
  [|Array|] `appE`
             (varE 'V.create `appE`
               doE (newMV:stmts++[ret]))

-- | Wrap an associative list of keys and quoted values in a quoted 'Object'.
objectE :: [(String, ExpQ)] -> ExpQ
objectE = fromPairsE . mconcatE . fmap (uncurry pairE)

-- | 'mconcat' a list of fixed length.
--
-- > mconcatE [ [|x|], [|y|], [|z|] ] = [| x <> (y <> z) |]
mconcatE :: [ExpQ] -> ExpQ
mconcatE [] = [|Monoid.mempty|]
mconcatE [x] = x
mconcatE (x : xs) = infixApp x [|(Monoid.<>)|] (mconcatE xs)

fromPairsE :: ExpQ -> ExpQ
fromPairsE = ([|fromPairs|] `appE`)

-- | Create (an encoding of) a key-value pair.
--
-- > pairE "k" [|v|] = [|pair "k" v|]
pairE :: String -> ExpQ -> ExpQ
pairE k v = [|pair k|] `appE` v

--------------------------------------------------------------------------------
-- FromJSON
--------------------------------------------------------------------------------

-- | Generates a 'FromJSON' instance declaration for the given data type or
-- data family instance constructor.
deriveFromJSON :: Options
               -- ^ Encoding options.
               -> Name
               -- ^ Name of the type for which to generate a 'FromJSON' instance
               -- declaration.
               -> Q [Dec]
deriveFromJSON = deriveFromJSONCommon fromJSONClass

-- | Generates a 'FromJSON1' instance declaration for the given data type or
-- data family instance constructor.
deriveFromJSON1 :: Options
                -- ^ Encoding options.
                -> Name
                -- ^ Name of the type for which to generate a 'FromJSON1' instance
                -- declaration.
                -> Q [Dec]
deriveFromJSON1 = deriveFromJSONCommon fromJSON1Class

-- | Generates a 'FromJSON2' instance declaration for the given data type or
-- data family instance constructor.
deriveFromJSON2 :: Options
                -- ^ Encoding options.
                -> Name
                -- ^ Name of the type for which to generate a 'FromJSON3' instance
                -- declaration.
                -> Q [Dec]
deriveFromJSON2 = deriveFromJSONCommon fromJSON2Class

deriveFromJSONCommon :: JSONClass
                     -- ^ The FromJSON variant being derived.
                     -> Options
                     -- ^ Encoding options.
                     -> Name
                     -- ^ Name of the type for which to generate an instance.
                     -- declaration.
                     -> Q [Dec]
deriveFromJSONCommon = deriveJSONClass [(ParseJSON, consFromJSON)]

-- | Generates a lambda expression which parses the JSON encoding of the given
-- data type or data family instance constructor.
mkParseJSON :: Options -- ^ Encoding options.
            -> Name -- ^ Name of the encoded type.
            -> Q Exp
mkParseJSON = mkParseJSONCommon fromJSONClass

-- | Generates a lambda expression which parses the JSON encoding of the given
-- data type or data family instance constructor by using the given parsing
-- function on occurrences of the last type parameter.
mkLiftParseJSON :: Options -- ^ Encoding options.
                -> Name -- ^ Name of the encoded type.
                -> Q Exp
mkLiftParseJSON = mkParseJSONCommon fromJSON1Class

-- | Generates a lambda expression which parses the JSON encoding of the given
-- data type or data family instance constructor by using the given parsing
-- functions on occurrences of the last two type parameters.
mkLiftParseJSON2 :: Options -- ^ Encoding options.
                 -> Name -- ^ Name of the encoded type.
                 -> Q Exp
mkLiftParseJSON2 = mkParseJSONCommon fromJSON2Class

mkParseJSONCommon :: JSONClass -- ^ Which class's method is being derived.
                  -> Options -- ^ Encoding options.
                  -> Name -- ^ Name of the encoded type.
                  -> Q Exp
mkParseJSONCommon = mkFunCommon consFromJSON

-- | Helper function used by both 'deriveFromJSON' and 'mkParseJSON'. Generates
-- code to parse the JSON encoding of a number of constructors. All constructors
-- must be from the same type.
consFromJSON :: JSONClass
             -- ^ The FromJSON variant being derived.
             -> Name
             -- ^ Name of the type to which the constructors belong.
             -> Options
             -- ^ Encoding options
             -> [Type]
             -- ^ The types from the data type/data family instance declaration
             -> [ConstructorInfo]
             -- ^ Constructors for which to generate JSON parsing code.
             -> Q Exp

consFromJSON _ _ _ _ [] = error $ "Data.Aeson.TH.consFromJSON: "
                                ++ "Not a single constructor given!"

consFromJSON jc tName opts instTys cons = do
  value <- newName "value"
  pjs   <- newNameList "_pj"  $ arityInt jc
  pjls  <- newNameList "_pjl" $ arityInt jc
  let zippedPJs      = zip pjs pjls
      interleavedPJs = interleave pjs pjls
      lastTyVars     = map varTToName $ drop (length instTys - arityInt jc) instTys
      tvMap          = M.fromList $ zip lastTyVars zippedPJs
  lamE (map varP $ interleavedPJs ++ [value]) $ lamExpr value tvMap

  where
    checkExi tvMap con = checkExistentialContext jc tvMap
                                                 (constructorContext con)
                                                 (constructorName con)

    lamExpr value tvMap = case cons of
      [con]
        | not (tagSingleConstructors opts)
            -> checkExi tvMap con $ parseArgs jc tvMap tName opts con (Right value)
      _ | sumEncoding opts == UntaggedValue
            -> parseUntaggedValue tvMap cons value
        | otherwise
            -> caseE (varE value) $
                   if allNullaryToStringTag opts && all isNullary cons
                   then allNullaryMatches
                   else mixedMatches tvMap

    allNullaryMatches =
      [ do txt <- newName "txt"
           match (conP 'String [varP txt])
                 (guardedB $
                  [ liftM2 (,) (normalG $
                                  infixApp (varE txt)
                                           [|(==)|]
                                           (conTxt opts conName)
                               )
                               ([|pure|] `appE` conE conName)
                  | con <- cons
                  , let conName = constructorName con
                  ]
                  ++
                  [ liftM2 (,)
                      (normalG [|otherwise|])
                      ( [|noMatchFail|]
                        `appE` litE (stringL $ show tName)
                        `appE` ([|T.unpack|] `appE` varE txt)
                      )
                  ]
                 )
                 []
      , do other <- newName "other"
           match (varP other)
                 (normalB $ [|noStringFail|]
                    `appE` litE (stringL $ show tName)
                    `appE` ([|valueConName|] `appE` varE other)
                 )
                 []
      ]

    mixedMatches tvMap =
        case sumEncoding opts of
          TaggedObject {tagFieldName, contentsFieldName} ->
            parseObject $ parseTaggedObject tvMap tagFieldName contentsFieldName
          UntaggedValue -> error "UntaggedValue: Should be handled already"
          ObjectWithSingleField ->
            parseObject $ parseObjectWithSingleField tvMap
          TwoElemArray ->
            [ do arr <- newName "array"
                 match (conP 'Array [varP arr])
                       (guardedB
                        [ liftM2 (,) (normalG $ infixApp ([|V.length|] `appE` varE arr)
                                                         [|(==)|]
                                                         (litE $ integerL 2))
                                     (parse2ElemArray tvMap arr)
                        , liftM2 (,) (normalG [|otherwise|])
                                     ([|not2ElemArray|]
                                       `appE` litE (stringL $ show tName)
                                       `appE` ([|V.length|] `appE` varE arr))
                        ]
                       )
                       []
            , do other <- newName "other"
                 match (varP other)
                       ( normalB
                         $ [|noArrayFail|]
                             `appE` litE (stringL $ show tName)
                             `appE` ([|valueConName|] `appE` varE other)
                       )
                       []
            ]

    parseObject f =
        [ do obj <- newName "obj"
             match (conP 'Object [varP obj]) (normalB $ f obj) []
        , do other <- newName "other"
             match (varP other)
                   ( normalB
                     $ [|noObjectFail|]
                         `appE` litE (stringL $ show tName)
                         `appE` ([|valueConName|] `appE` varE other)
                   )
                   []
        ]

    parseTaggedObject tvMap typFieldName valFieldName obj = do
      conKey <- newName "conKey"
      doE [ bindS (varP conKey)
                  (infixApp (varE obj)
                            [|(.:)|]
                            ([|T.pack|] `appE` stringE typFieldName))
          , noBindS $ parseContents tvMap conKey (Left (valFieldName, obj)) 'conNotFoundFailTaggedObject
          ]

    parseUntaggedValue tvMap cons' conVal =
        foldr1 (\e e' -> infixApp e [|(<|>)|] e')
               (map (\x -> parseValue tvMap x conVal) cons')

    parseValue _tvMap
        ConstructorInfo { constructorName    = conName
                        , constructorVariant = NormalConstructor
                        , constructorFields  = [] }
        conVal = do
      str <- newName "str"
      caseE (varE conVal)
        [ match (conP 'String [varP str])
                (guardedB
                  [ liftM2 (,) (normalG $ infixApp (varE str) [|(==)|] (conTxt opts conName)
                               )
                               ([|pure|] `appE` conE conName)
                  ]
                )
                []
        , matchFailed tName conName "String"
        ]
    parseValue tvMap con conVal =
      checkExi tvMap con $ parseArgs jc tvMap tName opts con (Right conVal)


    parse2ElemArray tvMap arr = do
      conKey <- newName "conKey"
      conVal <- newName "conVal"
      let letIx n ix =
              valD (varP n)
                   (normalB ([|V.unsafeIndex|] `appE`
                               varE arr `appE`
                               litE (integerL ix)))
                   []
      letE [ letIx conKey 0
           , letIx conVal 1
           ]
           (caseE (varE conKey)
                  [ do txt <- newName "txt"
                       match (conP 'String [varP txt])
                             (normalB $ parseContents tvMap
                                                      txt
                                                      (Right conVal)
                                                      'conNotFoundFail2ElemArray
                             )
                             []
                  , do other <- newName "other"
                       match (varP other)
                             ( normalB
                               $ [|firstElemNoStringFail|]
                                     `appE` litE (stringL $ show tName)
                                     `appE` ([|valueConName|] `appE` varE other)
                             )
                             []
                  ]
           )

    parseObjectWithSingleField tvMap obj = do
      conKey <- newName "conKey"
      conVal <- newName "conVal"
      caseE ([e|H.toList|] `appE` varE obj)
            [ match (listP [tupP [varP conKey, varP conVal]])
                    (normalB $ parseContents tvMap conKey (Right conVal) 'conNotFoundFailObjectSingleField)
                    []
            , do other <- newName "other"
                 match (varP other)
                       (normalB $ [|wrongPairCountFail|]
                                  `appE` litE (stringL $ show tName)
                                  `appE` ([|show . length|] `appE` varE other)
                       )
                       []
            ]

    parseContents tvMap conKey contents errorFun =
        caseE (varE conKey)
              [ match wildP
                      ( guardedB $
                        [ do g <- normalG $ infixApp (varE conKey)
                                                     [|(==)|]
                                                     ([|T.pack|] `appE`
                                                        conNameExp opts con)
                             e <- checkExi tvMap con $
                                  parseArgs jc tvMap tName opts con contents
                             return (g, e)
                        | con <- cons
                        ]
                        ++
                        [ liftM2 (,)
                                 (normalG [e|otherwise|])
                                 ( varE errorFun
                                   `appE` litE (stringL $ show tName)
                                   `appE` listE (map ( litE
                                                     . stringL
                                                     . constructorTagModifier opts
                                                     . nameBase
                                                     . constructorName
                                                     ) cons
                                                )
                                   `appE` ([|T.unpack|] `appE` varE conKey)
                                 )
                        ]
                      )
                      []
              ]

parseNullaryMatches :: Name -> Name -> [Q Match]
parseNullaryMatches tName conName =
    [ do arr <- newName "arr"
         match (conP 'Array [varP arr])
               (guardedB
                [ liftM2 (,) (normalG $ [|V.null|] `appE` varE arr)
                             ([|pure|] `appE` conE conName)
                , liftM2 (,) (normalG [|otherwise|])
                             (parseTypeMismatch tName conName
                                (litE $ stringL "an empty Array")
                                (infixApp (litE $ stringL "Array of length ")
                                          [|(++)|]
                                          ([|show . V.length|] `appE` varE arr)
                                )
                             )
                ]
               )
               []
    , matchFailed tName conName "Array"
    ]

parseUnaryMatches :: JSONClass -> TyVarMap -> Type -> Name -> [Q Match]
parseUnaryMatches jc tvMap argTy conName =
    [ do arg <- newName "arg"
         match (varP arg)
               ( normalB $ infixApp (conE conName)
                                    [|(<$>)|]
                                    (dispatchParseJSON jc conName tvMap argTy
                                      `appE` varE arg)
               )
               []
    ]

parseRecord :: JSONClass
            -> TyVarMap
            -> [Type]
            -> Options
            -> Name
            -> Name
            -> [Name]
            -> Name
            -> Bool
            -> ExpQ
parseRecord jc tvMap argTys opts tName conName fields obj inTaggedObject =
    (if rejectUnknownFields opts
     then infixApp checkUnknownRecords [|(>>)|]
     else id) $
    foldl' (\a b -> infixApp a [|(<*>)|] b)
           (infixApp (conE conName) [|(<$>)|] x)
           xs
    where
      tagFieldNameAppender =
          if inTaggedObject then (tagFieldName (sumEncoding opts) :) else id
      knownFields = appE [|H.fromList|] $ listE $
          map (\knownName -> tupE [appE [|T.pack|] $ litE $ stringL knownName, [|()|]]) $
              tagFieldNameAppender $ map (fieldLabel opts) fields
      checkUnknownRecords =
          caseE (appE [|H.keys|] $ infixApp (varE obj) [|H.difference|] knownFields)
              [ match (listP []) (normalB [|return ()|]) []
              , newName "unknownFields" >>=
                  \unknownFields -> match (varP unknownFields)
                      (normalB $ appE [|fail|] $ infixApp
                          (litE (stringL "Unknown fields: "))
                          [|(++)|]
                          (appE [|show|] (varE unknownFields)))
                      []
              ]
      x:xs = [ [|lookupField|]
               `appE` dispatchParseJSON jc conName tvMap argTy
               `appE` litE (stringL $ show tName)
               `appE` litE (stringL $ constructorTagModifier opts $ nameBase conName)
               `appE` varE obj
               `appE` ( [|T.pack|] `appE` stringE (fieldLabel opts field)
                      )
             | (field, argTy) <- zip fields argTys
             ]

getValField :: Name -> String -> [MatchQ] -> Q Exp
getValField obj valFieldName matches = do
  val <- newName "val"
  doE [ bindS (varP val) $ infixApp (varE obj)
                                    [|(.:)|]
                                    ([|T.pack|] `appE`
                                       litE (stringL valFieldName))
      , noBindS $ caseE (varE val) matches
      ]

matchCases :: Either (String, Name) Name -> [MatchQ] -> Q Exp
matchCases (Left (valFieldName, obj)) = getValField obj valFieldName
matchCases (Right valName)            = caseE (varE valName)

-- | Generates code to parse the JSON encoding of a single constructor.
parseArgs :: JSONClass -- ^ The FromJSON variant being derived.
          -> TyVarMap -- ^ Maps the last type variables to their decoding
                      --   function arguments.
          -> Name -- ^ Name of the type to which the constructor belongs.
          -> Options -- ^ Encoding options.
          -> ConstructorInfo -- ^ Constructor for which to generate JSON parsing code.
          -> Either (String, Name) Name -- ^ Left (valFieldName, objName) or
                                        --   Right valName
          -> Q Exp
-- Nullary constructors.
parseArgs _ _ _ _
  ConstructorInfo { constructorName    = conName
                  , constructorVariant = NormalConstructor
                  , constructorFields  = [] }
  (Left _) =
    [|pure|] `appE` conE conName
parseArgs _ _ tName _
  ConstructorInfo { constructorName    = conName
                  , constructorVariant = NormalConstructor
                  , constructorFields  = [] }
  (Right valName) =
    caseE (varE valName) $ parseNullaryMatches tName conName

-- Unary constructors.
parseArgs jc tvMap _ _
  ConstructorInfo { constructorName    = conName
                  , constructorVariant = NormalConstructor
                  , constructorFields  = [argTy] }
  contents = do
    argTy' <- resolveTypeSynonyms argTy
    matchCases contents $ parseUnaryMatches jc tvMap argTy' conName

-- Polyadic constructors.
parseArgs jc tvMap tName _
  ConstructorInfo { constructorName    = conName
                  , constructorVariant = NormalConstructor
                  , constructorFields  = argTys }
  contents = do
    argTys' <- mapM resolveTypeSynonyms argTys
    let len = genericLength argTys'
    matchCases contents $ parseProduct jc tvMap argTys' tName conName len

-- Records.
parseArgs jc tvMap tName opts
  ConstructorInfo { constructorName    = conName
                  , constructorVariant = RecordConstructor fields
                  , constructorFields  = argTys }
  (Left (_, obj)) = do
    argTys' <- mapM resolveTypeSynonyms argTys
    parseRecord jc tvMap argTys' opts tName conName fields obj True
parseArgs jc tvMap tName opts
  info@ConstructorInfo { constructorName    = conName
                       , constructorVariant = RecordConstructor fields
                       , constructorFields  = argTys }
  (Right valName) =
    case (unwrapUnaryRecords opts,argTys) of
      (True,[_])-> parseArgs jc tvMap tName opts
                             (info{constructorVariant = NormalConstructor})
                             (Right valName)
      _ -> do
        obj <- newName "recObj"
        argTys' <- mapM resolveTypeSynonyms argTys
        caseE (varE valName)
          [ match (conP 'Object [varP obj]) (normalB $
              parseRecord jc tvMap argTys' opts tName conName fields obj False) []
          , matchFailed tName conName "Object"
          ]

-- Infix constructors. Apart from syntax these are the same as
-- polyadic constructors.
parseArgs jc tvMap tName _
  ConstructorInfo { constructorName    = conName
                  , constructorVariant = InfixConstructor
                  , constructorFields  = argTys }
  contents = do
    argTys' <- mapM resolveTypeSynonyms argTys
    matchCases contents $ parseProduct jc tvMap argTys' tName conName 2

-- | Generates code to parse the JSON encoding of an n-ary
-- constructor.
parseProduct :: JSONClass -- ^ The FromJSON variant being derived.
             -> TyVarMap -- ^ Maps the last type variables to their decoding
                         --   function arguments.
             -> [Type] -- ^ The argument types of the constructor.
             -> Name -- ^ Name of the type to which the constructor belongs.
             -> Name -- ^ 'Con'structor name.
             -> Integer -- ^ 'Con'structor arity.
             -> [Q Match]
parseProduct jc tvMap argTys tName conName numArgs =
    [ do arr <- newName "arr"
         -- List of: "parseJSON (arr `V.unsafeIndex` <IX>)"
         let x:xs = [ dispatchParseJSON jc conName tvMap argTy
                      `appE`
                      infixApp (varE arr)
                               [|V.unsafeIndex|]
                               (litE $ integerL ix)
                    | (argTy, ix) <- zip argTys [0 .. numArgs - 1]
                    ]
         match (conP 'Array [varP arr])
               (normalB $ condE ( infixApp ([|V.length|] `appE` varE arr)
                                           [|(==)|]
                                           (litE $ integerL numArgs)
                                )
                                ( foldl' (\a b -> infixApp a [|(<*>)|] b)
                                         (infixApp (conE conName) [|(<$>)|] x)
                                         xs
                                )
                                ( parseTypeMismatch tName conName
                                    (litE $ stringL $ "Array of length " ++ show numArgs)
                                    ( infixApp (litE $ stringL "Array of length ")
                                               [|(++)|]
                                               ([|show . V.length|] `appE` varE arr)
                                    )
                                )
               )
               []
    , matchFailed tName conName "Array"
    ]

--------------------------------------------------------------------------------
-- Parsing errors
--------------------------------------------------------------------------------

matchFailed :: Name -> Name -> String -> MatchQ
matchFailed tName conName expected = do
  other <- newName "other"
  match (varP other)
        ( normalB $ parseTypeMismatch tName conName
                      (litE $ stringL expected)
                      ([|valueConName|] `appE` varE other)
        )
        []

parseTypeMismatch :: Name -> Name -> ExpQ -> ExpQ -> ExpQ
parseTypeMismatch tName conName expected actual =
    foldl appE
          [|parseTypeMismatch'|]
          [ litE $ stringL $ nameBase conName
          , litE $ stringL $ show tName
          , expected
          , actual
          ]

class LookupField a where
    lookupField :: (Value -> Parser a) -> String -> String
                -> Object -> T.Text -> Parser a

instance OVERLAPPABLE_ LookupField a where
    lookupField = lookupFieldWith

instance INCOHERENT_ LookupField (Maybe a) where
    lookupField pj _ _ = parseOptionalFieldWith pj

instance INCOHERENT_ LookupField (Semigroup.Option a) where
    lookupField pj tName rec obj key =
        fmap Semigroup.Option
             (lookupField (fmap Semigroup.getOption . pj) tName rec obj key)

lookupFieldWith :: (Value -> Parser a) -> String -> String
                -> Object -> T.Text -> Parser a
lookupFieldWith pj tName rec obj key =
    case H.lookup key obj of
      Nothing -> unknownFieldFail tName rec (T.unpack key)
      Just v  -> pj v <?> Key key

unknownFieldFail :: String -> String -> String -> Parser fail
unknownFieldFail tName rec key =
    fail $ printf "When parsing the record %s of type %s the key %s was not present."
                  rec tName key

noArrayFail :: String -> String -> Parser fail
noArrayFail t o = fail $ printf "When parsing %s expected Array but got %s." t o

noObjectFail :: String -> String -> Parser fail
noObjectFail t o = fail $ printf "When parsing %s expected Object but got %s." t o

firstElemNoStringFail :: String -> String -> Parser fail
firstElemNoStringFail t o = fail $ printf "When parsing %s expected an Array of 2 elements where the first element is a String but got %s at the first element." t o

wrongPairCountFail :: String -> String -> Parser fail
wrongPairCountFail t n =
    fail $ printf "When parsing %s expected an Object with a single tag/contents pair but got %s pairs."
                  t n

noStringFail :: String -> String -> Parser fail
noStringFail t o = fail $ printf "When parsing %s expected String but got %s." t o

noMatchFail :: String -> String -> Parser fail
noMatchFail t o =
    fail $ printf "When parsing %s expected a String with the tag of a constructor but got %s." t o

not2ElemArray :: String -> Int -> Parser fail
not2ElemArray t i = fail $ printf "When parsing %s expected an Array of 2 elements but got %i elements" t i

conNotFoundFail2ElemArray :: String -> [String] -> String -> Parser fail
conNotFoundFail2ElemArray t cs o =
    fail $ printf "When parsing %s expected a 2-element Array with a tag and contents element where the tag is one of [%s], but got %s."
                  t (intercalate ", " cs) o

conNotFoundFailObjectSingleField :: String -> [String] -> String -> Parser fail
conNotFoundFailObjectSingleField t cs o =
    fail $ printf "When parsing %s expected an Object with a single tag/contents pair where the tag is one of [%s], but got %s."
                  t (intercalate ", " cs) o

conNotFoundFailTaggedObject :: String -> [String] -> String -> Parser fail
conNotFoundFailTaggedObject t cs o =
    fail $ printf "When parsing %s expected an Object with a tag field where the value is one of [%s], but got %s."
                  t (intercalate ", " cs) o

parseTypeMismatch' :: String -> String -> String -> String -> Parser fail
parseTypeMismatch' conName tName expected actual =
    fail $ printf "When parsing the constructor %s of type %s expected %s but got %s."
                  conName tName expected actual

--------------------------------------------------------------------------------
-- Shared ToJSON and FromJSON code
--------------------------------------------------------------------------------

-- | Functionality common to 'deriveJSON', 'deriveJSON1', and 'deriveJSON2'.
deriveJSONBoth :: (Options -> Name -> Q [Dec])
               -- ^ Function which derives a flavor of 'ToJSON'.
               -> (Options -> Name -> Q [Dec])
               -- ^ Function which derives a flavor of 'FromJSON'.
               -> Options
               -- ^ Encoding options.
               -> Name
               -- ^ Name of the type for which to generate 'ToJSON' and 'FromJSON'
               -- instances.
               -> Q [Dec]
deriveJSONBoth dtj dfj opts name =
    liftM2 (++) (dtj opts name) (dfj opts name)

-- | Functionality common to @deriveToJSON(1)(2)@ and @deriveFromJSON(1)(2)@.
deriveJSONClass :: [(JSONFun, JSONClass -> Name -> Options -> [Type]
                                        -> [ConstructorInfo] -> Q Exp)]
                -- ^ The class methods and the functions which derive them.
                -> JSONClass
                -- ^ The class for which to generate an instance.
                -> Options
                -- ^ Encoding options.
                -> Name
                -- ^ Name of the type for which to generate a class instance
                -- declaration.
                -> Q [Dec]
deriveJSONClass consFuns jc opts name = do
  info <- reifyDatatype name
  case info of
    DatatypeInfo { datatypeContext   = ctxt
                 , datatypeName      = parentName
#if MIN_VERSION_th_abstraction(0,3,0)
                 , datatypeInstTypes = instTys
#else
                 , datatypeVars      = instTys
#endif
                 , datatypeVariant   = variant
                 , datatypeCons      = cons
                 } -> do
      (instanceCxt, instanceType)
        <- buildTypeInstance parentName jc ctxt instTys variant
      (:[]) <$> instanceD (return instanceCxt)
                          (return instanceType)
                          (methodDecs parentName instTys cons)
  where
    methodDecs :: Name -> [Type] -> [ConstructorInfo] -> [Q Dec]
    methodDecs parentName instTys cons = flip map consFuns $ \(jf, jfMaker) ->
      funD (jsonFunValName jf (arity jc))
           [ clause []
                    (normalB $ jfMaker jc parentName opts instTys cons)
                    []
           ]

mkFunCommon :: (JSONClass -> Name -> Options -> [Type] -> [ConstructorInfo] -> Q Exp)
            -- ^ The function which derives the expression.
            -> JSONClass
            -- ^ Which class's method is being derived.
            -> Options
            -- ^ Encoding options.
            -> Name
            -- ^ Name of the encoded type.
            -> Q Exp
mkFunCommon consFun jc opts name = do
  info <- reifyDatatype name
  case info of
    DatatypeInfo { datatypeContext   = ctxt
                 , datatypeName      = parentName
#if MIN_VERSION_th_abstraction(0,3,0)
                 , datatypeInstTypes = instTys
#else
                 , datatypeVars      = instTys
#endif
                 , datatypeVariant   = variant
                 , datatypeCons      = cons
                 } -> do
      -- We force buildTypeInstance here since it performs some checks for whether
      -- or not the provided datatype's kind matches the derived method's
      -- typeclass, and produces errors if it can't.
      !_ <- buildTypeInstance parentName jc ctxt instTys variant
      consFun jc parentName opts instTys cons

dispatchFunByType :: JSONClass
                  -> JSONFun
                  -> Name
                  -> TyVarMap
                  -> Bool -- True if we are using the function argument that works
                          -- on lists (e.g., [a] -> Value). False is we are using
                          -- the function argument that works on single values
                          -- (e.g., a -> Value).
                  -> Type
                  -> Q Exp
dispatchFunByType _ jf _ tvMap list (VarT tyName) =
    varE $ case M.lookup tyName tvMap of
                Just (tfjExp, tfjlExp) -> if list then tfjlExp else tfjExp
                Nothing                -> jsonFunValOrListName list jf Arity0
dispatchFunByType jc jf conName tvMap list (SigT ty _) =
    dispatchFunByType jc jf conName tvMap list ty
dispatchFunByType jc jf conName tvMap list (ForallT _ _ ty) =
    dispatchFunByType jc jf conName tvMap list ty
dispatchFunByType jc jf conName tvMap list ty = do
    let tyCon :: Type
        tyArgs :: [Type]
        tyCon :| tyArgs = unapplyTy ty

        numLastArgs :: Int
        numLastArgs = min (arityInt jc) (length tyArgs)

        lhsArgs, rhsArgs :: [Type]
        (lhsArgs, rhsArgs) = splitAt (length tyArgs - numLastArgs) tyArgs

        tyVarNames :: [Name]
        tyVarNames = M.keys tvMap

    itf <- isInTypeFamilyApp tyVarNames tyCon tyArgs
    if any (`mentionsName` tyVarNames) lhsArgs || itf
       then outOfPlaceTyVarError jc conName
       else if any (`mentionsName` tyVarNames) rhsArgs
            then appsE $ varE (jsonFunValOrListName list jf $ toEnum numLastArgs)
                         : zipWith (dispatchFunByType jc jf conName tvMap)
                                   (cycle [False,True])
                                   (interleave rhsArgs rhsArgs)
            else varE $ jsonFunValOrListName list jf Arity0

dispatchToJSON
  :: ToJSONFun -> JSONClass -> Name -> TyVarMap -> Type -> Q Exp
dispatchToJSON target jc n tvMap =
    dispatchFunByType jc (targetToJSONFun target) n tvMap False

dispatchParseJSON
  :: JSONClass -> Name -> TyVarMap -> Type -> Q Exp
dispatchParseJSON  jc n tvMap = dispatchFunByType jc ParseJSON  n tvMap False

--------------------------------------------------------------------------------
-- Utility functions
--------------------------------------------------------------------------------

-- For the given Types, generate an instance context and head.
buildTypeInstance :: Name
                  -- ^ The type constructor or data family name
                  -> JSONClass
                  -- ^ The typeclass to derive
                  -> Cxt
                  -- ^ The datatype context
                  -> [Type]
                  -- ^ The types to instantiate the instance with
                  -> DatatypeVariant
                  -- ^ Are we dealing with a data family instance or not
                  -> Q (Cxt, Type)
buildTypeInstance tyConName jc dataCxt varTysOrig variant = do
    -- Make sure to expand through type/kind synonyms! Otherwise, the
    -- eta-reduction check might get tripped up over type variables in a
    -- synonym that are actually dropped.
    -- (See GHC Trac #11416 for a scenario where this actually happened.)
    varTysExp <- mapM resolveTypeSynonyms varTysOrig

    let remainingLength :: Int
        remainingLength = length varTysOrig - arityInt jc

        droppedTysExp :: [Type]
        droppedTysExp = drop remainingLength varTysExp

        droppedStarKindStati :: [StarKindStatus]
        droppedStarKindStati = map canRealizeKindStar droppedTysExp

    -- Check there are enough types to drop and that all of them are either of
    -- kind * or kind k (for some kind variable k). If not, throw an error.
    when (remainingLength < 0 || elem NotKindStar droppedStarKindStati) $
      derivingKindError jc tyConName

    let droppedKindVarNames :: [Name]
        droppedKindVarNames = catKindVarNames droppedStarKindStati

        -- Substitute kind * for any dropped kind variables
        varTysExpSubst :: [Type]
        varTysExpSubst = map (substNamesWithKindStar droppedKindVarNames) varTysExp

        remainingTysExpSubst, droppedTysExpSubst :: [Type]
        (remainingTysExpSubst, droppedTysExpSubst) =
          splitAt remainingLength varTysExpSubst

        -- All of the type variables mentioned in the dropped types
        -- (post-synonym expansion)
        droppedTyVarNames :: [Name]
        droppedTyVarNames = freeVariables droppedTysExpSubst

    -- If any of the dropped types were polykinded, ensure that they are of kind *
    -- after substituting * for the dropped kind variables. If not, throw an error.
    unless (all hasKindStar droppedTysExpSubst) $
      derivingKindError jc tyConName

    let preds    :: [Maybe Pred]
        kvNames  :: [[Name]]
        kvNames' :: [Name]
        -- Derive instance constraints (and any kind variables which are specialized
        -- to * in those constraints)
        (preds, kvNames) = unzip $ map (deriveConstraint jc) remainingTysExpSubst
        kvNames' = concat kvNames

        -- Substitute the kind variables specialized in the constraints with *
        remainingTysExpSubst' :: [Type]
        remainingTysExpSubst' =
          map (substNamesWithKindStar kvNames') remainingTysExpSubst

        -- We now substitute all of the specialized-to-* kind variable names with
        -- *, but in the original types, not the synonym-expanded types. The reason
        -- we do this is a superficial one: we want the derived instance to resemble
        -- the datatype written in source code as closely as possible. For example,
        -- for the following data family instance:
        --
        --   data family Fam a
        --   newtype instance Fam String = Fam String
        --
        -- We'd want to generate the instance:
        --
        --   instance C (Fam String)
        --
        -- Not:
        --
        --   instance C (Fam [Char])
        remainingTysOrigSubst :: [Type]
        remainingTysOrigSubst =
          map (substNamesWithKindStar (droppedKindVarNames `union` kvNames'))
            $ take remainingLength varTysOrig

        isDataFamily :: Bool
        isDataFamily = case variant of
                         Datatype        -> False
                         Newtype         -> False
                         DataInstance    -> True
                         NewtypeInstance -> True

        remainingTysOrigSubst' :: [Type]
        -- See Note [Kind signatures in derived instances] for an explanation
        -- of the isDataFamily check.
        remainingTysOrigSubst' =
          if isDataFamily
             then remainingTysOrigSubst
             else map unSigT remainingTysOrigSubst

        instanceCxt :: Cxt
        instanceCxt = catMaybes preds

        instanceType :: Type
        instanceType = AppT (ConT $ jsonClassName jc)
                     $ applyTyCon tyConName remainingTysOrigSubst'

    -- If the datatype context mentions any of the dropped type variables,
    -- we can't derive an instance, so throw an error.
    when (any (`predMentionsName` droppedTyVarNames) dataCxt) $
      datatypeContextError tyConName instanceType
    -- Also ensure the dropped types can be safely eta-reduced. Otherwise,
    -- throw an error.
    unless (canEtaReduce remainingTysExpSubst' droppedTysExpSubst) $
      etaReductionError instanceType
    return (instanceCxt, instanceType)

-- | Attempt to derive a constraint on a Type. If successful, return
-- Just the constraint and any kind variable names constrained to *.
-- Otherwise, return Nothing and the empty list.
--
-- See Note [Type inference in derived instances] for the heuristics used to
-- come up with constraints.
deriveConstraint :: JSONClass -> Type -> (Maybe Pred, [Name])
deriveConstraint jc t
  | not (isTyVar t) = (Nothing, [])
  | hasKindStar t   = (Just (applyCon (jcConstraint Arity0) tName), [])
  | otherwise = case hasKindVarChain 1 t of
      Just ns | jcArity >= Arity1
              -> (Just (applyCon (jcConstraint Arity1) tName), ns)
      _ -> case hasKindVarChain 2 t of
           Just ns | jcArity == Arity2
                   -> (Just (applyCon (jcConstraint Arity2) tName), ns)
           _ -> (Nothing, [])
  where
    tName :: Name
    tName = varTToName t

    jcArity :: Arity
    jcArity = arity jc

    jcConstraint :: Arity -> Name
    jcConstraint = jsonClassName . JSONClass (direction jc)

{-
Note [Kind signatures in derived instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

It is possible to put explicit kind signatures into the derived instances, e.g.,

  instance C a => C (Data (f :: * -> *)) where ...

But it is preferable to avoid this if possible. If we come up with an incorrect
kind signature (which is entirely possible, since Template Haskell doesn't always
have the best track record with reifying kind signatures), then GHC will flat-out
reject the instance, which is quite unfortunate.

Plain old datatypes have the advantage that you can avoid using any kind signatures
at all in their instances. This is because a datatype declaration uses all type
variables, so the types that we use in a derived instance uniquely determine their
kinds. As long as we plug in the right types, the kind inferencer can do the rest
of the work. For this reason, we use unSigT to remove all kind signatures before
splicing in the instance context and head.

Data family instances are trickier, since a data family can have two instances that
are distinguished by kind alone, e.g.,

  data family Fam (a :: k)
  data instance Fam (a :: * -> *)
  data instance Fam (a :: *)

If we dropped the kind signatures for C (Fam a), then GHC will have no way of
knowing which instance we are talking about. To avoid this scenario, we always
include explicit kind signatures in data family instances. There is a chance that
the inferred kind signatures will be incorrect, but if so, we can always fall back
on the mk- functions.

Note [Type inference in derived instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Type inference is can be tricky to get right, and we want to avoid recreating the
entirety of GHC's type inferencer in Template Haskell. For this reason, we will
probably never come up with derived instance contexts that are as accurate as
GHC's. But that doesn't mean we can't do anything! There are a couple of simple
things we can do to make instance contexts that work for 80% of use cases:

1. If one of the last type parameters is polykinded, then its kind will be
   specialized to * in the derived instance. We note what kind variable the type
   parameter had and substitute it with * in the other types as well. For example,
   imagine you had

     data Data (a :: k) (b :: k)

   Then you'd want to derived instance to be:

     instance C (Data (a :: *))

   Not:

     instance C (Data (a :: k))

2. We naïvely come up with instance constraints using the following criteria:

   (i)   If there's a type parameter n of kind *, generate a ToJSON n/FromJSON n
         constraint.
   (ii)  If there's a type parameter n of kind k1 -> k2 (where k1/k2 are * or kind
         variables), then generate a ToJSON1 n/FromJSON1 n constraint, and if
         k1/k2 are kind variables, then substitute k1/k2 with * elsewhere in the
         types. We must consider the case where they are kind variables because
         you might have a scenario like this:

           newtype Compose (f :: k2 -> *) (g :: k1 -> k2) (a :: k1)
             = Compose (f (g a))

         Which would have a derived ToJSON1 instance of:

           instance (ToJSON1 f, ToJSON1 g) => ToJSON1 (Compose f g) where ...
   (iii) If there's a type parameter n of kind k1 -> k2 -> k3 (where k1/k2/k3 are
         * or kind variables), then generate a ToJSON2 n/FromJSON2 n constraint
         and perform kind substitution as in the other cases.
-}

checkExistentialContext :: JSONClass -> TyVarMap -> Cxt -> Name
                        -> Q a -> Q a
checkExistentialContext jc tvMap ctxt conName q =
  if (any (`predMentionsName` M.keys tvMap) ctxt
       || M.size tvMap < arityInt jc)
       && not (allowExQuant jc)
     then existentialContextError conName
     else q

{-
Note [Matching functions with GADT type variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When deriving ToJSON2, there is a tricky corner case to consider:

  data Both a b where
    BothCon :: x -> x -> Both x x

Which encoding functions should be applied to which arguments of BothCon?
We have a choice, since both the function of type (a -> Value) and of type
(b -> Value) can be applied to either argument. In such a scenario, the
second encoding function takes precedence over the first encoding function, so the
derived ToJSON2 instance would be something like:

  instance ToJSON2 Both where
    liftToJSON2 tj1 tj2 p (BothCon x1 x2) = Array $ create $ do
      mv <- unsafeNew 2
      unsafeWrite mv 0 (tj1 x1)
      unsafeWrite mv 1 (tj2 x2)
      return mv

This is not an arbitrary choice, as this definition ensures that
liftToJSON2 toJSON = liftToJSON for a derived ToJSON1 instance for
Both.
-}

-- A mapping of type variable Names to their encoding/decoding function Names.
-- For example, in a ToJSON2 declaration, a TyVarMap might look like
--
-- { a ~> (tj1, tjl1)
-- , b ~> (tj2, tjl2) }
--
-- where a and b are the last two type variables of the datatype, tj1 and tjl1 are
-- the function arguments of types (a -> Value) and ([a] -> Value), and tj2 and tjl2
-- are the function arguments of types (b -> Value) and ([b] -> Value).
type TyVarMap = Map Name (Name, Name)

-- | Returns True if a Type has kind *.
hasKindStar :: Type -> Bool
hasKindStar VarT{}         = True
hasKindStar (SigT _ StarT) = True
hasKindStar _              = False

-- Returns True is a kind is equal to *, or if it is a kind variable.
isStarOrVar :: Kind -> Bool
isStarOrVar StarT  = True
isStarOrVar VarT{} = True
isStarOrVar _      = False

-- Generate a list of fresh names with a common prefix, and numbered suffixes.
newNameList :: String -> Int -> Q [Name]
newNameList prefix len = mapM newName [prefix ++ show n | n <- [1..len]]

-- | @hasKindVarChain n kind@ Checks if @kind@ is of the form
-- k_0 -> k_1 -> ... -> k_(n-1), where k0, k1, ..., and k_(n-1) can be * or
-- kind variables.
hasKindVarChain :: Int -> Type -> Maybe [Name]
hasKindVarChain kindArrows t =
  let uk = uncurryKind (tyKind t)
  in if (NE.length uk - 1 == kindArrows) && F.all isStarOrVar uk
        then Just (concatMap freeVariables uk)
        else Nothing

-- | If a Type is a SigT, returns its kind signature. Otherwise, return *.
tyKind :: Type -> Kind
tyKind (SigT _ k) = k
tyKind _          = starK

-- | Extract Just the Name from a type variable. If the argument Type is not a
-- type variable, return Nothing.
varTToNameMaybe :: Type -> Maybe Name
varTToNameMaybe (VarT n)   = Just n
varTToNameMaybe (SigT t _) = varTToNameMaybe t
varTToNameMaybe _          = Nothing

-- | Extract the Name from a type variable. If the argument Type is not a
-- type variable, throw an error.
varTToName :: Type -> Name
varTToName = fromMaybe (error "Not a type variable!") . varTToNameMaybe

interleave :: [a] -> [a] -> [a]
interleave (a1:a1s) (a2:a2s) = a1:a2:interleave a1s a2s
interleave _        _        = []

-- | Fully applies a type constructor to its type variables.
applyTyCon :: Name -> [Type] -> Type
applyTyCon = foldl' AppT . ConT

-- | Is the given type a variable?
isTyVar :: Type -> Bool
isTyVar (VarT _)   = True
isTyVar (SigT t _) = isTyVar t
isTyVar _          = False

-- | Detect if a Name in a list of provided Names occurs as an argument to some
-- type family. This makes an effort to exclude /oversaturated/ arguments to
-- type families. For instance, if one declared the following type family:
--
-- @
-- type family F a :: Type -> Type
-- @
--
-- Then in the type @F a b@, we would consider @a@ to be an argument to @F@,
-- but not @b@.
isInTypeFamilyApp :: [Name] -> Type -> [Type] -> Q Bool
isInTypeFamilyApp names tyFun tyArgs =
  case tyFun of
    ConT tcName -> go tcName
    _           -> return False
  where
    go :: Name -> Q Bool
    go tcName = do
      info <- reify tcName
      case info of
#if MIN_VERSION_template_haskell(2,11,0)
        FamilyI (OpenTypeFamilyD (TypeFamilyHead _ bndrs _ _)) _
          -> withinFirstArgs bndrs
        FamilyI (ClosedTypeFamilyD (TypeFamilyHead _ bndrs _ _) _) _
          -> withinFirstArgs bndrs
#else
        FamilyI (FamilyD TypeFam _ bndrs _) _
          -> withinFirstArgs bndrs
        FamilyI (ClosedTypeFamilyD _ bndrs _ _) _
          -> withinFirstArgs bndrs
#endif
        _ -> return False
      where
        withinFirstArgs :: [a] -> Q Bool
        withinFirstArgs bndrs =
          let firstArgs = take (length bndrs) tyArgs
              argFVs    = freeVariables firstArgs
          in return $ any (`elem` argFVs) names

-- | Peel off a kind signature from a Type (if it has one).
unSigT :: Type -> Type
unSigT (SigT t _) = t
unSigT t          = t

-- | Are all of the items in a list (which have an ordering) distinct?
--
-- This uses Set (as opposed to nub) for better asymptotic time complexity.
allDistinct :: Ord a => [a] -> Bool
allDistinct = allDistinct' Set.empty
  where
    allDistinct' :: Ord a => Set a -> [a] -> Bool
    allDistinct' uniqs (x:xs)
        | x `Set.member` uniqs = False
        | otherwise            = allDistinct' (Set.insert x uniqs) xs
    allDistinct' _ _           = True

-- | Does the given type mention any of the Names in the list?
mentionsName :: Type -> [Name] -> Bool
mentionsName = go
  where
    go :: Type -> [Name] -> Bool
    go (AppT t1 t2) names = go t1 names || go t2 names
    go (SigT t _k)  names = go t names
                              || go _k names
    go (VarT n)     names = n `elem` names
    go _            _     = False

-- | Does an instance predicate mention any of the Names in the list?
predMentionsName :: Pred -> [Name] -> Bool
#if MIN_VERSION_template_haskell(2,10,0)
predMentionsName = mentionsName
#else
predMentionsName (ClassP n tys) names = n `elem` names || any (`mentionsName` names) tys
predMentionsName (EqualP t1 t2) names = mentionsName t1 names || mentionsName t2 names
#endif

-- | Split an applied type into its individual components. For example, this:
--
-- @
-- Either Int Char
-- @
--
-- would split to this:
--
-- @
-- [Either, Int, Char]
-- @
unapplyTy :: Type -> NonEmpty Type
unapplyTy = NE.reverse . go
  where
    go :: Type -> NonEmpty Type
    go (AppT t1 t2)    = t2 <| go t1
    go (SigT t _)      = go t
    go (ForallT _ _ t) = go t
    go t               = t :| []

-- | Split a type signature by the arrows on its spine. For example, this:
--
-- @
-- forall a b. (a ~ b) => (a -> b) -> Char -> ()
-- @
--
-- would split to this:
--
-- @
-- (a ~ b, [a -> b, Char, ()])
-- @
uncurryTy :: Type -> (Cxt, NonEmpty Type)
uncurryTy (AppT (AppT ArrowT t1) t2) =
  let (ctxt, tys) = uncurryTy t2
  in (ctxt, t1 <| tys)
uncurryTy (SigT t _) = uncurryTy t
uncurryTy (ForallT _ ctxt t) =
  let (ctxt', tys) = uncurryTy t
  in (ctxt ++ ctxt', tys)
uncurryTy t = ([], t :| [])

-- | Like uncurryType, except on a kind level.
uncurryKind :: Kind -> NonEmpty Kind
uncurryKind = snd . uncurryTy

createKindChain :: Int -> Kind
createKindChain = go starK
  where
    go :: Kind -> Int -> Kind
    go k 0 = k
    go k !n = go (AppT (AppT ArrowT StarT) k) (n - 1)

-- | Makes a string literal expression from a constructor's name.
conNameExp :: Options -> ConstructorInfo -> Q Exp
conNameExp opts = litE
                . stringL
                . constructorTagModifier opts
                . nameBase
                . constructorName

-- | Extracts a record field label.
fieldLabel :: Options -- ^ Encoding options
           -> Name
           -> String
fieldLabel opts = fieldLabelModifier opts . nameBase

-- | The name of the outermost 'Value' constructor.
valueConName :: Value -> String
valueConName (Object _) = "Object"
valueConName (Array  _) = "Array"
valueConName (String _) = "String"
valueConName (Number _) = "Number"
valueConName (Bool   _) = "Boolean"
valueConName Null       = "Null"

applyCon :: Name -> Name -> Pred
applyCon con t =
#if MIN_VERSION_template_haskell(2,10,0)
          AppT (ConT con) (VarT t)
#else
          ClassP con [VarT t]
#endif

-- | Checks to see if the last types in a data family instance can be safely eta-
-- reduced (i.e., dropped), given the other types. This checks for three conditions:
--
-- (1) All of the dropped types are type variables
-- (2) All of the dropped types are distinct
-- (3) None of the remaining types mention any of the dropped types
canEtaReduce :: [Type] -> [Type] -> Bool
canEtaReduce remaining dropped =
       all isTyVar dropped
    && allDistinct droppedNames -- Make sure not to pass something of type [Type], since Type
                                -- didn't have an Ord instance until template-haskell-2.10.0.0
    && not (any (`mentionsName` droppedNames) remaining)
  where
    droppedNames :: [Name]
    droppedNames = map varTToName dropped

-------------------------------------------------------------------------------
-- Expanding type synonyms
-------------------------------------------------------------------------------

applySubstitutionKind :: Map Name Kind -> Type -> Type
applySubstitutionKind = applySubstitution

substNameWithKind :: Name -> Kind -> Type -> Type
substNameWithKind n k = applySubstitutionKind (M.singleton n k)

substNamesWithKindStar :: [Name] -> Type -> Type
substNamesWithKindStar ns t = foldr' (`substNameWithKind` starK) t ns

-------------------------------------------------------------------------------
-- Error messages
-------------------------------------------------------------------------------

-- | Either the given data type doesn't have enough type variables, or one of
-- the type variables to be eta-reduced cannot realize kind *.
derivingKindError :: JSONClass -> Name -> Q a
derivingKindError jc tyConName = fail
  . showString "Cannot derive well-kinded instance of form ‘"
  . showString className
  . showChar ' '
  . showParen True
    ( showString (nameBase tyConName)
    . showString " ..."
    )
  . showString "‘\n\tClass "
  . showString className
  . showString " expects an argument of kind "
  . showString (pprint . createKindChain $ arityInt jc)
  $ ""
  where
    className :: String
    className = nameBase $ jsonClassName jc

-- | One of the last type variables cannot be eta-reduced (see the canEtaReduce
-- function for the criteria it would have to meet).
etaReductionError :: Type -> Q a
etaReductionError instanceType = fail $
    "Cannot eta-reduce to an instance of form \n\tinstance (...) => "
    ++ pprint instanceType

-- | The data type has a DatatypeContext which mentions one of the eta-reduced
-- type variables.
datatypeContextError :: Name -> Type -> Q a
datatypeContextError dataName instanceType = fail
    . showString "Can't make a derived instance of ‘"
    . showString (pprint instanceType)
    . showString "‘:\n\tData type ‘"
    . showString (nameBase dataName)
    . showString "‘ must not have a class context involving the last type argument(s)"
    $ ""

-- | The data type mentions one of the n eta-reduced type variables in a place other
-- than the last nth positions of a data type in a constructor's field.
outOfPlaceTyVarError :: JSONClass -> Name -> a
outOfPlaceTyVarError jc conName = error
    . showString "Constructor ‘"
    . showString (nameBase conName)
    . showString "‘ must only use its last "
    . shows n
    . showString " type variable(s) within the last "
    . shows n
    . showString " argument(s) of a data type"
    $ ""
  where
    n :: Int
    n = arityInt jc

-- | The data type has an existential constraint which mentions one of the
-- eta-reduced type variables.
existentialContextError :: Name -> a
existentialContextError conName = error
  . showString "Constructor ‘"
  . showString (nameBase conName)
  . showString "‘ must be truly polymorphic in the last argument(s) of the data type"
  $ ""

-------------------------------------------------------------------------------
-- Class-specific constants
-------------------------------------------------------------------------------

-- | A representation of the arity of the ToJSON/FromJSON typeclass being derived.
data Arity = Arity0 | Arity1 | Arity2
  deriving (Enum, Eq, Ord)

-- | Whether ToJSON(1)(2) or FromJSON(1)(2) is being derived.
data Direction = To | From

-- | A representation of which typeclass method is being spliced in.
data JSONFun = ToJSON | ToEncoding | ParseJSON

-- | A refinement of JSONFun to [ToJSON, ToEncoding].
data ToJSONFun = Value | Encoding

targetToJSONFun :: ToJSONFun -> JSONFun
targetToJSONFun Value = ToJSON
targetToJSONFun Encoding = ToEncoding

-- | A representation of which typeclass is being derived.
data JSONClass = JSONClass { direction :: Direction, arity :: Arity }

toJSONClass, toJSON1Class, toJSON2Class,
    fromJSONClass, fromJSON1Class, fromJSON2Class :: JSONClass
toJSONClass    = JSONClass To   Arity0
toJSON1Class   = JSONClass To   Arity1
toJSON2Class   = JSONClass To   Arity2
fromJSONClass  = JSONClass From Arity0
fromJSON1Class = JSONClass From Arity1
fromJSON2Class = JSONClass From Arity2

jsonClassName :: JSONClass -> Name
jsonClassName (JSONClass To   Arity0) = ''ToJSON
jsonClassName (JSONClass To   Arity1) = ''ToJSON1
jsonClassName (JSONClass To   Arity2) = ''ToJSON2
jsonClassName (JSONClass From Arity0) = ''FromJSON
jsonClassName (JSONClass From Arity1) = ''FromJSON1
jsonClassName (JSONClass From Arity2) = ''FromJSON2

jsonFunValName :: JSONFun -> Arity -> Name
jsonFunValName ToJSON     Arity0 = 'toJSON
jsonFunValName ToJSON     Arity1 = 'liftToJSON
jsonFunValName ToJSON     Arity2 = 'liftToJSON2
jsonFunValName ToEncoding Arity0 = 'toEncoding
jsonFunValName ToEncoding Arity1 = 'liftToEncoding
jsonFunValName ToEncoding Arity2 = 'liftToEncoding2
jsonFunValName ParseJSON  Arity0 = 'parseJSON
jsonFunValName ParseJSON  Arity1 = 'liftParseJSON
jsonFunValName ParseJSON  Arity2 = 'liftParseJSON2

jsonFunListName :: JSONFun -> Arity -> Name
jsonFunListName ToJSON     Arity0 = 'toJSONList
jsonFunListName ToJSON     Arity1 = 'liftToJSONList
jsonFunListName ToJSON     Arity2 = 'liftToJSONList2
jsonFunListName ToEncoding Arity0 = 'toEncodingList
jsonFunListName ToEncoding Arity1 = 'liftToEncodingList
jsonFunListName ToEncoding Arity2 = 'liftToEncodingList2
jsonFunListName ParseJSON  Arity0 = 'parseJSONList
jsonFunListName ParseJSON  Arity1 = 'liftParseJSONList
jsonFunListName ParseJSON  Arity2 = 'liftParseJSONList2

jsonFunValOrListName :: Bool -- e.g., toJSONList if True, toJSON if False
                     -> JSONFun -> Arity -> Name
jsonFunValOrListName False = jsonFunValName
jsonFunValOrListName True  = jsonFunListName

arityInt :: JSONClass -> Int
arityInt = fromEnum . arity

allowExQuant :: JSONClass -> Bool
allowExQuant (JSONClass To _) = True
allowExQuant _                = False

-------------------------------------------------------------------------------
-- StarKindStatus
-------------------------------------------------------------------------------

-- | Whether a type is not of kind *, is of kind *, or is a kind variable.
data StarKindStatus = NotKindStar
                    | KindStar
                    | IsKindVar Name
  deriving Eq

-- | Does a Type have kind * or k (for some kind variable k)?
canRealizeKindStar :: Type -> StarKindStatus
canRealizeKindStar t = case t of
    _ | hasKindStar t -> KindStar
    SigT _ (VarT k) -> IsKindVar k
    _ -> NotKindStar

-- | Returns 'Just' the kind variable 'Name' of a 'StarKindStatus' if it exists.
-- Otherwise, returns 'Nothing'.
starKindStatusToName :: StarKindStatus -> Maybe Name
starKindStatusToName (IsKindVar n) = Just n
starKindStatusToName _             = Nothing

-- | Concat together all of the StarKindStatuses that are IsKindVar and extract
-- the kind variables' Names out.
catKindVarNames :: [StarKindStatus] -> [Name]
catKindVarNames = mapMaybe starKindStatusToName
