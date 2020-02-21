{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module T17722B (setHelper) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Void (Void)
import qualified Data.Foldable
import qualified Data.List
import qualified Data.Sequence
import qualified Data.Text

import T17722A

data Expr s a
  = App (Expr s a) (Expr s a)
  | List
  | ListLit (Maybe (Expr s a)) (Seq (Expr s a))

data Src

type Extractor s a = Validation (ExtractErrors s a)

typeError :: Expr s a -> Expr s a -> Extractor s a b
typeError expected actual =
    Failure . ExtractErrors . pure . TypeMismatch $ InvalidDecoder expected actual

extractError :: Text -> Extractor s a b
extractError = Failure . ExtractErrors . pure . ExtractError

newtype ExtractErrors s a = ExtractErrors (NonEmpty (ExtractError s a))
  deriving Semigroup

data ExtractError s a =
    TypeMismatch (InvalidDecoder s a)
  | ExtractError Text

data InvalidDecoder s a = InvalidDecoder (Expr s a) (Expr s a)

data Decoder a = Decoder
    (Expr Src Void -> Extractor Src Void a)
    (Expr Src Void)

setHelper :: (Eq a, Foldable t, Show a)
          => (t a -> Int)
          -> ([a] -> t a)
          -> Decoder a
          -> Decoder (t a)
setHelper size toSet (Decoder extractIn expectedIn) = Decoder extractOut expectedOut
  where
    extractOut (ListLit _ es) = case traverse extractIn es of
        Success vSeq
            | sameSize               -> Success vSet
            | otherwise              -> extractError err
          where
            vList = Data.Foldable.toList vSeq
            vSet = toSet vList
            sameSize = size vSet == Data.Sequence.length vSeq
            duplicates = vList Data.List.\\ Data.Foldable.toList vSet
            err | length duplicates == 1 =
                     "One duplicate element in the list: "
                     <> (Data.Text.pack $ show $ head duplicates)
                | otherwise              = Data.Text.pack $ unwords
                     [ show $ length duplicates
                     , "duplicates were found in the list, including"
                     , show $ head duplicates
                     ]
        Failure f -> Failure f
    extractOut expr = typeError expectedOut expr

    expectedOut = App List expectedIn
