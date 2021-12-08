{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}

-- |
-- Module      : Network.AWS.Data.Text
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Data.Text
    ( Text

    -- * Deserialisation
    , FromText (..)
    , fromText
    , fromTextError
    , takeLowerText

    -- * Serialisation
    , ToText   (..)
    ) where

import           Data.Attoparsec.Text              (Parser)
import qualified Data.Attoparsec.Text              as A (signed, rational, endOfInput, decimal, anyChar, takeText, parseOnly)
import           Data.ByteString                   (ByteString)
import           Data.Int
import           Data.Text                         (Text)
import qualified Data.Text                         as Text
import qualified Data.Text.Encoding                as Text
import qualified Data.Text.Lazy                    as LText
import           Data.Text.Lazy.Builder            (Builder)
import qualified Data.Text.Lazy.Builder            as Build
import qualified Data.Text.Lazy.Builder.Int        as Build
import           Numeric
import           Numeric.Natural

-- | Fail parsing with a 'Text' error.
--
-- Constrained to the actual attoparsec monad to avoid
-- exposing 'fail' usage directly.
fromTextError :: Text -> Parser a
fromTextError = fail . Text.unpack

fromText :: FromText a => Text -> Either String a
fromText = A.parseOnly parser

takeLowerText :: Parser Text
takeLowerText = Text.toLower <$> A.takeText

class FromText a where
    parser :: Parser a

instance FromText Text where
    parser = A.takeText

instance FromText String where
    parser = Text.unpack <$> A.takeText

instance FromText ByteString where
    parser = Text.encodeUtf8 <$> A.takeText

instance FromText Char where
    parser = A.anyChar <* A.endOfInput

instance FromText Int where
    parser = A.signed A.decimal <* A.endOfInput

instance FromText Int64 where
    parser = A.signed A.decimal <* A.endOfInput

instance FromText Integer where
    parser = A.signed A.decimal <* A.endOfInput

instance FromText Natural where
    parser = A.decimal <* A.endOfInput

instance FromText Double where
    parser = A.signed A.rational <* A.endOfInput

instance FromText Bool where
    parser = takeLowerText >>= \case
        "true"  -> pure True
        "false" -> pure False
        e       -> fromTextError $ "Failure parsing Bool from '" <> e <> "'."

class ToText a where
    toText :: a -> Text

instance ToText Text       where toText = id
instance ToText ByteString where toText = Text.decodeUtf8
instance ToText Char       where toText = Text.singleton
instance ToText String     where toText = Text.pack
instance ToText Int        where toText = shortText . Build.decimal
instance ToText Int64      where toText = shortText . Build.decimal
instance ToText Integer    where toText = shortText . Build.decimal
instance ToText Natural    where toText = shortText . Build.decimal
instance ToText Double     where toText = toText . ($ "") . showFFloat Nothing

instance ToText Bool where
    toText True  = "true"
    toText False = "false"

shortText :: Builder -> Text
shortText = LText.toStrict . Build.toLazyTextWith 32
