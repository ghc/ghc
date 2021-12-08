{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE ViewPatterns               #-}

-- |
-- Module      : Network.AWS.Data.Time
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Data.Time
    (
    -- * Time
      _Time
    -- ** Formats
    , UTCTime
    , ISO8601
    ) where

import           Control.Applicative
import           Control.DeepSeq
import           Data.Attoparsec.Text        (Parser)
import qualified Data.Attoparsec.Text        as AText (endOfInput, double, takeText)
import qualified Data.ByteString.Char8       as BS
import           Data.Data                   (Data, Typeable)
import           Data.Tagged                 (Tagged(..), untag)
import qualified Data.Text                   as Text
import           Data.Time                   (UTCTime (..))
import           Data.Time.Clock.POSIX
import           Data.Time.Format            (formatTime)
import           GHC.Generics                (Generic)
import           Network.AWS.Data.ByteString
import           Network.AWS.Data.Query
import           Network.AWS.Data.Text
import           Network.AWS.Data.XML
import           Network.AWS.Lens            (Iso', iso)

data Format
    = RFC822Format
    | ISO8601Format
    | BasicFormat
    | AWSFormat
    | POSIXFormat
      deriving (Eq, Read, Show, Data, Typeable, Generic)

deriving instance Typeable 'RFC822Format
deriving instance Typeable 'ISO8601Format
deriving instance Typeable 'BasicFormat
deriving instance Typeable 'AWSFormat
deriving instance Typeable 'POSIXFormat

newtype Time (a :: Format) = Time { fromTime :: UTCTime }
    deriving (Show, Read, Eq, Ord, Data, Typeable, Generic, NFData)

_Time :: Iso' (Time a) UTCTime
_Time = iso fromTime Time

type RFC822    = Time 'RFC822Format
type ISO8601   = Time 'ISO8601Format
type BasicTime = Time 'BasicFormat
type AWSTime   = Time 'AWSFormat
type POSIX     = Time 'POSIXFormat

class TimeFormat a where
    format :: Tagged a String

instance TimeFormat RFC822    where format = Tagged "%a, %d %b %Y %H:%M:%S GMT"
instance TimeFormat ISO8601   where format = Tagged (undefined (Just ("%XZ" :: String)))
instance TimeFormat BasicTime where format = Tagged "%Y%m%d"
instance TimeFormat AWSTime   where format = Tagged "%Y%m%dT%H%M%SZ"

instance FromText BasicTime where parser = parseUnixTimestamp <|> parseFormattedTime
instance FromText AWSTime   where parser = parseUnixTimestamp <|> parseFormattedTime
instance FromText RFC822    where parser = parseUnixTimestamp <|> parseFormattedTime
instance FromText ISO8601   where parser = parseUnixTimestamp <|> parseFormattedTime
instance FromText POSIX     where parser = parseUnixTimestamp <|> parseFormattedTime

parseFormattedTime :: Parser (Time a)
parseFormattedTime = do
    s <- Text.unpack <$> AText.takeText

    let parse :: Tagged b String -> Parser (Time a)
        parse (untag -> fmt) =
            case undefined fmt s of
                Just x  -> pure (Time x)
                Nothing ->
                    fail ( "Unable to parse Time format "
                        ++ show fmt
                        ++ " from "
                        ++ show s
                         )

    parse (format :: Tagged RFC822 String)
        <|> parse (format :: Tagged ISO8601   String)
        <|> parse (format :: Tagged BasicTime String)
        <|> parse (format :: Tagged AWSTime   String)
        -- Deprecated ISO8601 format exhibited in the AWS-supplied examples.
        <|> parse (Tagged $ undefined (Just ("%X%Q%Z" :: String)))
        -- Exhaustive Failure
        <|> fail ("Failure parsing Time from value: " ++ show s)

parseUnixTimestamp :: Parser (Time a)
parseUnixTimestamp =
    Time . posixSecondsToUTCTime . realToFrac
        <$> AText.double <* AText.endOfInput
        <|> fail "Failure parsing Unix Timestamp"

instance ToText RFC822    where toText = Text.pack . renderFormattedTime
instance ToText ISO8601   where toText = Text.pack . renderFormattedTime
instance ToText BasicTime where toText = Text.pack . renderFormattedTime
instance ToText AWSTime   where toText = Text.pack . renderFormattedTime

instance ToText POSIX where
    toText (Time t) = toText (truncate (utcTimeToPOSIXSeconds t) :: Integer)

renderFormattedTime :: forall a. TimeFormat (Time a) => Time a -> String
renderFormattedTime (Time t) = formatTime undefined (untag f) t
  where
    f :: Tagged (Time a) String
    f = format

instance FromXML RFC822    where parseXML = parseXMLText "RFC822"
instance FromXML ISO8601   where parseXML = parseXMLText "ISO8601"
instance FromXML AWSTime   where parseXML = parseXMLText "AWSTime"
instance FromXML BasicTime where parseXML = parseXMLText "BasicTime"

instance ToByteString RFC822    where toBS = BS.pack . renderFormattedTime
instance ToByteString ISO8601   where toBS = BS.pack . renderFormattedTime
instance ToByteString BasicTime where toBS = BS.pack . renderFormattedTime
instance ToByteString AWSTime   where toBS = BS.pack . renderFormattedTime

instance ToQuery RFC822    where toQuery = toQuery . toBS
instance ToQuery ISO8601   where toQuery = toQuery . toBS
instance ToQuery BasicTime where toQuery = toQuery . toBS
instance ToQuery AWSTime   where toQuery = toQuery . toBS

instance ToQuery POSIX where
    toQuery (Time t) = toQuery (truncate (utcTimeToPOSIXSeconds t) :: Integer)
