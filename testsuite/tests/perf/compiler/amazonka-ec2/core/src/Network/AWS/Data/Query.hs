{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}

-- |
-- Module      : Network.AWS.Data.Query
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Data.Query (toQueryList, ToQuery(..), (=:)) where

import           Data.ByteString.Builder     (Builder)
import qualified Data.ByteString.Builder     as Build
import qualified Data.ByteString.Char8       as BS8
import qualified Data.ByteString.Lazy        as LBS
import           Data.Data
import           Data.List                   (sort)
import           Data.Maybe                  (fromMaybe)
import           Data.String
import qualified Data.Text.Encoding          as Text
import           GHC.Exts
import           Network.AWS.Data.ByteString
import           Network.AWS.Data.Text
import           Numeric.Natural

data QueryString
    = QList  [QueryString]
    | QPair  ByteString QueryString
    | QValue (Maybe ByteString)
      deriving (Eq, Show, Data, Typeable)

instance Semigroup QueryString where
  a <> b = case (a, b) of
    (QList l, QList r) -> QList (l ++ r)
    (QList l, r)       -> QList (r : l)
    (l,       QList r) -> QList (l : r)
    (l,       r)       -> QList [l, r]

instance Monoid QueryString where
    mempty = QList []
    mappend = (<>)

instance IsString QueryString where
    fromString = parseQueryString . fromString
    {-# INLINE fromString #-}

parseQueryString :: ByteString -> QueryString
parseQueryString bs
    | BS8.null bs = mempty
    | otherwise   =
        QList (map breakPair . filter (not . BS8.null) $ BS8.split '&' bs)
  where
    breakPair x =
        case BS8.break (== '=') x of
            ("", "") -> mempty
            ("", v)  -> stripValue v
            (k,  v)  -> QPair k (stripValue v)

    stripValue x =
        case x of
            ""  -> QValue Nothing
            "=" -> QValue Nothing
            _   -> QValue (Just (fromMaybe x (stripPrefix "=" x)))

stripPrefix :: ByteString -> ByteString -> Maybe ByteString
#if MIN_VERSION_bytestring(0,10,8)
stripPrefix = BS8.stripPrefix
#else
stripPrefix bs1 bs2
   | bs1 `BS8.isPrefixOf` bs2 = Just (BS8.drop (BS8.length bs1) bs2)
   | otherwise = Nothing
#endif

-- FIXME: use Builder
instance ToByteString QueryString where
    toBS = LBS.toStrict . Build.toLazyByteString . cat . sort . enc Nothing
      where
        enc :: Maybe ByteString -> QueryString -> [ByteString]
        enc p = \case
            QList xs          -> concatMap (enc p) xs

            QPair (undefined True -> k) x
                | Just n <- p -> enc (Just (n <> kdelim <> k)) x -- <prev>.key <recur>
                | otherwise   -> enc (Just k)                  x -- key <recur>

            QValue (Just (undefined True -> v))
                | Just n <- p -> [n <> vsep <> v] -- key=value
                | otherwise   -> [v <> vsep]      -- value= -- note: required for signing.

            _   | Just n <- p -> [n <> vsep]      -- key=
                                                  -- note: this case required for request signing
                | otherwise   -> []

        cat :: [ByteString] -> Builder
        cat []     = mempty
        cat [x]    = Build.byteString x
        cat (x:xs) = Build.byteString x <> ksep <> cat xs

        kdelim = "."
        ksep   = "&"
        vsep   = "="

infixr 7 =:

(=:) :: ToQuery a => ByteString -> a -> QueryString
k =: v = QPair k (toQuery v)

toQueryList :: (IsList a, ToQuery (Item a))
            => ByteString
            -> a
            -> QueryString
toQueryList k = QPair k . QList . zipWith f [1..] . toList
  where
    f :: ToQuery a => Int -> a -> QueryString
    f n v = toBS n =: toQuery v

class ToQuery a where
    toQuery :: a -> QueryString

    default toQuery :: ToText a => a -> QueryString
    toQuery = toQuery . toText

instance ToQuery QueryString where
    toQuery = id

instance (ToByteString k, ToQuery v) => ToQuery (k, v) where
    toQuery (k, v) = QPair (toBS k) (toQuery v)

instance ToQuery Char where
    toQuery = toQuery . BS8.singleton

instance ToQuery ByteString where
    toQuery "" = QValue Nothing
    toQuery bs = QValue (Just bs)

instance ToQuery Text    where toQuery = toQuery . Text.encodeUtf8
instance ToQuery Int     where toQuery = toQuery . toBS
instance ToQuery Integer where toQuery = toQuery . toBS
instance ToQuery Double  where toQuery = toQuery . toBS
instance ToQuery Natural where toQuery = toQuery . toBS

instance ToQuery a => ToQuery (Maybe a) where
    toQuery (Just x) = toQuery x
    toQuery Nothing  = mempty

instance ToQuery Bool where
    toQuery True  = toQuery ("true"  :: ByteString)
    toQuery False = toQuery ("false" :: ByteString)
