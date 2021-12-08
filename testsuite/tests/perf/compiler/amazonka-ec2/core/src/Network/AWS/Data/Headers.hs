{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

-- |
-- Module      : Network.AWS.Data.Headers
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Data.Headers
    ( ToHeader(..)
    ) where

import qualified Data.Text.Encoding          as Text
import           Network.AWS.Data.ByteString
import           Network.AWS.Data.Text

type Header = (HeaderName, ByteString)

type HeaderName = ByteString

class ToHeader a where
    toHeader :: HeaderName -> a -> [Header]

    default toHeader :: ToText a => HeaderName -> a -> [Header]
    toHeader k = toHeader k . toText

instance ToHeader Text where
    toHeader k v = [(k, Text.encodeUtf8 v)]

instance ToHeader ByteString where
    toHeader k v = [(k, v)]

instance ToText a => ToHeader (Maybe a) where
    toHeader k = maybe [] (toHeader k . toText)
