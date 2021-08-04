{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:      Data.Aeson.Encode
-- Copyright:   (c) 2012-2016 Bryan O'Sullivan
--              (c) 2011 MailRank, Inc.
-- License:     BSD3
-- Maintainer:  Bryan O'Sullivan <bos@serpentine.com>
-- Stability:   experimental
-- Portability: portable
--
-- This module is left to supply limited backwards-compatibility.

module Data.Aeson.Encode {-# DEPRECATED "Use Data.Aeson or Data.Aeson.Text instead" #-}
    (
      encode
    , encodeToTextBuilder
    ) where


import Data.ByteString.Lazy (ByteString)
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Aeson as A
import qualified Data.Aeson.Text as A

encode :: A.ToJSON a => a -> ByteString
encode = A.encode
{-# DEPRECATED encode "Use encode from Data.Aeson" #-}

encodeToTextBuilder :: A.Value -> Builder
encodeToTextBuilder = A.encodeToTextBuilder
{-# DEPRECATED encodeToTextBuilder "Use encodeTotextBuilder from Data.Aeson.Text" #-}
