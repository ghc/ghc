{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

-- |
-- Module      : Network.AWS.Data.List1
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Data.List1 (parseXMLList1, _List1, List1) where

import           Control.DeepSeq
import           Control.Monad
import           Data.Coerce
import           Data.Data            (Data, Typeable)
import           Data.List.NonEmpty   (NonEmpty (..))
import qualified Data.List.NonEmpty   as NonEmpty
import           Data.Text            (Text)
import           GHC.Exts
import           GHC.Generics         (Generic)
import           Network.AWS.Data.XML
import           Network.AWS.Lens     (Iso', iso)
import           Text.XML             (Node)

newtype List1 a = List1 { toNonEmpty :: NonEmpty a }
    deriving
        ( Functor
        , Monad
        , Applicative
        , Foldable
        , Traversable
        , Semigroup
        , Eq
        , Ord
        , Read
        , Show
        , Data
        , Typeable
        , Generic
        )

instance NFData a => NFData (List1 a)

_List1 :: (Coercible a b, Coercible b a) => Iso' (List1 a) (NonEmpty b)
_List1 = iso (coerce . toNonEmpty) (List1 . coerce)

instance IsList (List1 a) where
   type Item (List1 a) = a

   fromList = List1 . NonEmpty.fromList
   toList   = NonEmpty.toList . toNonEmpty

parseXMLList1 :: FromXML a
              => Text
              -> [Node]
              -> Either String (List1 a)
parseXMLList1 n = parseXMLList n >=> parse
  where
    parse xs =
       maybe (Left $ "Error parsing empty List1 when expecting at least one element: " ++ show n)
             (Right . List1)
             (NonEmpty.nonEmpty xs)
