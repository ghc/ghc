-- |
-- Module      : Network.AWS.Prelude
-- Copyright   : (c) 2013-2018 Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Prelude
    ( module Network.AWS.Prelude
    , module Export
    ) where

import Control.DeepSeq           as Export (NFData)
import Data.Data                 as Export (Data, Typeable)
import Data.List.NonEmpty        as Export (NonEmpty)
import Data.Maybe                (fromMaybe)
import GHC.Generics              as Export (Generic)

import Network.AWS.Data.Base64     as Export (_Base64, Base64)
import Network.AWS.Data.ByteString as Export (ByteString, ToByteString(..))
import Network.AWS.Data.Headers    as Export (ToHeader(..))
import Network.AWS.Data.List1      as Export (parseXMLList1, _List1, List1)
import Network.AWS.Data.Query      as Export (toQueryList, ToQuery(..), (=:))
import Network.AWS.Data.Text       as Export (fromTextError, takeLowerText, ToText(..), FromText(..), Text)
import Network.AWS.Data.Time       as Export (ISO8601, UTCTime, _Time)
import Network.AWS.Data.XML        as Export (parseXMLText, parseXMLList, FromXML(..), (.@), (.@?))
import Network.AWS.Types           as Export (_Coerce, _Default)

infixl 7 .!@

(.!@) :: Functor f => f (Maybe a) -> a -> f a
f .!@ x = fromMaybe x <$> f

may :: Applicative f => ([a] -> f b) -> [a] -> f (Maybe b)
may _ [] = pure Nothing
may f xs = Just <$> f xs
