{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Complex
  ( Type(..)
  , OpenComplex(..)
  , CloseComplex(..)
  , Complex(..)
  , closeComplex
  ) where

import Data.ByteString as B
import Data.ByteString.Short as BS
import Data.Typeable

data Type
  = OpenType
  | CloseType

data OpenComplex = OpenComplex
  { openComplexSource :: ByteString
  }

data CloseComplex = CloseComplex
  { closeComplexHash   :: ByteString
  , closeComplexSource :: !ByteString
  }

data Complex (t :: Type) = Complex
  { complexInner   :: !(ComplexFamily t)
  }

type family ComplexFamily (t :: Type) where
  ComplexFamily 'OpenType    = OpenComplex
  ComplexFamily 'CloseType = CloseComplex

handleComplex ::
     forall t a. Typeable t
  => Complex t
  -> (Complex 'CloseType -> a)
  -> a
handleComplex complex onClose =
  case toCloseComplex complex of
    Just receiveComplex -> onClose receiveComplex
    Nothing             -> undefined

toCloseComplex ::
     forall t. Typeable t
  => Complex t
  -> Maybe (Complex 'CloseType)
toCloseComplex x = fmap (\Refl -> x) (eqT :: Maybe (t :~: 'CloseType))

closeComplex :: Typeable t => Complex t -> Close
closeComplex complex =
  handleComplex
    complex
    receiveComplexToProtocolCloseComplex

receiveComplexToProtocolCloseComplex :: Complex 'CloseType -> Close
receiveComplexToProtocolCloseComplex Complex {complexInner = inner} =
  Close (hashToLink (closeComplexSource inner))

data Close = Close !ShortByteString

hashToLink :: ByteString -> ShortByteString
hashToLink bh = BS.toShort bh
