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
  , zeroHash
  , zeroAddress
  ) where

import Data.ByteString as B
import Data.ByteString.Short as BS
import Data.Typeable
import Data.Word

data Type
  = OpenType
  | CloseType

data OpenComplex = OpenComplex
  { openComplexSource :: !Hash
  , openComplexNumber :: !Word64
  }

data CloseComplex = CloseComplex
  { closeComplexHash   :: !Hash
  , closeComplexSource :: !Hash
  , closeComplexNumber :: !Word64
  }

data Complex (t :: Type) = Complex
  { complexAddress :: !Address
  , complexInner   :: !(ComplexFamily t)
  }

type family ComplexFamily (t :: Type) where
  ComplexFamily 'OpenType    = OpenComplex
  ComplexFamily 'CloseType = CloseComplex


handleComplex ::
     forall t a. Typeable t
  => Complex t
  -> (Complex 'OpenType -> a)
  -> (Complex 'CloseType -> a)
  -> a
handleComplex complex onOpen onClose =
  case toOpenComplex complex of
    Just openComplex -> onOpen openComplex
    Nothing ->
      case toCloseComplex complex of
        Just receiveComplex -> onClose receiveComplex
        Nothing             -> errorType

toOpenComplex ::
     forall t. Typeable t
  => Complex t
  -> Maybe (Complex 'OpenType)
toOpenComplex x = fmap (\Refl -> x) (eqT :: Maybe (t :~: 'OpenType))


toCloseComplex ::
     forall t. Typeable t
  => Complex t
  -> Maybe (Complex 'CloseType)
toCloseComplex x = fmap (\Refl -> x) (eqT :: Maybe (t :~: 'CloseType))


errorType :: a
errorType = error "Impossible: all types have been tried"

closeComplex :: Typeable t => Complex t -> Close
closeComplex complex =
  handleComplex
    complex
    openComplexToProtocolCloseComplex
    receiveComplexToProtocolCloseComplex


openComplexToProtocolCloseComplex :: Complex 'OpenType -> Close
openComplexToProtocolCloseComplex complex@Complex {complexInner = inner} =
  Close
  { closeAddress = complexAddress complex
  , closeNumber = openComplexNumber inner
  , closeLink = hashToLink (openComplexSource inner)
  }

receiveComplexToProtocolCloseComplex :: Complex 'CloseType -> Close
receiveComplexToProtocolCloseComplex complex@Complex {complexInner = inner} =
  Close
    { closeAddress = complexAddress complex
    , closeNumber = closeComplexNumber inner
    , closeLink = hashToLink (closeComplexSource inner)
    }

newtype Address = Address ShortByteString

zeroAddress :: Address
zeroAddress = Address BS.empty

data Close = Close
  { closeAddress :: !Address
  , closeNumber  :: !Word64
  , closeLink    :: !ShortByteString
  }

hashToLink :: Hash -> ShortByteString
hashToLink (Hash bh) = BS.toShort bh


zeroHash :: Hash
zeroHash = Hash B.empty


newtype Hash = Hash ByteString

