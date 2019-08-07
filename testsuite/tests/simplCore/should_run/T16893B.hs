{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Complex
  ( OpenComplex(..)
  , CloseComplex(..)
  , Complex(..)
  , closeComplex
  , badComplex
  ) where

import Data.ByteString as B
import Data.ByteString.Short as BS
import Data.Typeable

data OpenComplex = OpenComplex
  { openComplexSource :: !B.ByteString
  }

data CloseComplex = CloseComplex
  { closeComplexHash   :: !B.ByteString
  , closeComplexSource :: !B.ByteString
  }

data Complex (t :: Bool) = Complex
  { complexInner   :: !(ComplexFamily t)
  }

type family ComplexFamily (t :: Bool) where
  ComplexFamily 'True  = OpenComplex
  ComplexFamily 'False = CloseComplex


toOpenComplex ::
     forall t. Typeable t
  => Complex t
  -> Maybe (Complex 'True)
toOpenComplex x = fmap (\Refl -> x) (eqT :: Maybe (t :~: 'True))


toCloseComplex ::
     forall t. Typeable t
  => Complex t
  -> Maybe (Complex 'False)
toCloseComplex x = fmap (\Refl -> x) (eqT :: Maybe (t :~: 'False))


errorType :: a
errorType = error "Impossible: all types have been tried"

closeComplex :: Typeable t => Complex t -> ShortByteString
closeComplex complex =
  case toOpenComplex complex of
    Just openComplex -> BS.toShort (openComplexSource (complexInner openComplex))
    Nothing ->
      case toCloseComplex complex of
        Just closeComplex' -> BS.toShort (closeComplexSource (complexInner closeComplex'))
        Nothing            -> errorType


badComplex :: Complex 'True
badComplex =
  Complex
    { complexInner =
        OpenComplex
          { openComplexSource = B.empty
          }
    }

