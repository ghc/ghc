{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
{-# OPTIONS_GHC -frebindable-known-names #-}
module NativeClosedOtherEither where
import Data.Proxy (Proxy(..))
import Prelude hiding (Either)
import NativeClosedChoiceLib (Either)
import qualified NativeClosedChoiceLib as P (Either(First), otherEither)

bad :: Proxy (P.otherEither 0)
bad = Proxy
