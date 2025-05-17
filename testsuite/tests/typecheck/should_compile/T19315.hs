{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Bug where

import Control.Monad.Reader
import Data.Kind

type Lens f s a = (f, s, a)

view :: MonadReader s m => Lens a s a -> m a
view = undefined

data TickLabels b n = TickLabels

type family N a :: Type
type instance N (TickLabels b (n :: Type)) = n

tickLabelTextFunction ::  Lens f a (QDiagram b (N a))
tickLabelTextFunction = undefined

class HasTickLabels f a b | a -> b where
  tickLabelFunction ::  Lens f a (N a -> String)

instance HasTickLabels f (TickLabels b n) b where
  tickLabelFunction = undefined

data QDiagram b n = QD

renderColourBar :: forall n b. TickLabels b n -> n -> ()
renderColourBar cbTickLabels bnds = ()
  where
  f :: a -> a
  f x = x

  tickLabelXs :: String
  tickLabelXs = view tickLabelFunction cbTickLabels bnds

  drawTickLabel :: n -> QDiagram b n
  drawTickLabel x = view tickLabelTextFunction cbTickLabels
     where v = f x
