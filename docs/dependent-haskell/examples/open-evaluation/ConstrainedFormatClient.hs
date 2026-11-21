{-# LANGUAGE FlexibleContexts, FlexibleInstances, NoExperimentalDependentTypes #-}
{-# LANGUAGE QuantifiedConstraints, ScopedTypeVariables, TypeApplications, TypeFamilies, UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module ConstrainedFormatClient (renderedRawView) where

import ConstrainedElements (Allowed, FieldViews(..), FormatKey, Formatted, RenderFormat(..))
import ConstrainedElementsClient (ViewsAfter)
import Data.Proxy (Proxy(..))

-- Lists keep the same format. They require evidence for their elements.
instance {-# OVERLAPPING #-} Allowed a => RenderFormat (FormatKey [a]) where
    type FieldView (FormatKey [a]) detailed = Formatted [a] detailed

-- | Use the quantified format rule at the current element type.
renderRaw :: forall f a.
    ( Allowed a
    , forall x. Allowed x => RenderFormat (FormatKey (f x))
    , Show (f a)
    ) => Proxy f -> Proxy a -> ViewsAfter f a -> String
renderRaw _ _ (FieldViews raw _) = show raw

-- | Read the raw list without forcing its detailed view.
renderedRawView :: String
renderedRawView =
    renderRaw (Proxy @[]) (Proxy @Bool) (FieldViews [True, False] (error "forced detailed view"))
