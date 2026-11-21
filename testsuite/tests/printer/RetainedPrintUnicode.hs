{-# LANGUAGE ExperimentalDependentTypes #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE DataKinds, ExplicitForAll, GADTs, RankNTypes #-}
{-# LANGUAGE RequiredTypeArguments, TypeAbstractions #-}

module RetainedPrintUnicode where

import GHC.Exts (U)

keep ∷ foreach -- Retain this Boolean argument.
    (b ∷ Bool)
    → Select b → Select b
keep b x = x

identity ∷ ∀ a. {- Before metadata. -} @U a → a
identity x = x

visible ∷ ∀ a → @U a → a
visible (type a) x = x

data Ordered where
    Ordered ∷ ∀ a. @U {- Keep this comment after metadata. -}
              ∀ b. @U
              Show a ⇒ a → b → Ordered

type Wrap = \a {- Before the Unicode arrow. -} → Maybe a
