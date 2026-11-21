{-# LANGUAGE DataKinds, GADTs, NoExperimentalDependentTypes, TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module PreludeSpanProof (spanAcceptsHead, spanAcceptsSuffix, spanRejectsHead, rejoinSpan) where

import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import PreludeDropWhileSource (Rejoined)
import PreludeFilterAppendProof (FilterDecisions(..))
import PreludeFilterRuntime (BooleanResult(..))
import PreludeFilterSource (Negation)
import PreludeSpanSource (Span, SpanPrefix, SpanSuffix)

-- | The accepted head is available while the tail stays unknown.
spanAcceptsHead :: Proxy xs
                -> SpanPrefix Negation ('False ': xs) :~: ('False ': SpanPrefix Negation xs)
spanAcceptsHead _ = Refl

-- | An accepted head leaves the same suffix as the recursive split.
spanAcceptsSuffix :: Proxy xs -> SpanSuffix Negation ('False ': xs) :~: SpanSuffix Negation xs
spanAcceptsSuffix _ = Refl

-- | A rejected head returns the original cons in the second field.
spanRejectsHead :: Proxy xs -> SpanSuffix Negation ('True ': xs) :~: ('True ': xs)
spanRejectsHead _ = Refl

-- | Append the two lists from the actual split with the supplied decisions.
rejoinSpan :: FilterDecisions p xs -> Rejoined (Span p xs) :~: xs
rejoinSpan Done = Refl
rejoinSpan (Decide IsFalse _) = Refl
rejoinSpan (Decide IsTrue ds) = case rejoinSpan ds of
    Refl -> Refl
