{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeData #-}

module GadtEmptyRequiredOff where

#if defined(CHECK_SOURCE)
data Leading where
  Leading :: forall -> Bool -> Leading

data Late where
  Late :: Bool -> (forall -> Late)
#elif defined(CHECK_QUOTE)
import Language.Haskell.TH (Dec, Q)

quoted :: Q [Dec]
quoted = [d| data Quoted where
               QuotedLeading :: forall -> Bool -> Quoted
               QuotedLate :: Bool -> (forall -> Quoted)
           |]
#elif defined(CHECK_SPLICE)
import Language.Haskell.TH

$( let parent = mkName "Spliced"
       con = RetainedGadtC [mkName "Spliced"] []
               (ForallVisT [] (AppT (AppT ArrowT (ConT ''Bool)) (ConT parent)))
   in pure [DataD [] parent [] Nothing [con] []] )
#elif defined(CHECK_TYPE_DATA)
type data TypeOnly a where
  TypeOnly :: forall a -> TypeOnly a
#elif defined(CHECK_TERM_REQUIRED)
required :: forall a -> a -> a
required t x = x :: t
#elif defined(CHECK_TERM_EMPTY)
ordinary :: forall -> Bool -> Bool
ordinary = id
#elif defined(CHECK_CAPTURE)
capture = not

implicitQuantification :: capture -> capture
implicitQuantification = id
#endif
