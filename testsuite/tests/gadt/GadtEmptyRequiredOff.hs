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
#elif defined(CHECK_SPLICE_LEADING) || defined(CHECK_SPLICE_LATE) || defined(CHECK_SPLICE_STRUCTURED) || defined(CHECK_SPLICE_GADTC) || defined(CHECK_SPLICE_FIELD)
import Language.Haskell.TH

$( let parent = mkName "Spliced"
       names = [mkName "Spliced"]
       result = ConT parent
       boolArrow = AppT (AppT ArrowT (ConT ''Bool))
#if defined(CHECK_SPLICE_LEADING)
       con = RetainedGadtC names [] (ForallVisT [] (boolArrow result))
#elif defined(CHECK_SPLICE_LATE)
       con = RetainedGadtC names [] (boolArrow (ForallVisT [] result))
#elif defined(CHECK_SPLICE_STRUCTURED)
       con = RetainedGadtC names [GadtRequiredArgs (PromotedT (mkName "U")) []] result
#elif defined(CHECK_SPLICE_GADTC)
       con = GadtC names [] (ForallVisT [] result)
#else
       field = (Bang NoSourceUnpackedness NoSourceStrictness, ForallVisT [] (ConT ''Bool))
       con = GadtC names [field] result
#endif
   in pure [DataD [] parent [] Nothing [con] []] )
#elif defined(CHECK_TYPE_DATA)
type data TypeOnly a where
  MkTypeOnly :: forall a -> TypeOnly a
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
#elif defined(CHECK_RECORD_SOURCE)
data Record where
  Record :: {field :: Bool} -> forall -> Record
#elif defined(CHECK_RECORD_SPLICE)
import Language.Haskell.TH

$( let parent = mkName "Record"
       field = (mkName "field", Bang NoSourceUnpackedness NoSourceStrictness, ConT ''Bool)
       con = RecGadtC [mkName "Record"] [field] (ForallVisT [] (ConT parent))
   in pure [DataD [] parent [] Nothing [con] []] )
#endif
