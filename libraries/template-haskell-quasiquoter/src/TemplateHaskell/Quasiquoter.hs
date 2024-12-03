{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP #-}

module TemplateHaskell.Quasiquoter
  ( Q
  , Exp
  , Pat
  , Type
  , Dec
  , QuasiQuoter (QuasiQuoter, quoteExp, quotePat, quoteType, quoteDec)
  )where

#if __GLASGOW_HASKELL__ >= 912
import GHC.Internal.TH.Quote
import GHC.Internal.TH.Syntax
#else
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
#endif
