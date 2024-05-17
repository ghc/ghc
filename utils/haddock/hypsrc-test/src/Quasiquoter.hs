{-# LANGUAGE Haskell2010 #-}
module Quasiquoter ( string ) where

import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

-- | Quoter for constructing multiline string literals
string :: QuasiQuoter
string = QuasiQuoter
  { quoteExp = pure . LitE . StringL
  , quotePat = invalidDomain
  , quoteType = invalidDomain
  , quoteDec = invalidDomain
  }
  where
    invalidDomain :: String -> Q a
    invalidDomain _ = fail "stringQuoter: only valid in expression context"
