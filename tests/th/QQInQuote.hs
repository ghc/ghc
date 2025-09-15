{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module QQInQuote where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

-- A trivial quasiquoter that returns a string literal expression
myQQ :: QuasiQuoter
myQQ = QuasiQuoter
  { quoteExp  = \s -> [| s |]
  , quotePat  = error "no pat"
  , quoteType = error "no type"
  , quoteDec  = error "no dec"
  }

-- An untyped quotation that embeds a quasiquoter directly
generatedExpr :: Q Exp
generatedExpr = [| putStrLn [myQQ|Hello from inside untyped quotation|] |]

