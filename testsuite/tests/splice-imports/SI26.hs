{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ExplicitLevelImports #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module SI26 where

-- Test using 'quote' as a post-qualifier in imports
import Prelude quote
import Prelude quote qualified as P
import quote Prelude qualified as P2

-- Test using 'splice' as a post-qualifier in imports
import Language.Haskell.TH.Syntax splice

import splice Language.Haskell.TH.Syntax qualified as TH
import Language.Haskell.TH.Syntax splice qualified as TH2

-- Use the imported modules
testQuote = [| id |]
testQuote2 = [| P.id |]
testQuote3 = [| P2.id |]

testSplice = $(lift "Hello from splice")
testSplice2 = $(TH.lift "Hello from splice2")
testSplice3 = $(TH2.lift "Hello from splice3")

