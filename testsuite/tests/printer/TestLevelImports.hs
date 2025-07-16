
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ExplicitLevelImports #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module TestLevelImports where
-- Based on test SI26 and SI01

------------------------------------------------
-- SI26

-- Test using 'quote' as a post-qualifier in imports
import Prelude quote
import Prelude quote qualified as P
import quote Prelude qualified as P2
import quote qualified Prelude as P3

-- Test using 'splice' as a post-qualifier in imports
import Language.Haskell.TH.Syntax splice

import splice Language.Haskell.TH.Syntax qualified as TH
import Language.Haskell.TH.Syntax splice qualified as TH2

-- Using a splice imported thing, inside an untyped and typed splice works
import splice SI01A

-- Use the imported modules
testQuote = [| id |]
testQuote2 = [| P.id |]
testQuote3 = [| P2.id |]

testSplice = $(lift "Hello from splice")
testSplice2 = $(TH.lift "Hello from splice2")
testSplice3 = $(TH2.lift "Hello from splice3")

------------------------------------------------
-- SI01

main :: IO ()
main = $( sid [| pure () |]) >> $$( sid [|| pure () ||])
