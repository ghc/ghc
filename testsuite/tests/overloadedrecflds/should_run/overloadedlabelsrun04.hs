{-# LANGUAGE OverloadedLabels, TemplateHaskell #-}

import OverloadedLabelsRun04_A

-- Who knew that there were so many ways that a line could start with
-- a # sign in Haskell? None of these are overloaded labels:
#line 7 "overloadedlabelsrun04.hs"
# 8 "overloadedlabelsrun04.hs"
#!notashellscript
#pragma foo

-- But this one is:
#foo
