{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ExplicitLevelImports #-}
module SI19 where

import quote SI19A

-- Quote imports work for variable quotes

boo = 'foo
