{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ExplicitLevelImports #-}
module SI21 where

-- a is unbound, and apparently that should carry on working.
boo = 'a
