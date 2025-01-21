{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExplicitLevelImports #-}

module SI27 where

-- This should fail: 'splice' used in both pre and post positions
import splice Prelude splice
import quote Prelude quote
import splice Prelude quote
import quote Prelude splice
