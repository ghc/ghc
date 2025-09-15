{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExplicitLevelImports #-}
{-# LANGUAGE NoImplicitPrelude #-}
module SI28 where

import quote Prelude -- Introduces Prelude at level 1

main = $([| id |]) -- Used at level 0 (should be an error)