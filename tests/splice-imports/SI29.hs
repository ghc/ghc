{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExplicitLevelImports #-}
{-# LANGUAGE NoImplicitPrelude #-}
module SI29 where

import quote Prelude -- Introduces Prelude at level 1
import Prelude

-- Interesting case: An instance for a wired-in type, that should always be available
-- since we don't require level checking for the wired-in type itself.
main = $([| id |])