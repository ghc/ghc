{-# LANGUAGE ExplicitLevelImports, NoImplicitPrelude #-}
module T26616 where

import quote  Data.Maybe qualified as Q
import        Data.Maybe qualified as Z
import splice Data.Maybe qualified as S

foo = Q.isJust
