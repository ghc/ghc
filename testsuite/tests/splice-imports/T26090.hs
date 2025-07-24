{-# LANGUAGE ExplicitLevelImports, TemplateHaskell #-}
module T26090 ( a --varaible
              , T(..) -- WithAll
              , S(s) -- With
              , R    -- Abs
              ) where

import quote T26090A
import T26090A (T(T), S)

