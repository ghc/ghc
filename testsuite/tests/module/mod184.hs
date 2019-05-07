{-# LANGUAGE ImportQualifiedPost #-}

-- With '-Wprepositive-qualified-module', a prepositive qualified
-- import should warn.

import qualified Prelude

main = Prelude.undefined
