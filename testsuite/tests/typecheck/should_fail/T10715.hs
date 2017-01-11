{-# LANGUAGE FlexibleContexts #-}
module T10715 where

import Data.Coerce (coerce, Coercible)
import Data.Ord ( Down )  -- convenient newtype

data X a

-- See Trac #10715 for a long discussion about whether
-- this should be accepted or not.
--
-- But in Trac #12466 we decided to accept contradictory
-- type signatures, so definition is now accepeted even
-- though you can never call it.  Instead we get a
-- redundant pattern-match warning, in the
-- post-typechecking pattern-match checks
doCoerce :: Coercible a (X a) => a -> X a
doCoerce = coerce
