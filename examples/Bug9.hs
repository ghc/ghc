-- Haddock 0.6 didn't parse this module, because the qualified
-- identifier C.safe was incorrectly lexed as 3 tokens.

module Check where
import qualified Foo as C
check = undefined { C.safe = 3 }
