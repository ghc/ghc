-- This test-case tickled a bug where an optimization pass incorrectly
-- reloaded a stack slot before the slot was initialized.  It was a bit
-- tricky to reproduce, and I don't really know why this particular
-- harness was necessary.

-- Miscompiled code must be in another module, otherwise problem doesn't
-- show up.
import Cgrun067A (miscompiledFn)
import Foreign.C.String

main = withCString "foobar" $ \p -> print =<< miscompiledFn p
