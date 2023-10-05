-- !!! re-exportation and ambiguity again.
-- Exporting M (=> id) is not ambiguous, as
-- Prelude isn't also exported.
module M (module M) where id x = x;
