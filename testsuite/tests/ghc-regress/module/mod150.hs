-- !!! ambiguous re-exportation.
module M (module M,module Prelude) where id x = x;
