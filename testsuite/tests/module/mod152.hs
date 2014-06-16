-- !!! ambiguous re-exportation.
module M (module Prelude,id) where id x = x;
