-- !!! ambiguous re-exportation.
module M (module M,id) where id x = x;
