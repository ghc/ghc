-- !!! re-exportation and ambiguity
-- should not fail as the absence of an export
-- list spec means "export all entities defined,
-- but none imported".
module M where id x = [];
