-- !!! Exporting non-existent module
module M(module N) where
x = 'a' -- dummy definition to get round a separate bug
