-- !!! Exporting non-existent type transparently
module M(T(..)) where
x = 'a' -- dummy definition to get round a separate bug
