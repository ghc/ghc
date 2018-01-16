-- !!! ds011 -- uses of "error"

module ShouldCompile where

f = error []

g = error ""

h = error "\""

i = error "foo"
