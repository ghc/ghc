-- Currently, capitalized identifiers in SCC pragmas must be put in quotes.
-- If you change this decision, this test is there to remind you to
-- update the section scc-pragma in User's Guide.
module T17916 where

f = {-# SCC CapitalCase #-} ()
