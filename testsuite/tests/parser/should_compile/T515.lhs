In #515, an LHS file gave warnings attributed to the wrong source location
(i.e., line 1).  This regression tests makes sure they are attributed to
the correct line.

> module Test where
> a = 1
> b = 2

The above should attribute the warnings to lines 6 and 7.
