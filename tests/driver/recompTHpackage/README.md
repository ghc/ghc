Modifying the definition of `p` in `Lib.hs` does not cause `q` to be recompiled.

```
> cabal new-run q
0
-- Change p = [| 1 |]
> cabal new-run q
-- BUG: Should be 1
0
