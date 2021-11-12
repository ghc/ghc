{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
module List where
{-
inplace/bin/ghc-stage0 -O2 -dcore-lint

See Cabal:Distribution.Types.VersionRange:556
-}

import GHC.Base

data J = J ()

j :: () -> J
j = J

tup = (j, J)
tup2 = (J, j)

tup3 = [j, J]
tup4 = [J, j]

{-

[1 of 1] Compiling List             ( linear-tests/List.hs, linear-tests/List.o )

linear-tests/List.hs:17:12: error:
    • Couldn't match expected type ‘() -> J’ with actual type ‘() ⊸ J’
    • In the expression: J
      In the expression: [j, J]
      In an equation for ‘tup3’: tup3 = [j, J]
   |
17 | tup3 = [j, J]
   |            ^

linear-tests/List.hs:18:12: error:
    • Couldn't match expected type ‘() ⊸ J’ with actual type ‘() -> J’
    • In the expression: j
      In the expression: [J, j]
      In an equation for ‘tup4’: tup4 = [J, j]
   |
18 | tup4 = [J, j]
   |            ^

-}
