{-# LANGUAGE TemplateHaskell #-}

import SizedLiteralsA
import Language.Haskell.TH

{-

  This file is compiled with the GHC flags:

         -O -fbyte-code-and-object-code -fprefer-byte-code

  This makes sure that the Template Haskell runs in the bytecode
  interpreter with optimized bytecode, allowing us to test the
  sized unboxed literals.

  Running the test in GHCi directly would disable optimization.

 -}

main :: IO ()
main = do
    print $(pure $ ListE [ ie (fibw8  5)
                         , ie (fibw16 5)
                         , ie (fibw32 5)
                         , ie (fibw64 5)
                         ])

    print $(pure $ ListE [ ie (fibi8  5)
                         , ie (fibi16 5)
                         , ie (fibi32 5)
                         , ie (fibi64 5)
                         ])

    print $(pure $ ListE [ ie (branchi8 0)
                         , ie (branchi8 1)
                         , ie (branchi8 (-1))
                         , ie (branchi8 126)
                         , ie (branchi8 127)
                         , ie (branchi8 (-127))
                         , ie (branchi8 (-128))
                         , ie (branchi8 2)
                         ])

    print $(pure $ ListE [ ie (branchi16 0)
                         , ie (branchi16 1)
                         , ie (branchi16 (-1))
                         , ie (branchi16 32767)
                         , ie (branchi16 32766)
                         , ie (branchi16 (-32768))
                         , ie (branchi16 (-32767))
                         , ie (branchi16 2)
                         ])

    print $(pure $ ListE [ ie (branchi32 0)
                         , ie (branchi32 1)
                         , ie (branchi32 (-1))
                         , ie (branchi32 2147483646)
                         , ie (branchi32 2147483647)
                         , ie (branchi32 (-2147483648))
                         , ie (branchi32 (-2147483647))
                         , ie (branchi32 2)
                         ])

    print $(pure $ ListE [ ie (branchi64 0)
                         , ie (branchi64 1)
                         , ie (branchi64 (-1))
                         , ie (branchi64 2147483647)
                         , ie (branchi64 2147483648)
                         , ie (branchi64 4294967297)
                         , ie (branchi64 (-2147483648))
                         , ie (branchi64 (-2147483649))
                         , ie (branchi64 (-4294967295))
                         , ie (branchi64 9223372036854775807)
                         , ie (branchi64 9223372036854775806)
                         , ie (branchi64 (-9223372036854775808))
                         , ie (branchi64 (-9223372036854775807))
                         , ie (branchi64 2)
                         ])

    print $(pure $ ListE [ ie (branchw8 0)
                         , ie (branchw8 1)
                         , ie (branchw8 254)
                         , ie (branchw8 255)
                         , ie (branchw8 2)
                         ])

    print $(pure $ ListE [ ie (branchw16 0)
                         , ie (branchw16 1)
                         , ie (branchw16 255)
                         , ie (branchw16 256)
                         , ie (branchw16 65534)
                         , ie (branchw16 65535)
                         , ie (branchw16 2)
                         ])

    print $(pure $ ListE [ ie (branchw32 0)
                         , ie (branchw32 1)
                         , ie (branchw32 65534)
                         , ie (branchw32 65535)
                         , ie (branchw32 65536)
                         , ie (branchw32 4294967295)
                         , ie (branchw32 4294967294)
                         , ie (branchw32 4294967293)
                         , ie (branchw32 2)
                         ])

    print $(pure $ ListE [ ie (branchw64 0)
                         , ie (branchw64 1)
                         , ie (branchw64 65536)
                         , ie (branchw64 4294967295)
                         , ie (branchw64 4294967296)
                         , ie (branchw64 4294967297)
                         , ie (branchw64 18446744073709551615)
                         , ie (branchw64 18446744073709551614)
                         , ie (branchw64 18446744073709551613)
                         , ie (branchw64 2)
                         ])