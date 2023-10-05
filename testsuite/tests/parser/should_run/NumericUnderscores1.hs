{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NegativeLiterals #-}

-- Test for NumericUnderscores extension.
-- See #14473
-- This is a testcase for unboxed literals.

import GHC.Types

main :: IO ()
main = do
    -- Each case corresponds to the definition of Lexer.x
    --
    -- Unboxed ints and words
    -- decimal int
    print [ (I# 1_000_000#) == 1000000,
            (I# 299_792_458#) == 299792458
          ]

    -- binary int
    print [ (I# 0b01_0000_0000#) == 0b0100000000,
            (I# 0b1_11_01_0000_0_111#) == 0b1110100000111
          ]

    -- octal int
    print [ (I# 0o1_000_000#) == 0o1000000,
            (I# 0O1__0#) == 0O10
          ]

    -- hexadecimal int
    print [ (I# 0x1_000_000#) == 0x1000000,
            (I# 0X3fff_ffff#) == 0x3fffffff
          ]

    -- negative decimal int
    print [ (I# -1_000_000#) == -1000000
          ]

    -- negative binary int
    print [ (I# -0b01_0000_0000#) == -0b0100000000
          ]

    -- negative octal int
    print [ (I# -0o1_000_000#) == -0o1000000
          ]

    -- negative hexadecimal int
    print [ (I# -0x1_000_000#) == -0x1000000
          ]

    -- decimal word
    print [ (W# 1_000_000##) == 1000000,
            (W# 299_792_458##) == 299792458
          ]

    -- binary word
    print [ (W# 0b1_0##) == 0b10
          ]

    -- octal word
    print [ (W# 0o1_0##) == 0o10
          ]

    -- hexadecimal word
    print [ (W# 0x1_0##) == 0x10
          ]

    -- Unboxed floats and doubles
    -- float
    print [ (F# 3.141_592_653_589_793#) == 3.141592653589793,
            (F# 3_14e-2#) == 314e-2,
            (F# 96_485.332_89#) == 96485.33289,
            (F# 6.022_140_857e+23#) == 6.022140857e+23,
            (F# -3.141_592#) == -3.141592,
            (F# -3_14e-2#) == -314e-2,
            (F# -6.022_140e+23#) == -6.022140e+23
          ]

    -- double
    print [ (D# 3_14e-2##) == 314e-2,
            (D# 96_485.332_89##) == 96485.33289,
            (D# 6.022_140_857e+23##) == 6.022140857e+23,
            (D# -3.141_592##) == -3.141592,
            (D# -3_14e-2##) == -314e-2,
            (D# -6.022_140e+23##) == -6.022140e+23
          ]
