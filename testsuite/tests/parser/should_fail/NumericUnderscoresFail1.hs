{-# LANGUAGE NumericUnderscores #-}

-- Test for NumericUnderscores extension.
-- See #14473
-- This is a testcase for invalid case of NumericUnderscores.

main :: IO ()
main = do
    print [
            -- float
            0_.0001,
            _0.0001,
            0.0001_,
            0._0001,

            -- float with exponent
            1e_+23,
            1e+23_,
            1e+_23
          ]
