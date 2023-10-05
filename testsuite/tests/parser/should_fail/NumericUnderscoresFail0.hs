{-# LANGUAGE NumericUnderscores #-}

-- Test for NumericUnderscores extension.
-- See #14473
-- This is a testcase for invalid case of NumericUnderscores.

main :: IO ()
main = do
    print [
            -- integer
            1000000_,
            _1000000
          ]
