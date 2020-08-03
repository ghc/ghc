import Numeric

main :: IO ()
main = do
  h "" "Infinity"
  h "" "-Infinity"
  h "" "  -  Infinity  "
  h "" "NaN"
  h "" "-NaN"
  h "" "  -  NaN  "
  -- Examples taken from the double-precision floating-point wikipedia article
  h "wiki" "4.9406564584124654e-324" -- smallest positive subnormal number
  h "wiki" "2.2250738585072009e-308" -- largest subnormal number
  h "wiki" "2.2250738585072014e-308" -- smallest positive normal number
  h "wiki" "1.7976931348623157e308"  -- largest normal number
  h "wiki" "1.0000000000000002"      -- smallest number greater than one
  h "wiki" "0.9999999999999999"      -- largest number less than one
  -- Negative of the above
  h "-wiki" "-4.9406564584124654e-324"
  h "-wiki" "-2.2250738585072009e-308"
  h "-wiki" "-2.2250738585072014e-308"
  h "-wiki" "-1.7976931348623157e308"
  h "-wiki" "-1.0000000000000002"
  h "-wiki" "-0.9999999999999999"
  -- Repeated with hexadecimal floating-point notation
  h "wikihex" "0x1p-1074"               -- smallest positive subnormal number
  h "wikihex" "0x1.ffffffffffffep-1023" -- largest subnormal number
  h "wikihex" "0x1p-1022"               -- smallest positive normal number
  h "wikihex" "0x1.fffffffffffffp1023"  -- largest normal number
  h "wikihex" "0x1.0000000000001p0"     -- smallest number greater than one
  h "wikihex" "0x1.fffffffffffffp-1"    -- largest number less than one
  -- Negative of the above
  h "-wikihex" "-0x1p-1074"
  h "-wikihex" "-0x1.ffffffffffffep-1023"
  h "-wikihex" "-0x1p-1022"
  h "-wikihex" "-0x1.fffffffffffffp1023"
  h "-wikihex" "-0x1.0000000000001p0"
  h "-wikihex" "-0x1.fffffffffffffp-1"
  -- Other numbers of interest
  h "hex" "0x1p-1075"               -- Tie between 0 and the smallest subnormal
                                    -- should be broken in favor of 0
  h "hex" "0x1.0000000000001p-1074" -- Small number that should round up to the
                                    -- smallest subnormal value
  h "hex" "0x1.fffffffffffff7p1023" -- Any number less than the upper bound
                                    -- (defined below) will round to the largest
                                    -- normal number
  h "hex" "0x1.fffffffffffff8p1023" -- b^emax * (2 - 1/2 * b^(1-p)) and greater
                                    -- round to infinity
  h "hex" "0x1p1024"                -- With directed rounding it's possible this
                                    -- should round to the largest normal number
  h "hex" "0x0.1p-1072"             -- irrepresentable number, should shortcut
  -- Negative of the above
  h "-hex" "-0x1p-1075"
  h "-hex" "-0x1.0000000000001p-1074"
  h "-hex" "-0x1.fffffffffffff7p1023"
  h "-hex" "-0x1.fffffffffffff8p1023"
  h "-hex" "-0x1p1024"
  h "-hex" "-0x0.1p-1072"
  -- Allowed elisions
  h "dec elision" ".2e3"
  h "dec elision" "1.e3"
  h "dec elision" "1.2"
  h "dec elision" ".2"
  h "dec elision" "1."
  h "hex elision" "0x.2p3"
  h "hex elision" "0x1.p3"
  h "hex elision" "0x1.2"
  h "hex elision" "0x.2"
  h "hex elision" "0x1."

h :: String -> String -> IO ()
h prefix str = putStrLn (concat
                          (concatMap (\(a,b) -> [ prefix
                                                , "[("
                                                , showHFloat a ", "
                                                , show b
                                                , ")]"
                                                ]
                                     )
                                     (reads str :: [(Double, String)])
                          )
                        )
