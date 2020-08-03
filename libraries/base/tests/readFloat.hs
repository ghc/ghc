import Numeric

main :: IO ()
main = do
  h "" "Infinity"
  h "" "-Infinity"
  h "" "  -  Infinity  "
  h "" "NaN"
  h "" "-NaN"
  h "" "  -  NaN  "
  -- Examples taken from the single-precision floating-point wikipedia article
  h "wiki" "1.4012984643e-45"     -- smallest positive subnormal number
  h "wiki" "1.1754942107e-38"     -- largest subnormal number
  h "wiki" "1.1754943508e-38"     -- smallest positive normal number
  h "wiki" "3.4028234664e38"      -- largest normal number
  h "wiki" "0.999999940395355225" -- largest number less than one
  h "wiki" "1.00000011920928955"  -- smallest number larger than one
  -- Negative of the above
  h "-wiki" "-1.4012984643e-45"
  h "-wiki" "-1.1754942107e-38"
  h "-wiki" "-1.1754943508e-38"
  h "-wiki" "-3.4028234664e38"
  h "-wiki" "-0.999999940395355225"
  h "-wiki" "-1.00000011920928955"
  -- Repeated with hexadecimal floating-point notation
  h "wikihex" "0x1p-149"        -- smallest positive subnormal number
  h "wikihex" "0x1.fffffcp-127" -- largest subnormal number
  h "wikihex" "0x1p-126"        -- smallest positive normal number
  h "wikihex" "0x1.fffffep127"  -- largest normal number
  h "wikihex" "0x1.fffffep-1"   -- largest number less than one
  h "wikihex" "0x1.000002p0"    -- smallest number larger than one
  -- Negative of the above
  h "-wikihex" "-0x1p-149"
  h "-wikihex" "-0x1.fffffcp-127"
  h "-wikihex" "-0x1p-126"
  h "-wikihex" "-0x1.fffffep127"
  h "-wikihex" "-0x1.fffffep-1"
  h "-wikihex" "-0x1.000002p0"
  -- Other numbers of interest
  h "hex" "0x1p-150"        -- Tie between 0 and the smallest subnormal should
                            -- be broken in favor of 0
  h "hex" "0x1.000001p-150" -- Small number that should round up to the
                            -- smallest subnormal value
  h "hex" "0x1.fffffefp127" -- Any number less than the upper bound (defined
                            -- below) will round to the largest normal number
  h "hex" "0x1.ffffffp127"  -- b^emax * (2 - 1/2 * b^(1-p)) and greater round
                            -- to infinity
  h "hex" "0x1p128"         -- With directed rounding it's possible this should
                            -- round to the largest normal number
  h "hex" "0x0.1p-147"      -- irrepresentable number, should shortcut to 0
  -- Negative of the above
  h "-hex" "-0x1p-150"
  h "-hex" "-0x1.000001p-150"
  h "-hex" "-0x1.fffffefp127"
  h "-hex" "-0x1.ffffffp127"
  h "-hex" "-0x1p128"
  h "-hex" "-0x0.1p-147"
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
  putStrLn $ showFloat (read "" :: Float) ""

h :: String -> String -> IO ()
h prefix str = putStrLn (concat
                          (concatMap (\(a,b) -> [ prefix
                                                , "[("
                                                , showHFloat a ", "
                                                , show b
                                                , ")]"
                                                ]
                                     )
                                     (reads str :: [(Float, String)])
                          )
                        )
