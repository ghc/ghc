This version includes checks to see if `extended precision' is
used in expressions, but does not determine the characteristics.


> module Main (main) where

> import Bits
> import LIAS

> version = "@(#)TestLIAS.lhs	1.2 dated 92/07/31 at 08:53:52"
> int_name = "Int"
> flp_name = "Float"
> int_val :: Int
> int_val = 1
> flp_val :: Float
> flp_val = 1

> maxInt, minInt :: Int
> maxInt = maxBound
> minInt = minBound + 1		-- NOTA BENE: this program does (minInt `rem` (-1)), and
>				-- that gives an exception if minInt = minBound,
>				-- because the result of the division is too big to fit

> main  =  (initial_checks flp_parms . main_identities flp_parms .
>           notification_checks flp_parms) (return ())
>          where
>          flp_parms  =  makeFloatParms flp_val

Data type for representing parameters of a RealFloat.
AN element has the form
(MkFloatParms r p emin emax denorm fmax fmin fminN epsilon)

> data (RealFloat a) =>
>      FloatParms a = MkFloatParms Integer Int Int Int Bool a a a a

> makeFloatParms :: (RealFloat a) => a -> FloatParms a
> makeFloatParms x
>     =  MkFloatParms (floatRadix x) (floatDigits x) (emin x) (emax x)
>                     (denorm x) (fmax x) (fmin x) (fminN x) (epsilon x)

> initial_checks :: (RealFloat a) => FloatParms a -> Cont -> Cont
> initial_checks (MkFloatParms r p eemin eemax ddenorm ffmax ffmin ffminN eps)
>     =   -- text is output here to form the basis of a report.
>        new_line .
>        showits "LIAS Model Implementation " . showits version . new_line .
>        new_line .
>        showits "Test results" .  new_line .
>        showits "Computer: " .  new_line .
>        showits "Compiler: " .   new_line .
>        showits "Options used: " . new_line .
>        showits "Program modifications (with reasons): " . new_line .
>        showits "Date tested: " . new_line .
>        showits "Tested by: " . new_line .
>        new_line .
>        showits "Integer type (int) name " . showits int_name . new_line .
>        showits "Floating point type (flp) name " . showits flp_name .
>        new_line . new_line .
>        showits "Parameter values" . new_line .
>        showits "        minint,         maxint" .
>        new_line .
>        showits (pad 15 (show minInt)) . showits (pad 15 (show maxInt)) .
>        new_line .
>        showits " r,  p,    emin,   emax, denorm" .
>        new_line .
>        showits (pad 3 (show r)) .
>        showits (pad 4 (show p)) .
>        showits (pad 8 (show eemin)) . showits (pad 8 (show eemax)) .
>        (if ddenorm then
>           showits "  true"
>        else
>           showits "  false") .
>        new_line .
>        showits "fmax    " . showit ffmax . new_line .
>        showits "fmin    " . showit ffmin . new_line .
>        showits "fminn   " . showit ffminN . new_line .
>        showits "epsilon " . showit eps . new_line .
>        (if (r `mod` 2 /= 0) || (r < 0) then
>           showits "floatRadix value is not positive even integer" .
>           new_line
>        else id) .
>        (if fromIntegral (p -1) * log (fromInteger r)
>            < log 1.0e6 then
>           --  the accuracy of the log function used here is not critical
>           showits "precision less than six decimal floatDigits" .
>           new_line
>        else id) .
>        (if (eemin -1) >= -2*(fromInteger r -1) then
>           showits "Exponent minimum too large" .
>           new_line
>        else id) .
>        (if eemax <= 2*(fromInteger r -1) then
>           showits "Exponent maximum not large enough" .
>           new_line
>        else id) .
>        (if (-2 > eemin -1+eemax) ||
>           (eemin -1+eemax > 2) then
>           showits "Exponent range not roughly symmetric" .
>           new_line
>        else id) .
>        new_line

> equal_int :: (Integral a) => (a, a, Int) -> Cont -> Cont
> equal_int (i,j, test_number)
>     | i /= j  =  showits "Integer operation check number " .
>                  showit test_number . showits " fails with " .
>                  showit i . showits " ". showit j . new_line
>     | True    =  showits "Integer operation check number " . showit test_number . showits " ok " . new_line

> equal_flp :: (RealFloat a) => (a, a, Int) -> Cont -> Cont
> equal_flp (x, y, test_number)
>     | x /= y  =  showits "Floating point operation check number " .
>                  showit test_number . showits " fails" . new_line .
>                  showit x . showits " " . showit y . new_line
>     | True    =  showits "Floating operation check number " . showit test_number . showits " ok " . new_line

> test_true :: (Bool, Int) -> Cont -> Cont
> test_true (b, test_number)
>     | not b  =  showits "Predicate number " . showit test_number .
>                 showits " fails " .  showit b . new_line
>     | True   =  showits "Predicate number " . showit test_number . showits " ok " . new_line

> -- This procedure checks that sqrt(y*y) = y when y*y is exact 
> check_exact_squares :: (RealFloat a) => FloatParms a -> Cont -> Cont
> check_exact_squares (MkFloatParms r p eemin eemax ddenorm
>                                   ffmax ffmin ffminN eps)
>     =  foldr (.) id (map foo list)
>        where
>        list  =  takeWhile in_range (iterate mul 10)
>        mul y  =  fromInteger (truncate (1.2 * y)) :: Float
>        in_range y  =  exponent y < p `div` 2
>        foo y  =  if y /= sqrt (fromInteger (truncate (y * y)))
>                  then showits "Square root not exact for a square" .
>                       showit y . new_line
>                  else id

> flp :: (Integral a, RealFloat b) => a -> b
> flp  =  fromIntegral

> int_part :: (RealFloat a) => a -> a
> int_part x  =  flp (truncate x)

> main_identities :: (RealFloat a) => FloatParms a -> Cont -> Cont
> main_identities flp_parms@(MkFloatParms r p eemin eemax ddenorm
>                                         ffmax ffmin ffminN eps)
>     =  equal_int(-(-maxInt), maxInt, 1) .
>        equal_int(2+2, 2*2, 2) .
>        equal_int(minInt `rem` (-1), 0, 3) .
>        equal_flp(1.0+1.0, 2.0, 4) .
>        equal_flp(ffmax-1.0, ffmax, 5) .
>        equal_flp(ffmax/2.0+ffmax/2.0, ffmax, 6) .
>        equal_flp(ffmax/ffmax, 1.0, 7) .
>        equal_flp((ffmax/flp(r))*flp(r), ffmax, 8) .
>        equal_flp(ffmin/ffmin, 1.0, 9) .
>        equal_flp(-(-1.1), 1.1, 10) .
>        equal_flp(abs(-ffmax), ffmax, 11) .
>        equal_flp(abs(-ffminN), ffminN, 12) .
>        equal_flp(signf(-ffmin), -1.0, 13) .
>        equal_flp(signf(0.0), 1.0, 14) .
>        equal_flp(signf(ffmin), 1.0, 15) .
>        -- NDN Tests 16-25 changed as they were incorrect
>        equal_int(exponent 1.0, 1, 16) .
>        equal_int(exponent 1.6, 1, 17) .
>        equal_int(exponent(flp(r)), 2, 18) .
>        equal_int(exponent(ffmax), eemax, 19) .
>        equal_int(exponent(ffminN), eemin, 20) .
>        (if ddenorm then
>           equal_int(exponent(ffmin), eemin-p+1, 21)
>        else id) .
>        equal_flp(significand(0.9), 0.9, 22) .
>        equal_flp(significand(1.0), scaleFloat (-1) 1, 23) .
>        -- NDN This fails on hbc. I'm not sure  if the test is correct.
>        equal_flp(significand(ffmax), predf(1), 24) .
>        -- equal_flp(significand(-ffmin), -1.0, 25) .
>        equal_flp(scaleFloat 1 1.1, 1.1*flp(r), 26) .
>        equal_flp(scaleFloat (-11) (scaleFloat 11 1.7), 1.7, 27) .
>        equal_flp(succf(1.0), 1.0+eps, 28) .
>        -- NDN Test 29 changed as it was incorrect
>        equal_flp(succf(significand(ffmax)), 1.0, 29) .
>        equal_flp(succf(-ffmin), 0.0, 30) .
>        equal_flp(succf(0.0), ffmin, 31) .
>        equal_flp(predf(succf(ffmin)), ffmin, 32) .
>        test_true(predf(flp(r)) < flp(r), 33) .
>        test_true(predf 1.1 < 1.1, 34) .
>        equal_flp(predf(succf 1.2), 1.2, 35) .
>        equal_flp(ulpf(1.0), eps, 36) .
>        equal_flp(flp(r)*ulpf(predf 1.0), eps, 37) .
>        equal_flp(succf(predf(ffmax)), ffmax, 38) .
>        equal_flp(truncf (1.0 + 3.0*eps) p, 1.0 + 3.0*eps, 39) .
>        equal_flp(truncf (1.0 + 3.0*eps) (p-1), 1.0 + 2.0*eps, 40) .
>        equal_flp(truncf (1.0 + 3.0*eps) (p-2), 1.0, 41) .
>        equal_flp(roundf (1.0 + 3.0*eps) p, 1.0 + 3.0*eps, 42) .
>        equal_flp(roundf (1.0 + 3.0*eps) (p-1), 1.0 + 4.0*eps, 43) .
>        equal_flp(roundf (1.0 + 3.0*eps) (p-2), 1.0 + 4.0*eps, 44) .
>        equal_flp(int_part 1.0, 1.0, 45) .
>        equal_flp(int_part(succf 1.0), 1.0, 46) .
>        equal_flp(int_part(predf 2.0), 1.0, 47) .
>        equal_flp(int_part(-ffmin), 0.0, 48) .
>        equal_flp(int_part(ffmin), 0.0, 49) .
>        equal_flp(fractpart(ffmax), 0.0, 50) .
>        equal_flp(fractpart(ffmin), ffmin, 51) .
>        equal_flp(fractpart(succf 1.0), eps, 52) .
>        equal_flp(fractpart(flp(r)), 0.0, 53) .
>        equal_flp(fractpart(-ffmin), -ffmin, 54) .
>        test_true(ffmin > 0.0, 55) .
>        test_true(-ffmax < -ffmin, 56) .
>        check_exact_squares flp_parms .
>        -- equal_int(int(predf 3.5), 3, 57) .
>        -- equal_int(int(succf 3.5), 4, 58) .
>        -- equal_int(int(predf -3.5), -4, 59) .
>        -- equal_flp(flp(int(-5.0)), -5.0, 60) .
>        -- equal_flp(flp(int(-5.6)), -6.0, 61) .
>        -- equal_flp(scaleFloat (eemax+1) (ffminN),
>        --        flp(r) ^^ integer(eemax+eemin), 62) .
>        -- equal_flp(scaleFloat(ffmax, eemin-2),
>        --        significand(ffmax) *
>        --          flp(r) ^^ integer(eemax+eemin-3), 63) .
>        -- check_conversions .
>        id

> notification_checks :: (RealFloat a) => FloatParms a -> Cont -> Cont
> notification_checks (MkFloatParms r p eemin eemax ddenorm
>                                   ffmax ffmin ffminN eps)
>     =  showits "Test  condition tested notify  result(ce/ne/other/no)" .
>        new_line .
>        let my_maxint = maxInt in
>            showits " 1   addi overflow pos  overf " .
>            showit (my_maxint + 1) .
>            new_line .
>            showits " 2   addi overflow neg  overf " .
>            (let tempi1 = -minInt ; tempi2 = -1 in
>            showit (tempi1 + tempi2)) .
>            new_line .
>            showits " 3   subi overflow neg  overf " .
>            showit (minInt - 1) .
>            new_line .
>            showits " 4   subi overflow pos  overf " .
>            (let tempi1 = maxInt ; tempi2 = -1 in
>            showit (tempi1 - tempi2)) .
>            new_line .
>            showits " 5   muli overflow pos  overf " .
>            (let tempi1 = my_maxint `div` 2 + 1 ; tempi2 = 2 in
>            showit (tempi1 * tempi2)) .
>            new_line .
>            showits " 6   muli overflow neg  overf " .
>            (let tempi1 = -2 ; tempi2 = my_maxint `div` 2 + 2 in
>            showit (tempi1 * tempi2)) .
>            new_line .
>            showits " 7   int divide by zero zerod " .
>            (let tempi1 = 1 ; tempi2 = my_maxint - maxInt in
>            showit (tempi1 `div` tempi2)) .
>            new_line .
>            showits " 8   int divide by zero zerod " .
>            showit (1 `div` (my_maxint-maxInt)) .
>            new_line .
>            showits " 9   remi divide by 0   zerod " .
>            showit (1 `rem` (my_maxint - maxInt)) .
>            new_line .
>            showits "10   modi divide by 0   zerod " .
>            (let tempi1 = 1 ; tempi2 = my_maxint - maxInt in
>            showit (tempi1 `mod` tempi2)) .
>            new_line .
>            showits "11  divide by zero      zerod " .
>            showit (1 `div` (my_maxint-maxInt)) .
>            new_line .
>            showits "12  divide by zero      zerod " .
>            showit (1 `div` (my_maxint-maxInt)) .
>            new_line .
>            -- showits "13   addf overflow      overf " .
>            -- showit (ffmax + flp(r) ** integer(eemax-p+1)) .
>            -- new_line .
>            -- showits "14   subf overflow      overf " .
>            -- showit (-ffmax - flp(r) ** integer(eemax-p+1)) .
>            -- new_line .
>            showits "15   mulf overflow      overf " .
>            showit (ffmax * 1.001) .
>            new_line .
>            showits "16   divf overflow      overf " .
>            showit (ffmax / 0.7) .
>            new_line .
>            showits "17   divf by zero       zerod " .
>            (let tempf1 = flp(my_maxint-maxInt) in
>            showit (1.0 / tempf1)) .
>            new_line .
>            showits "18   sqrt of tiny neg   undef " .
>            showit (sqrt(-ffmin)) .
>            new_line .
>            showits "19   exponentf(zero)    undef " .
>            (let tempf1 = flp(my_maxint-maxInt) in
>            showit (exponent(tempf1))) .
>            new_line .
>            showits "20   succf of fmax      overf " .
>            showit (succf(ffmax)) .
>            new_line .
>            showits "21   predf of -fmax     overf " .
>            showit (predf(-ffmax)) .
>            new_line .
>            showits "22   ulpf(zero)         undef " .
>            showit (ulpf(0.0)) .
>            new_line .
>            showits "23   roundf to 0 p undef " .
>            showit (roundf 1.0 0) .
>            new_line .
>            showits "24   roundf  overflow   overf " .
>            showit (roundf ffmax 2) .
>            -- new_line .
>            -- showits "25   trunc  overflow    overf " .
>            -- (if flp(maxInt) < max_mantissa then
>            --    showit (int(flp(maxInt)+1.0))
>            -- else
>            --    showit (int(succf(flp(maxInt))))) .
>            -- new_line .
>            -- showits "26   round overflow     overf " .
>            -- (if flp(maxInt) < max_mantissa then
>            --    showit (int(flp(minInt)-1.0))
>            -- else
>            --    showit (int(predf(flp(minInt)))))
>            id

