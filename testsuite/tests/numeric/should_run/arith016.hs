{-# LANGUAGE MagicHash #-}

module Main where

import GHC.Exts ( Float(F#),
		   eqFloat#, neFloat#, ltFloat#,
		   leFloat#, gtFloat#, geFloat#,
                   isTrue#
		 )

fcmp_eq, fcmp_ne, fcmp_lt, fcmp_le, fcmp_gt, fcmp_ge :: (String, Float -> Float -> Bool)
fcmp_eq = ("==", \ (F# a) (F# b) -> isTrue# (a `eqFloat#` b))
fcmp_ne = ("/=", \ (F# a) (F# b) -> isTrue# (a `neFloat#` b))
fcmp_lt = ("< ", \ (F# a) (F# b) -> isTrue# (a `ltFloat#` b))
fcmp_le = ("<=", \ (F# a) (F# b) -> isTrue# (a `leFloat#` b))
fcmp_gt = ("> ", \ (F# a) (F# b) -> isTrue# (a `gtFloat#` b))
fcmp_ge = (">=", \ (F# a) (F# b) -> isTrue# (a `geFloat#` b))

float_fns = [fcmp_eq, fcmp_ne, fcmp_lt, fcmp_le, fcmp_gt, fcmp_ge]

float_vals :: [Float]
float_vals = [0.0, 1.0, read "NaN"]

float_text
   = [show4 arg1 ++ " " ++ fn_name ++ " " ++ show4 arg2 ++ "   = " ++ show (fn arg1 arg2)
      | (fn_name, fn) <- float_fns,
        arg1 <- float_vals,
        arg2 <- float_vals
     ]
     where
        show4 x = take 4 (show x ++ repeat ' ')

main
   = putStrLn (unlines float_text)
