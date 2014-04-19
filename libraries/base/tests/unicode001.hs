-- !!! Tests the various character classifiactions for a selection of Unicode
-- characters.

module Main where

import Data.Char

main = do
  putStrLn ("            " ++ concat (map (++" ") strs))
  mapM putStrLn (map do_char chars)
 where
  do_char char = s ++ (take (12-length s) (repeat ' ')) ++ concat (map f bs)
    where
          s = show char
          bs = map ($char) functions
          f True  = "X     "
          f False = "      "

strs = ["upper","lower","alpha","alnum","digit","print","space","cntrl"]

functions = [isUpper,isLower,isAlpha,isAlphaNum,isDigit,isPrint,isSpace,isControl]

chars = [backspace,tab,space,zero,lower_a,upper_a,delete,
        right_pointing_double_angle_quotation_mark,
        greek_capital_letter_alpha,
        bengali_digit_zero,
        en_space,
        gothic_letter_ahsa,
        monospaced_digit_zero
        ]

backspace             = '\x08'
tab                   = '\t'
space                 = ' '
zero                  = '0'
lower_a               = 'a'
upper_a               = 'A'
delete                = '\x7f'
right_pointing_double_angle_quotation_mark = '\xBB'
latin_small_letter_i_with_caron = '\x1D0'
combining_acute_accent = '\x301'
greek_capital_letter_alpha = '\x0391'
bengali_digit_zero    = '\x09E6'
en_space              = '\x2002'
gothic_letter_ahsa    = '\x10330'
monospaced_digit_zero = '\x1D7F6'
