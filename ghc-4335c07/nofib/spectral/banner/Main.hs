------------------------------------------------------------------------------
-- A simple banner program:                             Mark P Jones, 1992
--
-- Many years ago, I was helping out on a stand at a computer show.
-- Or at least, I would have been if anyone had been interested in
-- what we had on the stand.  So instead, I sat down to see if I
-- could write a banner program -- something to print messages out
-- in large letters.
--
-- The original program was in Basic, but here is a version in Hugs.
-- The program itself is only two lines long and that is rather pleasing,
-- but the raw data for the letters (and the function mapping characters
-- to letters) take up rather more space.  I don't have that Basic version
-- anymore.  I wonder whether the complete Hugs code is that much shorter?
--
-- One of the nice things about this program is that the main program is
-- completely independent of the size of characters.  You could easily add
-- a new font, perhaps with higher resolution (bigger letters), or even
-- variable width characters, and the program would take it all in its
-- stride.
--
-- If you have a wide screen (>80 cols), you might like to try evaluating:
--
--               (concat . map say . lines . say) "Hi"
--
-- and contemplating how easy it might have been to get my original
-- Basic version to perform this trick...
--
-- Enjoy!
------------------------------------------------------------------------------
import Data.Char
import Data.List
-- main: added by partain
main = do { stuff <- getContents; putStr (say stuff) }

------------------------------------------------------------------------------

say   = ('\n':) . unlines . map join . transpose . map picChar
        where join  = foldr1 (\xs ys -> xs ++ "  " ++ ys)

-- mapping characters to letters: --------------------------------------------

picChar c  | isUpper c  = alphas !! (fromEnum c - fromEnum 'A')
           | isLower c  = alphas !! (fromEnum c - fromEnum 'a')
           | isSpace c  = blank
           | isDigit c  = digits !! (fromEnum c - fromEnum '0')
           | c=='/'     = slant
           | c=='\\'    = reverse slant
           | otherwise  = head ([ letter | (c',letter) <- punct, c'==c ]
                                ++ [nothing])

-- letters data: -------------------------------------------------------------

blank  =  ["     ", "     ", "     ", "     ", "     "]

slant  =  ["    ",  "   ",   "  ",    " ",     ""     ]

nothing=  repeat ""

punct  =  [('.',  ["     ", "     ", "     ", "  .. ", "  .. "]),
           ('?',  [" ??? ", "?   ?", "   ? ", "  ?  ", "  .  "]),
           ('!',  ["  !  ", "  !  ", "  !  ", "  !  ", "  .  "]),
           ('-',  ["     ", "     ", "-----", "     ", "     "]),
           ('+',  ["  +  ", "  +  ", "+++++", "  +  ", "  +  "]),
           (':',  ["     ", "  :: ", "     ", "  :: ", "     "]),
           (';',  ["     ", "  ;; ", "     ", "  ;; ", " ;;  "])
          ]

digits = [[" OOO ", "0  00", "0 0 0", "00  0", " 000 "],
          ["  1  ", " 11  ", "  1  ", "  1  ", "11111"],
          [" 222 ", "2   2", "   2 ", "  2  ", "22222"],
          ["3333 ", "    3", " 333 ", "    3", "3333 "],
          ["   4 ", "  44 ", " 4 4 ", "44444", "   4 "],
          ["55555", "5    ", "5555 ", "    5", "5555 "],
          ["   66", "  6  ", " 666 ", "6   6", " 666 "],
          ["77777", "    7", "   7 ", "   7 ", "  7  "],
          [" 888 ", "8   8", " 888 ", "8   8", " 888 "],
          [" 999 ", "9   9", " 999 ", "  9  ", "99   "]]

alphas = [["  A  ", " A A ", "AAAAA", "A   A", "A   A"],
          ["BBBB ", "B   B", "BBBB ", "B   B", "BBBB "],
          [" CCCC", "C    ", "C    ", "C    ", " CCCC"],
          ["DDDD ", "D   D", "D   D", "D   D", "DDDD "],
          ["EEEEE", "E    ", "EEEEE", "E    ", "EEEEE"],
          ["FFFFF", "F    ", "FFFF ", "F    ", "F    "],
          [" GGGG", "G    ", "G  GG", "G   G", " GGG "],
          ["H   H", "H   H", "HHHHH", "H   H", "H   H"],
          ["IIIII", "  I  ", "  I  ", "  I  ", "IIIII"],
          ["JJJJJ", "   J ", "   J ", "J  J ", " JJ  "],
          ["K   K", "K  K ", "KKK  ", "K  K ", "K   K"],
          ["L    ", "L    ", "L    ", "L    ", "LLLLL"],
          ["M   M", "MM MM", "M M M", "M   M", "M   M"],
          ["N   N", "NN  N", "N N N", "N  NN", "N   N"],
          [" OOO ", "O   O", "O   O", "O   O", " OOO "],
          ["PPPP ", "P   P", "PPPP ", "P    ", "P    "],
          [" QQQ ", "Q   Q", "Q Q Q", "Q  Q ", " QQ Q"],
          ["RRRR ", "R   R", "RRRR ", "R  R ", "R   R"],
          [" SSSS", "S    ", " SSS ", "    S", "SSSS "],
          ["TTTTT", "  T  ", "  T  ", "  T  ", "  T  "],
          ["U   U", "U   U", "U   U", "U   U", " UUU "],
          ["V   V", "V   V", "V   V", " V V ", "  V  "],
          ["W   W", "W   W", "W   W", "W W W", " W W "],
          ["X   X", " X X ", "  X  ", " X X ", "X   X"],
          ["Y   Y", " Y Y ", "  Y  ", "  Y  ", "  Y  "],
          ["ZZZZZ", "   Z ", "  Z  ", " Z   ", "ZZZZZ"]
         ]

-- end of banner program -----------------------------------------------------
