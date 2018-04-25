--
-- Interactive utility functions
-- Mark P. Jones November 1990
--
-- uses Haskell B. version 0.99.3
--
module Interact(Interactive, skip, end, readln, writeln, readch) where

-- The functions defined in this module provide basic facilities for
-- writing line-oriented interactive programs (i.e. a function mapping
-- an input string to an appropriate output string).  These definitions
-- are an enhancement of thos in B+W 7.8
--
-- skip p         is an interactive program which consumes no input, produces
--                no output and then behaves like the interactive program p.
-- end            is an interactive program which ignores the input and
--                produces no output.
-- writeln txt p  is an interactive program which outputs the message txt
--                and then behaves like the interactive program p
-- readch act def is an interactive program which reads the first character c
--                from the input stream and behaves like the interactive
--                program act c.  If the input character stream is empty,
--                readch act def prints the default string def and terminates.
-- 
-- readln p g     is an interactive program which prints the prompt p and
--                reads a line (upto the first carriage return, or end of
--                input) from the input stream.  It then behaves like g line.
--                Backspace characters included in the input stream are
--                interpretted in the usual way.

type Interactive = String -> String

--- Interactive program combining forms:

skip                 :: Interactive -> Interactive
skip p inn             = p inn    -- a dressed up identity function

end                  :: Interactive
end inn                = ""

writeln              :: String -> Interactive -> Interactive
writeln txt p inn      = txt ++ p inn

readch               :: (Char -> Interactive) -> String -> Interactive
readch act def ""     = def
readch act def (c:cs) = act c cs

readln               :: String -> (String -> Interactive) -> Interactive
readln prompt g inn    = prompt ++ lineOut 0 line ++ "\n"
                               ++ g (noBackSpaces line) input'
                        where line     = before '\n' inn
                              input'   = after  '\n' inn
                              after x  = tail . dropWhile (x/=)
                              before x = takeWhile (x/=)

--- Filter out backspaces etc:

rubout  :: Char -> Bool
rubout c = (c=='\DEL' || c=='\BS')

lineOut                      :: Int -> String -> String
lineOut n ""                  = ""
lineOut n (c:cs)
          | n>0  && rubout c  = "\BS \BS" ++ lineOut (n-1) cs
          | n==0 && rubout c  = lineOut 0 cs
          | otherwise         = c:lineOut (n+1) cs

noBackSpaces :: String -> String
noBackSpaces  = reverse . delete 0 . reverse
                where delete n ""          = ""
                      delete n (c:cs)
                               | rubout c  = delete (n+1) cs
                               | n>0       = delete (n-1) cs
                               | otherwise = c:delete 0 cs

--- End of Interact.hs
