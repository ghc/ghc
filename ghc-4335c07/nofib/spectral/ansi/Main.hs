-- This is a simple program using ANSI escape sequences to create a program
-- which uses direct cursor addressing and input/output.
--
-- People are often quite surprised the first time they see a program like
-- this written in a functional language.

import System.Environment

-- Basic screen control codes:

-- Choose whichever of the following lines is suitable for your system:
--partain: cls = "\ESC[2J"     -- for PC with ANSI.SYS
cls         = "\^L"         -- for Sun window

goto x y    = '\ESC':'[':(show y ++(';':show x ++ "H"))
at (x,y) s  = goto x y ++ s
home        = goto 1 1
highlight s = "\ESC[7m"++s++"\ESC[0m"


-- Some general purpose functions for interactive programs:

type Interact             = String -> String

end                      :: Interact
end cs                    = ""

readChar, peekChar       :: Interact -> (Char -> Interact) -> Interact
readChar eof use []       = eof []
readChar eof use (c:cs)   = use c cs

peekChar eof use []       = eof []     -- like readChar, but character is
peekChar eof use cs@(c:_) = use c cs   -- not removed from input stream

pressAnyKey              :: Interact -> Interact
pressAnyKey prog          = readChar prog (\c -> prog)

unreadChar               :: Char -> Interact -> Interact
unreadChar c prog cs      = prog (c:cs)

writeChar                :: Char -> Interact -> Interact
writeChar c prog cs       = c : prog cs

writeString              :: String -> Interact -> Interact
writeString s prog cs     = s ++ prog cs

writes                   :: [String] -> Interact -> Interact
writes  ss                = writeString (concat ss)

ringBell                 :: Interact -> Interact
ringBell                  = writeChar '\BEL'


-- Screen oriented input/output functions:

type Pos           = (Int,Int)

clearScreen        = writeString cls
writeAt (x,y) s    = writeString (goto x y ++ s)
moveTo  (x,y)      = writeString (goto x y)


readAt            :: Pos                  ->  -- Start coordinates
                     Int                  ->  -- Maximum input length
                     (String -> Interact) ->  -- How to use entered string
                     Interact

readAt (x,y) l use = writeAt (x,y) (copy l '_') (moveTo  (x,y) (loop 0 ""))
 where loop n s    = readChar (return s) (\c ->
                     case c of '\BS'         -> delete n s
                               '\DEL'        -> delete n s
                               '\n'          -> return s
                               c | n < l     -> writeChar c (loop (n+1) (c:s))
                                 | otherwise -> ringBell (loop n s))
       delete n s  = if n>0 then writeString "\BS_\BS" (loop (n-1) (tail s))
                            else ringBell (loop 0 "")
       return s    = use (reverse s)


defReadAt         :: Pos                  ->  -- Start coordinates
                     Int                  ->  -- Maximum input length
                     String               ->  -- Default string value
                     (String -> Interact) ->  -- How to use entered string
                     Interact
defReadAt (x,y) l def use
                   = writeAt (x,y) (take l (def++repeat '_')) (
                     readChar (use def) (\c ->
                     if c=='\n' then use def
                                else unreadChar c (readAt (x,y) l use)))

promptReadAt (x,y) l prompt use
                   = writeAt (x,y) prompt (readAt (x+length prompt,y) l use)

defPromptReadAt (x,y) l prompt def use
                   = writeAt (x,y) prompt (
                     defReadAt (x+length prompt,y) l def use)
                                  
 
-- A sample program:
-- Enter the expression `run program' in Hugs to try this program out

program = writes [ cls,
                   at (17,5)  (highlight "Demonstration program"),
                   at (48,5)  "Version 1.0",
                   at (17,7)  "This program illustrates a simple approach",
                   at (17,8)  "to screen-based interactive programs using",
                   at (17,9)  "the Hugs functional programming system.",
                   at (17,11) "Please press any key to continue ..."
                 ]
          (pressAnyKey
          (promptReadAt (17,15) 18 "Please enter your name: " (\name ->
          (let reply = "Hello " ++ name ++ "!" in
           writeAt (40-(length reply`div` 2),18) reply
          (moveTo (1,23)
          (writeString "I'm waiting...\n"
          (pressAnyKey
          end)))))))

-- added by partain

main = do
  (n:_) <- getArgs
  interact (foldr (.) id (take (read n) (repeat program)))

copy    :: Int -> a -> [a]
copy n x = take n (repeat x)

-- End of file
