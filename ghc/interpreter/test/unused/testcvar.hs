-- test:
-- A split-screen program:
--   User input is displayed in top half of screen;
--   Program output in the bottom half of the screen.

module TestCVar(talk) where
import Concurrent(
         forkIO, CVar, newCVar, readCVar, writeCVar
	 )

-- from ansi.hs (modified for Xterm settings)
goto :: Int -> Int -> String
goto x y = "\ESC[" ++ show (y+1) ++ ";" ++ show (x+1) ++ "H"

cls :: String
cls = "\ESC[H\ESC[2J"         -- for Xterm

-- Raw terminal handler:
--  Atomically writes characters to screen at specific coordinates.

type Terminal = CVar (Int,Int,Char)

terminal :: IO Terminal
terminal 
  = newCVar                  >>= \ buf ->
    forkIO (server_loop buf) >>
    return buf
 where
  -- possible optimisation: 
  --  remember current screen location to let us omit goto sometimes
  server_loop buf
    = readCVar buf          >>= \ (x,y,c) ->
      putStr (goto x y)    >>
      putChar c            >>
      server_loop buf

-- Window handler:
--  Keeps track of cursor position so that user program doesn't have to.
--  Doesn't do redraw, scrolling, clipping, etc

type DemoWindow = CVar Char

window :: Terminal -> Int -> Int -> IO DemoWindow
window t left top 
  = newCVar                      >>= \ buf ->
    forkIO (server_loop buf left top) >>
    return buf
 where
  server_loop buf x y
    = readCVar buf >>= \ c ->
      if c == '\n' then
        server_loop buf left (y+1)
      else
        writeCVar t (x,y,c) >>
        server_loop buf (x+1) y

put :: DemoWindow -> Char -> IO ()
put w c = writeCVar w c

-- copy input to top of screen, output to bottom of screen
talk :: (Char -> Char) -> IO ()
talk f =
  putStr cls     >>
  terminal       >>= \ t ->
  window t 0 0   >>= \ w1 ->
  window t 0 12  >>= \ w2 ->
  loop w1 w2
 where
  loop w1 w2
    = getCh        >>= \ c ->
      put w1 c     >>
      put w2 (f c) >>
      loop w1 w2

-- Non-blocking getchar
-- ToDo: find a way to replace the busy wait.
-- (Not easy in Unix!)
getCh :: IO Char
getCh
  = primIOAvailable           >>= \ avail ->
    if avail then
      getChar
    else
      primWait >>
      getCh
