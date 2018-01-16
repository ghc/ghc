import System.IO
import GHC.IO.Handle
import Control.Monad
import Data.List

newlines = ["\n","\r","\r\n","\n\r","\n\n","\r\r"]

-- make sure the file ends in '\r': that's a tricky case for CRLF
-- conversion, because the IO library has to check whether there's a
-- following \n before returning the \r.
content = concat [ show i ++ t | (i,t) <- zip [1..100] (cycle newlines) ]

filename = "newline001.out"

fromCRLF [] = []
fromCRLF ('\r':'\n':cs) = '\n' : fromCRLF cs
fromCRLF (c:cs) = c : fromCRLF cs

toCRLF [] = []
toCRLF ('\n':cs) = '\r':'\n': toCRLF cs
toCRLF (c:cs) = c : toCRLF cs

main = do
  h <- openBinaryFile filename WriteMode
  hPutStr h content
  hClose h
  testinput NoBuffering
  testinput LineBuffering
  testinput (BlockBuffering Nothing)
  testinput (BlockBuffering (Just 3))
  testinput (BlockBuffering (Just 7))
  testinput (BlockBuffering (Just 16))
  testoutput NoBuffering
  testoutput LineBuffering
  testoutput (BlockBuffering Nothing)
  testoutput (BlockBuffering (Just 3))
  testoutput (BlockBuffering (Just 7))
  testoutput (BlockBuffering (Just 16))

testinput b = do
  h <- openFile filename ReadMode
  hSetBuffering h b
  hSetNewlineMode h noNewlineTranslation
  str <- hGetContents h
  check "in1" b str content
  hClose h

  h <- openFile filename ReadMode
  hSetBuffering h b
  hSetNewlineMode h noNewlineTranslation
  str <- read_chars h
  check "in2" b str content
  hClose h

  h <- openFile filename ReadMode
  hSetBuffering h b
  hSetNewlineMode h noNewlineTranslation
  str <- read_lines h
  check "in3" b str content
  hClose h

  h <- openFile filename ReadMode
  hSetBuffering h b
  hSetNewlineMode h NewlineMode{ inputNL=CRLF, outputNL=LF }
  str <- hGetContents h
  check "in4" b str (fromCRLF content)
  hClose h

  h <- openFile filename ReadMode
  hSetBuffering h b
  hSetNewlineMode h NewlineMode{ inputNL=CRLF, outputNL=LF }
  str <- read_chars  h
  check "in5" b str (fromCRLF content)
  hClose h

  h <- openFile filename ReadMode
  hSetBuffering h b
  hSetNewlineMode h NewlineMode{ inputNL=CRLF, outputNL=LF }
  str <- read_lines  h
  check "in6" b str (fromCRLF content)
  hClose h

testoutput b = do
  h <- openFile filename WriteMode
  hSetBuffering h b
  hSetNewlineMode h NewlineMode{ inputNL=LF, outputNL=CRLF }
  hPutStr h content
  hClose h
  h <- openBinaryFile filename ReadMode
  str <- hGetContents h
  check "out1" b (toCRLF content) str
  hClose h

  h <- openFile filename WriteMode
  hSetBuffering h b
  hSetNewlineMode h NewlineMode{ inputNL=LF, outputNL=CRLF }
  mapM_ (hPutChar h) content
  hClose h
  h <- openBinaryFile filename ReadMode
  str <- hGetContents h
  check "out2" b (toCRLF content) str
  hClose h

check s b str1 str2 = do
  when (str1 /= str2) $ error ("failed: " ++ s ++ ", " ++ show b ++ '\n':show str1 ++ '\n':show str2)

read_chars :: Handle -> IO String
read_chars h = loop h ""
  where loop h acc = do
          b <- hIsEOF h
          if b then return (reverse acc) else do
          c <- hGetChar h
          loop h (c:acc)

read_lines :: Handle -> IO String
read_lines h = loop h []
  where loop h acc = do
          b <- hIsEOF h
          if b then return (intercalate "\n" (reverse acc)) else do
          l <- hGetLine h
          loop h (l : acc)
