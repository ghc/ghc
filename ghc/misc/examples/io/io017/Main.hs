import IO -- 1.3

main =
      hSetBuffering stdout NoBuffering                  >>
      putStr   "Enter an integer: "                     >>
      readLine                                          >>= \ x1 -> 
      putStr   "Enter another integer: "                >>
      readLine                                          >>= \ x2 -> 
      putStr  ("Their sum is " ++ show (read x1+ read x2) ++ "\n")

 where readLine = isEOF                                 >>= \ eof ->
                  if eof then return []
                  else getChar                          >>= \ c ->
                       if c `elem` ['\n','\r'] then
                          return []
                       else
                          readLine                      >>= \ cs ->
                          return (c:cs)

