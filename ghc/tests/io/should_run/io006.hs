import IO -- 1.3

main =
    hClose stderr >>
    hPutStr stderr "junk" `catch` \ err -> if isIllegalOperation err then putStr "Okay\n" else error "Not okay\n"

