main =
    hClose stderr >>
    hPutStr stderr "junk" `handle` \ (IllegalOperation _) -> putStr "Okay\n"

