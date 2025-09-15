{-# LANGUAGE OverloadedStrings #-}
import GHC.SysTools.Ar

-- obtained from echo -n \0 > x.o && ar -q b.a x.o && cat b.a
archive = "!<arch>\nx.o/            0           0     0     644     1         \
\`\n0\nx.o/            0           0     0     644     1         `\n0\n"

main = print (parseAr archive)
