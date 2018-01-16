data T = C Int

unpick (C i) = i + 1

main = print (unpick (C{}))