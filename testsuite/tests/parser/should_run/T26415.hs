{-# LANGUAGE MultilineStrings #-}

main :: IO ()
main = do
  -- The below strings contain the characters ['\\', '\t', '\\']
  print "\	\"
  print """\	\"""
