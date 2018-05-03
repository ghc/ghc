import GHC.ResponseFile

assertEqual :: (Eq a, Show a) => a -> a -> IO ()
assertEqual x y = if x == y
                  then return ()
                  else error $ "assertEqual: " ++ show x ++ " /= " ++ show y

-- Migrated from Haddock.

-- The first two elements are
--   1) a list of 'args' to encode and
--   2) a single string of the encoded args
-- The 3rd element is just a description for the tests.
testStrs :: [(([String], String), String)]
testStrs =
  [ ((["a simple command line"],
      "a\\ simple\\ command\\ line\n"),
     "the white-space, end with newline")

  , ((["arg 'foo' is single quoted"],
      "arg\\ \\'foo\\'\\ is\\ single\\ quoted\n"),
     "the single quotes as well")

  , ((["arg \"bar\" is double quoted"],
      "arg\\ \\\"bar\\\"\\ is\\ double\\ quoted\n"),
     "the double quotes as well" )

  , ((["arg \"foo bar\" has embedded whitespace"],
      "arg\\ \\\"foo\\ bar\\\"\\ has\\ embedded\\ whitespace\n"),
     "the quote-embedded whitespace")

  , ((["arg 'Jack said \\'hi\\'' has single quotes"],
      "arg\\ \\'Jack\\ said\\ \\\\\\'hi\\\\\\'\\'\\ has\\ single\\ quotes\n"),
     "the escaped single quotes")

  , ((["arg 'Jack said \\\"hi\\\"' has double quotes"],
      "arg\\ \\'Jack\\ said\\ \\\\\\\"hi\\\\\\\"\\'\\ has\\ double\\ quotes\n"),
     "the escaped double quotes")

  , ((["arg 'Jack said\\r\\n\\t \\\"hi\\\"' has other whitespace"],
      "arg\\ \\'Jack\\ said\\\\r\\\\n\\\\t\\ \\\\\\\"hi\\\\\\\"\\'\\ has\\ \
           \other\\ whitespace\n"),
     "the other whitespace")

  , (([ "--prologue=.\\dist\\.\\haddock-prologue3239114604.txt"
      , "--title=HaddockNewline-0.1.0.0: This has a\n\
        \newline yo."
      , "-BC:\\Program Files\\Haskell Platform\\lib"],
      "--prologue=.\\\\dist\\\\.\\\\haddock-prologue3239114604.txt\n\
      \--title=HaddockNewline-0.1.0.0:\\ This\\ has\\ a\\\n\
      \newline\\ yo.\n\
      \-BC:\\\\Program\\ Files\\\\Haskell\\ Platform\\\\lib\n"),
     "an actual haddock response file snippet with embedded newlines")
  ]

main :: IO ()
main = do
  -- Test escapeArgs
  mapM_ (\((ss1,s2),des) -> escapeArgs ss1 `assertEqual` s2) testStrs

  -- Test unescapeArgs
  mapM_ (\((ss1,s2),des) -> unescapeArgs s2 `assertEqual` ss1) testStrs

  -- Given unescaped quotes, it should pass-through,
  -- without escaping everything inside

  (filter (not . null) $
   unescapeArgs "this\\ is\\ 'not escape\\d \"inside\"'\\ yo\n")
   `assertEqual`
   ["this is not escaped \"inside\" yo"]

  (filter (not . null) $
   unescapeArgs "this\\ is\\ \"not escape\\d 'inside'\"\\ yo\n")
   `assertEqual`
   ["this is not escaped 'inside' yo"]
