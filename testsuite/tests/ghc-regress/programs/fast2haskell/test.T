
include ($confdir ++ "/../multimod-test.T")

-- Args to mtr are: extra compile flags
--                  extra run flags
--                  expected process return value, if not zero

test "fast2haskell" { mtr("-fglasgow-exts -package lang", "", "") }

