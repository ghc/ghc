
include ($confdir ++ "/../multimod-test.T")

-- Args to mtr are: extra compile flags
--                  extra run flags
--                  expected process return value, if not zero

test "callback" { mtr("-fglasgow-exts -package lang -fvia-C", "", "") }

