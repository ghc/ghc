
include ($confdir ++ "/../multimod-test.T")

-- Args to mtr are: extra compile flags
--                  extra run flags
--                  expected process return value, if not zero

test "okeefe_neural" { mtr("-package lang -H120m", "", "") }

