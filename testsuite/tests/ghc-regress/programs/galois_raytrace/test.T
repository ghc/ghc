
include ($confdir ++ "/../multimod-test.T")

-- Args to mtr are: extra compile flags
--                  extra run flags
--                  expected process return value, if not zero

-- pass $testdir so it knows where its home dir is
test "galois_raytrace" { mtr("-O -package lang -package text", "", "") }

