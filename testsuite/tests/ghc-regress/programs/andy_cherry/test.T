
include ($confdir ++ "/../multimod-test.T")

-- Args to mtr are: extra compile flags
--                  extra run flags
--                  expected process return value, if not zero

-- The program wants to read stuff from its own directory,
-- so we pass it the test dir as its single argument.
test "andy_cherry" { mtr("-package lang -cpp", $testdir, "") }

