
include ($confdir ++ "/../multimod-test.T")

-- Args to mtr are: extra compile flags
--                  extra run flags
--                  expected process return value, if not zero

-- this one causes the compiler to run out of heap in the simplifier
test "okeefe_neural" { 
	$expect = "fail"
	mtr("-package lang +RTS -M64m -RTS", "", "") }
