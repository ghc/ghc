
include ($confdir ++ "/../multimod-test.T")

-- Args to mtr are: extra compile flags
--                  extra run flags
--                  expected process return value, if not zero

-- this one causes the compiler to run out of heap in the simplifier
test "okeefe_neural" { 
	-- Skip the test on alpha-dec-osf3, where we
	-- run into an infinite loop on some builds
	skip when $platform == "alpha-dec-osf3"
	$expect = "fail"
	mtr("-package lang +RTS -M64m -RTS", "", "") }
