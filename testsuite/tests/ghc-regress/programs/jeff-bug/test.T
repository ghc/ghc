
include ($confdir ++ "/../multimod-test.T")

-- Args to mtr are: extra compile flags
--                  extra run flags
--                  expected process return value, if not zero

-- Lack sensible input file to test it with.
test "jeff-bug" {
	$expect = "fail"
	mtr("-package lang -fglasgow-exts -cpp", "-count /dev/null", "") 
   }
