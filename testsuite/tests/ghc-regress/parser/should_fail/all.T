
include ($confdir ++ "/../vanilla-test.T")

test "read001" { vtcf("") }
test "read002" { $expect = "fail" vtcf("") }
test "read003" { vtcf("") }
test "read004" { vtcf("") }
test "read005" { vtcf("") }
test "read006" { vtcf("") }
test "read007" { vtcf("") }
test "read008" { vtcf("") }
test "read009" { vtcf("") }
--test "read010" { vtcf("") }
test "read011" { vtcf("") }
test "read012" { vtcf("") }
test "read013" { vtcf("-fglasgow-exts") }
test "read014" { vtcf("") }
test "read015" { vtcf("") }
test "read016" { vtcf("") }
test "read017" { vtcf("") }
test "read018" { vtcf("") }
test "read019" { vtcf("") }
test "read020" { vtcf("") }

-- empty file (length zero) is a legal Haskell module.  It fails to compile
-- because it doesn't contain a definition of Main.main.  GHC 5.02 crashed
-- on this example.
test "read021" { vtcf("") }
test "read022" { vtcf("") }
