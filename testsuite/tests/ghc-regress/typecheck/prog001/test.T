
include ($confdir ++ "/../multimod-test.T")

test "typecheck.prog001" { mtc("C", "-fglasgow-exts") }

