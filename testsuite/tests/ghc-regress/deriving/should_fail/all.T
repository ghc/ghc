
include ($confdir ++ "/../vanilla-test.T")

-- Args to vtcf are: extra compile flags

test "drvfail001" { vtcf("") }
test "drvfail002" { vtcf("") }
test "drvfail004" { vtcf("") }
test "drvfail007" { vtcf("") }
