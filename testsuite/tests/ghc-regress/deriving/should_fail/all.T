
include ($confdir ++ "/../vanilla-test.T")

-- Args to vtf are: extra compile flags

test "drvfail001" { vtf("") }
test "drvfail004" { vtf("") }
test "drvfail007" { vtf("") }
