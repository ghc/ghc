
include ($confdir ++ "/../vanilla-test.T")

-- Args to vt are: extra compile flags
--                 extra run flags
--                 expected process return value, if not zero

test "drvrun001" { vt("", "", "") }
test "drvrun002" { vt("", "", "") }
test "drvrun003" { vt("", "", "") }
test "drvrun004" { vt("", "", "") }
test "drvrun005" { vt("", "-fail", "") }
test "drvrun006" { vt("", "", "") }
test "drvrun007" { vt("", "", "") }
test "drvrun008" { vt("-funbox-strict-fields", "", "") }
