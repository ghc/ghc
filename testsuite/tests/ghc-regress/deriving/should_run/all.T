
include ($confdir ++ "/../vanilla-test.T")

-- Args to vt are: extra compile flags
--                 extra run flags
--                 expected process return value, if not zero

test "drvrun001" { vtr("", "", "") }
test "drvrun002" { vtr("", "", "") }
test "drvrun003" { vtr("", "", "") }
test "drvrun004" { vtr("", "", "") }
test "drvrun005" { vtr("", "-fail", "") }
test "drvrun006" { vtr("", "", "") }
test "drvrun007" { vtr("", "", "") }
test "drvrun008" { vtr("-funbox-strict-fields", "", "") }
