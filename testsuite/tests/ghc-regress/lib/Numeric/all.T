
include ($confdir ++ "/../vanilla-test.T")

-- Args to vt are: extra compile flags
--                 extra run flags
--                 expected process return value, if not zero

test "num001" { vtr("", "", "") }
test "num002" { vtr("", "", "") }
test "num003" { vtr("", "", "") }
test "num004" { vtr("", "", "") }
test "num005" { vtr("", "", "") }
test "num006" { vtr("-package lang", "", "") }
test "num007" { vtr("", "", "") }
test "num008" { vtr("", "", "") }
