
include ($confdir ++ "/../vanilla-test.T")

-- Args to vt are: extra compile flags
--                 extra run flags
--                 expected process return value, if not zero

test "arr001" { vtr("", "", "") }
test "arr002" { vtr("", "", "") }
test "arr003" { vtr("", "", "1") }
test "arr004" { vtr("", "", "1") }
test "arr005" { vtr("", "", "") }
test "arr006" { vtr("", "", "") }
test "arr007" { vtr("", "", "7") }
test "arr008" { vtr("", "", "8") }
test "arr009" { vtr("", "", "") }
test "arr010" { vtr("", "", "") }
test "arr011" { vtr("", "", "") }
test "arr012" { vtr("", "", "") }
test "arr013" { vtr("", "", "") }
test "arr014" { vtr("-package lang", "", "") }
test "arr015" { vtr("", "", "") }
test "arr016" { vtr("-fglasgow-exts", "", "") }
