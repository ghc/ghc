
include ($confdir ++ "/../vanilla-test.T")

-- Args to vt are: extra compile flags
--                 extra run flags
--                 expected process return value, if not zero

test "tcrun001" { vtr("", "", "") }
test "tcrun002" { vtr("", "", "") }
test "tcrun003" { vtr("-fglasgow-exts", "", "") }
test "tcrun004" { vtr("-fglasgow-exts", "", "") }
test "tcrun005" { vtr("", "", "") }
test "tcrun006" { vtr("", "", "") }

-- expect failure, because the prelude isn't compiled with generic support yet
test "tcrun007" { $expect = "fail" vtr("", "", "") }

test "tcrun008" { vtr("", "", "") }
test "tcrun009" { vtr("", "", "") }
test "tcrun010" { vtr("", "", "") }
test "tcrun011" { vtr("", "", "") }
test "tcrun012" { vtr("", "", "") }
test "tcrun013" { vtr("", "", "") }
test "tcrun014" { vtr("", "", "") }
test "tcrun015" { vtr("-O", "", "") }
