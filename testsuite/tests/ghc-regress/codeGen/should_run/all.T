
include ($confdir ++ "/../vanilla-test.T")

-- Args to vt are: extra compile flags
--                 extra run flags
--                 expected process return value, if not zero

test "cg001" { vtr("", "", "") }
test "cg002" { vtr("", "", "") }
test "cg003" { vtr("", "", "") }
test "cg004" { vtr("", "", "") }
test "cg005" { vtr("", "", "") }
test "cg006" { vtr("", "", "") }
test "cg007" { vtr("", "", "") }
test "cg008" { vtr("", "", "") }
test "cg009" { vtr("", "", "") }
test "cg010" { vtr("", "", "") }
test "cg011" { vtr("", "", "") }
test "cg012" { vtr("-fglasgow-exts", "", "") }
test "cg013" { vtr("", "", "") }
test "cg014" { vtr("", "", "") }
test "cg015" { vtr("-fglasgow-exts", "", "") }
test "cg016" { vtr("", "", "1") }
test "cg017" { vtr("", "", "") }
test "cg018" { vtr("-fglasgow-exts", "", "") }
test "cg019" { vtr("", "", "") }
test "cg020" { vtr("", "", "") }
test "cg021" { vtr("", "", "") }
test "cg022" { vtr("", "", "") }
test "cg024" { vtr("", "", "") }
test "cg025" { vtr("", "cg025.hs", "1") }
test "cg026" { vtr("-fglasgow-exts -fvia-C", "", "") }
test "cg027" { vtr("", "", "") }
test "cg028" { vtr("", "", "") }
test "cg029" { vtr("-fglasgow-exts", "", "") }
test "cg030" { vtr("-fglasgow-exts", "", "") }
test "cg031" { vtr("-fglasgow-exts", "", "") }
test "cg032" { vtr("-fglasgow-exts", "", "") }
test "cg033" { vtr("-fglasgow-exts", "", "") }
test "cg034" { vtr("", "", "") }
test "cg035" { vtr("-fglasgow-exts", "", "") }
test "cg036" { vtr("", "", "") }
test "cg037" { vtr("", "", "") }
test "cg038" { vtr("", "", "") }
test "cg039" { vtr("", "", "") }
test "cg040" { vtr("", "", "") }
test "cg042" { vtr("-fglasgow-exts", "", "") }
test "cg043" { vtr("", "", "") }
test "cg044" { vtr("-cpp", "", "" ) }

-- -O is temporary, until we fix the problems with seq#...
test "cg045" { vtr( "-O", "", "1") }
test "cg046" { vtr("", "", "") }
test "cg047" { vtr("", "", "") }
test "cg048" { vtr("", "", "") }
test "cg049" { vtr( "-funbox-strict-fields", "", "") }

-- NB: be sure to run cg050 *without* -O; that's what showed the bug
test "cg050" { vtr( "", "", "") }
test "cg051" { vtr( "", "", "1") }	-- Expected to fail
