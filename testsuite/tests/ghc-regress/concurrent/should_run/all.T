
include ($confdir ++ "/../vanilla-test.T")

-- Args to vt are: extra compile flags
--                 extra run flags
--                 expected process return value, if not zero

def myvt ( $args_c, $args_r, $ret_res )
{
   vt ( " -package concurrent -fglasgow-exts " ++ $args_c,
        $args_r, $ret_res )
}

--conc009_RUNTEST_OPTS = -x 1
--conc021_RUNTEST_OPTS = -x 250 -o2 conc021.stderr-mingw

test "conc001" { vt("", "", "") }
test "conc002" { vt("", "", "") }
test "conc003" { vt("", "", "") }
test "conc004" { vt("", "", "") }
test "conc005" { vt("", "", "") }
test "conc006" { vt("", "", "") }
test "conc007" { vt("", "+RTS -H128M -RTS", "") }
test "conc008" { vt("", "", "") }
test "conc009" { vt("", "", "") }
test "conc010" { vt("", "", "") }
test "conc012" { vt("", "", "") }
test "conc013" { vt("", "", "") }
test "conc014" { vt("", "", "") }
test "conc015" { vt("", "", "") }
test "conc016" { vt("", "", "") }
test "conc017" { vt("", "", "") }
test "conc018" { vt("", "", "") }
test "conc019" { vt("", "", "") }

--# conc020 *should* work on mingw32
test "conc020" { skip when $platform == "i386-unknown-mingw32"
                 vt("", "", "") }

test "conc021" { skip when $platform == "i386-unknown-mingw32"
                 vt("", "", "") }

test "conc022" { vt("", "", "") }
test "conc023" { vt("", "", "") }
test "conc024" { vt("", "", "") }
test "conc025" { vt("", "", "") }
test "conc026" { vt("", "", "") }
test "conc027" { vt("", "", "") }
test "conc028" { vt("", "", "") }
test "conc029" { vt("", "", "") }
test "conc030" { vt("", "", "") }
test "conc031" { vt("", "", "") }
