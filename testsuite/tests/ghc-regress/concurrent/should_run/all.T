
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

test "conc001" { myvtr("", "", "") }
test "conc002" { myvtr("", "", "") }
test "conc003" { myvtr("", "", "") }
test "conc004" { myvtr("", "", "") }
test "conc005" { myvtr("", "", "") }
test "conc006" { myvtr("", "", "") }
test "conc007" { myvtr("", "+RTS -H128M -RTS", "") }
test "conc008" { myvtr("", "", "") }
test "conc009" { myvtr("", "", "") }
test "conc010" { myvtr("", "", "") }
test "conc012" { myvtr("", "", "") }
test "conc013" { myvtr("", "", "") }
test "conc014" { myvtr("", "", "") }
test "conc015" { myvtr("", "", "") }
test "conc016" { myvtr("", "", "") }
test "conc017" { myvtr("", "", "") }
test "conc018" { myvtr("", "", "") }
test "conc019" { myvtr("", "", "") }

--# conc020 *should* work on mingw32
test "conc020" { skip when $platform == "i386-unknown-mingw32"
                 myvtr("", "", "") }

test "conc021" { skip when $platform == "i386-unknown-mingw32"
                 myvtr("", "", "") }

test "conc022" { myvtr("", "", "") }
test "conc023" { myvtr("", "", "") }
test "conc024" { myvtr("", "", "") }
test "conc025" { myvtr("", "", "") }
test "conc026" { myvtr("", "", "") }
test "conc027" { myvtr("", "", "") }
test "conc028" { myvtr("", "", "") }
test "conc029" { myvtr("", "", "") }
test "conc030" { myvtr("", "", "") }
test "conc031" { myvtr("", "", "") }
