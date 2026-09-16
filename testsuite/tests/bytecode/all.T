ghci_dump_bcos = [only_ways(['ghci']), extra_run_opts('-dno-typeable-binds -dsuppress-uniques -ddump-bcos')]

test('T23068', ghci_dump_bcos + [filter_stdout_lines(r'.*bitmap: .*')], ghci_script, ['T23068.script'])

test('T25975', extra_ways(ghci_ways), compile_and_run,
     # Some of the examples work more robustly with these flags
     ['-fno-break-points -fno-full-laziness'])

test('T26565', extra_files(["T26565.hs"]), ghci_script, ['T26565.script'])
test('T23973', extra_files(["T23973.hs"]), ghci_script, ['T23973.script'])
test('T26640', extra_files(["T26640.hs"]), ghci_script, ['T26640.script'])

# Nullary data constructors
test('T26216', extra_files(["T26216_aux.hs"]), ghci_script, ['T26216.script'])

# SLIDE across stack chunk boundary (#27001)
test('T27001', [extra_files(['T27001.hs']), req_interp],
     run_command,
     ['{compiler} -e main -O -fno-unoptimized-core-for-interpreter T27001.hs'])

# Case alternatives are inlined into the parent BCO when the scrutinee cannot
# leave it (Note [Inlined case continuations]); count the resulting ProtoBCOs
# with and without -fbc-inline-case-conts.
inline_case_conts_cmd = ("{compiler} -fbyte-code -no-link -ddump-bcos -fforce-recomp "
                         "-fno-break-points -dno-typeable-binds -v0 {flags} InlineCaseConts.hs "
                         "| grep -c '^ *ProtoBCO'")
test('InlineCaseConts',
     [extra_files(['InlineCaseConts.hs']), req_interp, only_ways(['normal'])],
     run_command, [inline_case_conts_cmd.format(compiler='{compiler}', flags='')])
test('InlineCaseContsOff',
     [extra_files(['InlineCaseConts.hs']), req_interp, only_ways(['normal'])],
     run_command, [inline_case_conts_cmd.format(compiler='{compiler}', flags='-fno-bc-inline-case-conts')])

# Join points are compiled as labels when all their jumps stay in the BCO of
# the definition (Note [Join points as labels]); run the program in the
# interpreter, and count the ProtoBCOs with and without
# -fbc-join-points-as-labels.
join_points_opts = "-O -fno-unoptimized-core-for-interpreter -fno-break-points"
test('JoinPointsAsLabels',
     [extra_files(['JoinPointsAsLabels.hs']), req_interp, only_ways(['normal'])],
     run_command,
     ['{compiler} -e main ' + join_points_opts + ' JoinPointsAsLabels.hs'])
join_points_count_cmd = ("{compiler} -fbyte-code -no-link -ddump-bcos -fforce-recomp "
                         "-dno-typeable-binds -v0 " + join_points_opts +
                         " {flags} JoinPointsAsLabels.hs | grep -c '^ *ProtoBCO'")
test('JoinPointsAsLabelsCount',
     [extra_files(['JoinPointsAsLabels.hs']), req_interp, only_ways(['normal'])],
     run_command, [join_points_count_cmd.format(compiler='{compiler}', flags='')])
test('JoinPointsAsLabelsCountOff',
     [extra_files(['JoinPointsAsLabels.hs']), req_interp, only_ways(['normal'])],
     run_command, [join_points_count_cmd.format(compiler='{compiler}', flags='-fno-bc-join-points-as-labels')])

# Join points whose jumps are all in one case continuation BCO are compiled as
# labels in that BCO (Note [Join points as labels]). -fno-float-in keeps the
# join points outside the case alternatives that use them.
join_conts_opts = join_points_opts + " -fno-float-in"
test('JoinPointsInConts',
     [extra_files(['JoinPointsInConts.hs']), req_interp, only_ways(['normal'])],
     run_command,
     ['{compiler} -e main ' + join_conts_opts + ' JoinPointsInConts.hs'])
join_conts_count_cmd = ("{compiler} -fbyte-code -no-link -ddump-bcos -fforce-recomp "
                        "-dno-typeable-binds -v0 " + join_conts_opts +
                        " {flags} JoinPointsInConts.hs | grep -c '^ *ProtoBCO'")
test('JoinPointsInContsCount',
     [extra_files(['JoinPointsInConts.hs']), req_interp, only_ways(['normal'])],
     run_command, [join_conts_count_cmd.format(compiler='{compiler}', flags='')])
test('JoinPointsInContsCountOff',
     [extra_files(['JoinPointsInConts.hs']), req_interp, only_ways(['normal'])],
     run_command, [join_conts_count_cmd.format(compiler='{compiler}', flags='-fno-bc-join-points-as-labels')])

# Self-recursive join points whose jumps all sit in the BCO of the
# definition are compiled as loops (Note [Join points as loops]). Run the
# four shapes in LoopShapes.hs in the interpreter and check their output
# against native compilation, with -fbc-join-points-as-labels on and off
# (JoinPointLoopsOff is the control: with the flag off no loop is emitted
# and the program must still print the same).
loop_points_opts = "-O -fno-unoptimized-core-for-interpreter -fno-break-points"
test('JoinPointLoops',
     [extra_files(['LoopShapes.hs']), only_ways(ghci_ways),
      extra_hc_opts(loop_points_opts)],
     compile_and_run, [''])
test('JoinPointLoopsOff',
     [extra_files(['LoopShapes.hs']), only_ways(ghci_ways),
      extra_hc_opts(loop_points_opts + ' -fno-bc-join-points-as-labels')],
     compile_and_run, [''])

# Verdict checks on the "join points:" counter line for LoopShapes.hs, not
# raw dump text: grep the line, split it into its fields (at the commas and
# parentheses) and keep only the fields this feature added or changed, one
# per line in the order they have on the line. A field is matched whole, so
# "as labels" cannot be confused with "as labels in continuations", and
# fields added or removed elsewhere on the line do not disturb the test.
# NB: the command string is formatted once more by the driver (runCmd), so
# it must not contain any literal brace; hence tr/sed/grep and no awk.
join_points_loop_fields = ('as loops|rejected-recursive|'
                           'loop-rejected-in-continuation|'
                           'loop-rejected-placed-join|in-rec-join|'
                           'in-loop-join|rejected-disabled')
def join_points_filter(fields):
    return ("grep '^join points:' | tr '(),' '\\n\\n\\n' | sed -e 's/^join points://' "
            "-e 's/^ *//' -e 's/ *$//' | grep -xE '[0-9]+ (" + fields + ")'")
join_points_loop_cmd = ("{compiler} -fbyte-code -no-link -ddump-bcos -fforce-recomp "
                        "-dno-typeable-binds -v0 " + loop_points_opts +
                        " {flags} {module}.hs | " + join_points_filter('{fields}'))
test('JoinPointLoopsCount',
     [extra_files(['LoopShapes.hs']), req_interp, only_ways(['normal'])],
     run_command, [join_points_loop_cmd.format(compiler='{compiler}', flags='',
                                               module='LoopShapes', fields=join_points_loop_fields)])
test('JoinPointLoopsCountOff',
     [extra_files(['LoopShapes.hs']), req_interp, only_ways(['normal'])],
     run_command, [join_points_loop_cmd.format(compiler='{compiler}', flags='-fno-bc-join-points-as-labels',
                                               module='LoopShapes', fields=join_points_loop_fields)])

# Positive controls for the join-as-loop verdict, one module per shape:
#   JoinPointLoopCtl1  the defining-BCO loop, the case the whole placement
#                      commit exists for
#   JoinPointLoopCtl2  the same shape, reached only from a case continuation
#   JoinPointLoopCtl3  would be a loop, but its RHS defines a placed join
#                      point: loop-rejected-placed-join
#   JoinPointLoopCtl4  recursive jump inside a continuation of its own RHS:
#                      not at the empty path, stays rejected-recursive
#   JoinPointLoopCtl5  pins the order of the two rejections above: a RHS
#                      with both a placed join point and a spread jump site
#                      is rejected-recursive, not loop-rejected-placed-join
#   JoinPointLoopCtl6  all jumps in one continuation BCO:
#                      loop-rejected-in-continuation
loopctl_fields = {
    'JoinPointLoopCtl1': 'total|as labels|as loops|rejected-recursive|'
                         'loop-rejected-in-continuation|loop-rejected-placed-join|'
                         'in-rec-join|in-loop-join',
    'JoinPointLoopCtl2': 'total|as loops|rejected-recursive|in-rec-join|in-loop-join',
    'JoinPointLoopCtl3': 'total|as labels|as loops|rejected-recursive|'
                         'loop-rejected-placed-join|in-rec-join|in-loop-join',
    'JoinPointLoopCtl4': 'total|as loops|rejected-recursive|in-rec-join',
    'JoinPointLoopCtl5': 'total|as labels|as loops|rejected-recursive|'
                         'loop-rejected-placed-join|in-rec-join',
    'JoinPointLoopCtl6': 'total|as loops|rejected-recursive|'
                         'loop-rejected-in-continuation|in-rec-join',
}
for _m, _fields in loopctl_fields.items():
    test(_m, [extra_files([_m + '.hs']), req_interp, only_ways(['normal'])],
         run_command,
         [join_points_loop_cmd.format(compiler='{compiler}', flags='', module=_m, fields=_fields)])

# The safepoint's slow path (YIELD_CHECK -> slow path -> closure entry ->
# heap check) is only exercised by a loop that allocates on every iteration;
# see LoopSlow.hs. A rise in the GC count during the loop is evidence the
# slow path ran, sized on allocation rather than on wall time.
# JoinPointLoopSlowOff is the control with the flag off: no loop is emitted,
# so no GC-count assertion is made.
loop_slow_opts = loop_points_opts + " +RTS -T -A1m -RTS"
test('JoinPointLoopSlow',
     [extra_files(['LoopSlow.hs']), only_ways(ghci_ways),
      extra_hc_opts(loop_slow_opts), extra_run_opts('check-gcs')],
     compile_and_run, [''])
test('JoinPointLoopSlowOff',
     [extra_files(['LoopSlow.hs']), only_ways(ghci_ways),
      extra_hc_opts(loop_slow_opts + ' -fno-bc-join-points-as-labels')],
     compile_and_run, [''])
