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
