
test('read001', normal, compile, [''])
test('read002', normal, compile, [''])
test('read003', normal, compile, [''])
test('read004', normal, compile, [''])
test('read005', normal, compile, [''])

# Missing:
# test('read006', normal, compile, [''])
# test('read007', normal, compile, [''])

test('read008', normal, compile, [''])
test('read009', normal, compile, [''])
test('read010', normal, compile, [''])
test('read011', normal, compile, [''])

# Missing:
# test('read012', normal, compile, [''])
# test('read013', normal, compile, [''])

test('read014', normal, compile, ['-Wall -fno-warn-orphans'])
test('read015', normal, compile, [''])
test('read016', normal, compile, [''])
test('read017', normal, compile, [''])
test('read018', normal, compile, [''])
test('read019', normal, compile, [''])

# Missing:
# test('read020', normal, compile, [''])

test('read021', normal, compile, [''])
test('read022', normal, compile, [''])

# we don't support strict Haskell 98 with respect to qualified identifiers
# any more (read023), see #1215.  This test is in fact incorrect Haskell 98
# anyway, so expect_fail would be wrong.
test('read023', skip, compile, [''])

test('read024', normal, compile, [''])
test('read025', normal, compile, [''])
test('read026', normal, compile, [''])
test('read027', normal, compile, [''])
test('read028', normal, compile, [''])
test('read029', normal, compile, [''])
test('read030', normal, compile, [''])
test('read031', normal, compile, [''])
test('read032', normal, compile, [''])
test('read033', normal, compile, [''])
test('read034', normal, compile, [''])
test('read036', normal, compile, [''])
test('read037', normal, compile, [''])
test('read038', normal, compile, [''])
test('read039', normal, compile, ['-fno-code'])
test('read040', normal, compile, [''])
test('read041', literate, compile, [''])
test('read042', normal, compile, [''])
test('read043', normal, compile, [''])
test('read044', normal, compile, [''])
test('read045', normal, compile, [''])
test('read046', normal, compile, [''])
test('read047', normal, compile, [''])
test('read048', normal, compile, [''])
test('read049', normal, compile, [''])
test('read050', normal, compile, [''])
test('read051', normal, compile, [''])
test('read052', normal, compile, [''])
test('read053', normal, compile, [''])
test('read054', normal, compile, [''])
test('read055', normal, compile, [''])
test('read056', normal, compile, [''])
test('read057', normal, compile, [''])
test('read058', normal, compile, [''])
test('read059', normal, compile, [''])
test('read060', normal, compile, [''])
test('read061', normal, compile, [''])
test('read062', normal, compile, [''])
test('read063', normal, compile, [''])
test('read064', normal, compile, [''])
test('read066', normal, compile, [''])
test('read067', normal, compile, [''])
test('read068', normal, compile, [''])
test('read069', normal, compile, [''])
test('read_1821', normal, compile, [''])
test('T2245', normal, compile, ['-fwarn-type-defaults'])
test('T3303', [], multimod_compile, ['T3303', '-v0'])
test('T3741', normal, compile, [''])
test('DoAndIfThenElse', normal, compile, [''])
test('BlockArguments', normal, compile, [''])
test('BlockArgumentsArrowCmds', normal, compile, [''])
test('BlockArgumentsLambdaCase', normal, compile, [''])
test('NoBlockArguments', normal, compile, [''])
test('NondecreasingIndentation', normal, compile, [''])
test('mc15', normal, compile, [''])
test('mc16', normal, compile, [''])
test('EmptyDecls', normal, compile, [''])
test('ParserLambdaCase', [], compile, [''])
test('ParserArrowLambdaCase', [], compile, [''])

test('ColumnPragma', normal, compile, [''])
test('T5243', [], multimod_compile, ['T5243', ''])
test('T7118', normal, compile, [''])
test('T7776', normal, compile, [''])
test('RdrNoStaticPointers01', [], compile, [''])
test('T5682', normal, compile, [''])
test('T8258', normal, compile, [''])
test('T8708', normal, compile, [''])
test('T9723a', normal, compile, [''])
test('T9723b', normal, compile, [''])
test('T10188', normal, compile, [''])
test('VtaParse', normal, compile, [''])
test('T10196', normal, compile, [''])
test('T10379', normal, compile, [''])
test('T10582', expect_broken(10582), compile, [''])
test('T11622', normal, compile, [''])
test('DumpParsedAst',      normal, compile, ['-dsuppress-uniques -ddump-parsed-ast'])
test('DumpRenamedAst',     normal, compile, ['-dsuppress-uniques -ddump-rn-ast'])
test('DumpTypecheckedAst', normal, compile, ['-dsuppress-uniques -ddump-tc-ast'])
test('T12045e', normal, compile, [''])
test('T13087', normal, compile, [''])
test('T13747', normal, compile, [''])
test('T14189',     normal, compile, ['-dsuppress-uniques -ddump-rn-ast'])
test('T13986', normal, compile, [''])
test('T10855', normal, compile, [''])
test('T13600a', normal, compile, [''])
test('T13600b', normal, compile, ['-Wno-missing-signatures -Wno-unused-matches'])
test('T15139', normal, compile, ['-Wincomplete-patterns -fdiagnostics-show-caret'])
test('T15323', normal, compile, ['-dsuppress-uniques -ddump-parsed-ast'])
test('KindSigs', normal, compile, ['-dsuppress-uniques -ddump-parsed-ast'])


def only_MG_loc(x):
    """
    Only compares the location embedded inside the MatchGroup, which has the form
    (MG
      (NoExtField)
      (L
       (SrcSpanAnn (ApiAnnNotUsed) { <location> })
    """
    ls = x.split("\n")
    mgLocs = (loc.strip() for (mg,loc) in zip(ls,ls[3:])
                if mg.strip().startswith("(MG"))
    return '\n'.join(mgLocs)
test('T15279', normalise_errmsg_fun(only_MG_loc), compile, [''])
test('T15457', normal, compile, [''])
test('T15675', normal, compile, [''])
test('T15781', normal, compile, [''])
test('T16339', normal, compile, [''])
test('T16619', req_th, multimod_compile, ['T16619', '-v0'])
test('T504', normal, compile, [''])
test('T515', literate, compile, ['-Wall'])
test('T1087', normal, compile, [''])
test('proposal-229a', normal, compile, [''])
test('proposal-229b', normal, compile, [''])
test('proposal-229d', normal, compile, [''])
test('proposal-229e', normal, compile, [''])
test('LexicalNegation', normal, compile, [''])
test('NegativeLiterals', normal, compile, [''])
test('NegativeLiteralsNoExt', normal, compile, [''])

# We omit 'profasm' because it fails with:
# Cannot load -prof objects when GHC is built with -dynamic
#     To fix this, either:
#       (1) Use -fexternal-interpreter, or
#       (2) Build the program twice: once with -dynamic, and then
#           with -prof using -osuf to set a different object file suffix.
test('proposal-229f',
     [ extra_files(['proposal-229f.hs', 'Proposal229f_instances.hs']),
       omit_ways(['profasm', 'profthreaded'])
     ],
     multimod_compile_and_run, ['proposal-229f.hs', ''])

test('T15730a', normal, compile_and_run, [''])
test('T18130', normal, compile, [''])
test('T18834a', normal, compile, [''])
test('T18834b', normal, compile, [''])
test('T12862', normal, compile, [''])
