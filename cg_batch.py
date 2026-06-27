#!/usr/bin/env python3
"""Profile each GHC perf/compiler test under cgwork (cachegrind, Ir-only).

One raw cachegrind file per test lands in cg-out/<name>.cg, ready for
cg_aggregate.py. We profile the *prebuilt* compiler at
~/ghc/head0/_build/stage1/bin/ghc against head0's own testsuite sources, so the
binary and the test inputs match.

The per-test config table below is self-contained: it is authored from head0's
testsuite/tests/perf/compiler/all.T, not by driving the testsuite. For each test
we reconstruct the command `simple_build` would build
(testlib.py: `{compiler} {to_do} {srcname} {flags}`) and add:
  * the test's *way* flags when it pins a single way (only_ways(['optasm']) ->
    '-O -fasm'; only_ways(['normal']) or unpinned -> none, the 'normal' way),
  * `-fforce-recomp` so a warm .hi/.o can't short-circuit the compile,
  * `+RTS -V0 -I0 -RTS` for determinism (zero the timer, disable idle GC),
    matching the original T17904 run. GHC scans every +RTS..-RTS segment, so a
    test that already carries its own +RTS block keeps it.

We do NOT add the stats `-t`/`--machine-readable` opts: cgwork measures Ir, not
allocations.

Usage:
  cg_batch.py list                 # show the table (scope + command)
  cg_batch.py run [NAME...]        # profile all in-scope tests, or just NAME...
  cg_batch.py run --force NAME...  # re-profile even if the .cg exists
"""
import sys, os, json, shutil, subprocess, argparse, time

sys.path.insert(0, '/home/simon/prog/ghc-personal')
import cgwork

HEAD0 = '/home/simon/ghc/head0'
GHC   = HEAD0 + '/_build/stage1/bin/ghc'
PERF  = HEAD0 + '/testsuite/tests/perf/compiler'
CODEGEN = HEAD0 + '/testsuite/tests/codeGen/should_compile'
CGWORK = '/home/simon/prog/ghc-personal/cgwork.py'

HERE = os.path.dirname(os.path.abspath(__file__))
OUT  = os.path.join(HERE, 'cg-out')
WORK = os.path.join(OUT, 'work')

# Way flags a pinned test compiles with. Unpinned tests run in several ways;
# we profile the 'normal' (no-extra-flags) way as the single representative.
OPTASM = '-O -fasm'

def C(name, src, flags='', pre=None, xfail=False, files=None, srcdir=PERF):
    return dict(name=name, kind='compile', src=src + '.hs', flags=flags,
                pre=pre, xfail=xfail, files=files, srcdir=srcdir)

def CF(name, src, flags='', pre=None, files=None):
    d = C(name, src, flags, pre, xfail=True, files=files); return d

def M(name, root, flags, pre=None):
    return dict(name=name, kind='multimod', src=root, flags=flags,
                pre=pre, xfail=False, files=None, srcdir=PERF)

# --- The self-contained table -------------------------------------------------
# Smoke baseline: T17904 lives in codeGen/should_compile, compile ['-O'].
# Its known cgwork profile is ~70.6M COMPILER Ir (used by verification step 1).
TESTS = [
    C('T17904', 'T17904', '-O', files=['T17904.hs'], srcdir=CODEGEN),

    # single-module compile / compile_fail
    C('T1969', 'T1969', '-dcore-lint -static'),
    C('T3294', 'T3294'),
    C('T4801', 'T4801', '-static'),
    C('T3064', 'T3064'),
    C('T5030', 'T5030', '-freduction-depth=300'),
    C('T5631', 'T5631'),
    CF('parsing001', 'parsing001'),
    C('T783', 'T783', '-static'),
    C('T26426', 'T26426'),
    C('T5321Fun', 'T5321Fun'),
    C('T5321FD', 'T5321FD'),
    C('T5642', 'T5642', '-O'),
    C('T5837', 'T5837', '-freduction-depth=50'),
    C('T6048', 'T6048', OPTASM),
    C('T9020', 'T9020', OPTASM),
    C('T9675', 'T9675', OPTASM),
    CF('T9872a', 'T9872a'),
    CF('T9872b', 'T9872b'),
    C('T9872b_defer', 'T9872b_defer', '-fdefer-type-errors'),
    CF('T9872c', 'T9872c'),
    C('T9872d', 'T9872d'),
    C('T8095', 'T8095', '-v0 -O'),
    C('T13386', 'T13386', '-v0 -O0'),
    C('CoOpt_Read', 'CoOpt_Read', '-v0 -O'),
    C('CoOpt_Singletons', 'CoOpt_Singletons', '-v0 -O'),
    C('T9961', 'T9961', '-O'),
    C('T10370', 'T10370', OPTASM),
    CF('T10547', 'T10547', '-fprint-expanded-synonyms'),
    C('T12227', 'T12227', '-O2 -ddump-hi -ddump-to-file +RTS -M1G -RTS'),
    C('T12425', 'T12425', OPTASM),
    C('T12234', 'T12234', OPTASM),
    C('T13035', 'T13035'),
    C('T13056', 'T13056', OPTASM + ' -O1'),
    C('T12707', 'T12707'),
    C('T12150', 'T12150', OPTASM),
    C('T13379', 'T13379'),
    C('T13820', 'T13820', '-v0'),
    C('T13960', 'T13960', '-O'),
    C('T14766', 'T14766', '-v0', pre='python3 genT14766.py > T14766.hs'),
    C('T18223', 'T18223', '-v0 -O'),
    C('T18923', 'T18923', '-v0 -O'),
    C('T16577', 'T16577', '-v0 -O'),
    C('T9198', 'T9198'),
    C('T11545', 'T11545', '-O'),
    C('T15304', 'T15304', '-O'),
    C('T20049', 'T20049'),
    C('T19695', 'T19695', '-v0 -O2'),
    C('hard_hole_fits', 'hard_hole_fits',
      '-fdefer-type-errors -fno-max-valid-hole-fits -package ghc'),
    C('T16875', 'T16875',
      '-fdefer-type-errors -fno-max-valid-hole-fits -package ghc'),
    C('T20261', 'T20261'),
    C('T21839c', 'T21839c', '-O'),
    C('T18304', 'T18304', '-v0 -O'),
    C('T18282', 'T18282', '-v0 -O'),
    C('T18140', 'T18140', '-v0 -O'),
    C('T13253', 'T13253', '-v0 -O'),
    C('T13253-spj', 'T13253-spj', '-v0 -O'),
    C('T15164', 'T15164', '-v0 -O'),
    C('T15630', 'T15630', '-O2'),
    C('T15630a', 'T15630a', '-O2'),
    C('WWRec', 'WWRec', '-v0 -O'),
    C('T24582', 'T24582', '-O'),
    C('MultilineStringsPerf', 'MultilineStringsPerf', '-O'),
    C('T24984', 'T24984', '-O'),
    C('T26425', 'T26425', '-O'),

    # multi-module --make
    M('T15703', 'T15703', '-v0 -O'),
    M('T18730', 'T18730', OPTASM + ' -v0 -O'),
    M('LargeRecord', 'LargeRecord', '-v0 -O'),
    M('T9233', 'T9233', '-v0 -O2 -fno-spec-constr'),
    M('T12545', 'T12545', '-v0'),
    M('MultiLayerModules', 'MultiLayerModules', '-v0 +RTS -ki2k -kb2k -RTS',
      pre='./genMultiLayerModules'),
    M('ManyConstructors', 'ManyConstructors', '-v0', pre='./genManyConstructors'),
    M('ManyAlternatives', 'ManyAlternatives', '-v0', pre='./genManyAlternatives'),
    M('T13701', 'T13701', '-v0 +RTS -ki2k -kb2k -RTS', pre='./genT13701'),
    M('T13719', 'T13719', '-v0', pre='./genT13719'),
    M('T14683', 'T14683', '-v0'),
    M('T9630', 'T9630', '-v0 -O'),
    M('T10421', 'T10421', '-v0 -O'),
    M('T10421a', 'T10421a', '-v0 -O'),
    M('InstanceMatching1', 'Defs', '-fno-code -fwrite-interface -v0',
      pre='./genMatchingTest 1'),
    M('RecordUpdPerf', 'RecordUpdPerf', '-fno-code -v0', pre='./genRecordUpdPerf'),
    M('T26989', 'T26989', '-v0 -O'),
]

# Out of scope for this batch; see aggregate.md. cgwork picks a single largest
# ghc process, which doesn't represent these cleanly:
#   makefile_test    : T4007, T11068
#   ghci_script      : MultiLayerModulesDefsGhci{,WithCore,WithBytecodeFiles,Reload},
#                      MultiLayerModulesNoCode, interpreter_steplocal
#   multiunit_compile: MultiComponentModules{,Recomp,100,Recomp100}
#   TH (req_th/iserv): MultiLayerModulesTH_{Make,OneShot}, T16190, T22744, T24471
#   recomp / $MAKE   : MultiLayerModulesRecomp, MultiLayerModulesDefsGhcWithCore,
#                      InstanceMatching
#   compile_and_run  : T16473, T25723, InfiniteListFusion, LookupFusion
#   huge cmdline     : T14697 ($(cat T14697-flags), 10k -optP args)
OUT_OF_SCOPE = ['T4007', 'T11068', 'interpreter_steplocal', 'T14697',
                'T16190', 'T22744', 'T24471', 'T16473', 'T25723',
                'InfiniteListFusion', 'LookupFusion']

def to_do(kind):
    return '-c' if kind in ('compile',) or kind == 'compile_fail' else '--make'

def ghc_cmd(t):
    td = '--make' if t['kind'] == 'multimod' else '-c'
    parts = [GHC, td, t['src']] + t['flags'].split() + \
            ['-fforce-recomp', '+RTS', '-V0', '-I0', '-RTS']
    return parts

def prep_workdir(t):
    wd = os.path.join(WORK, t['name'])
    if os.path.exists(wd):
        shutil.rmtree(wd)
    os.makedirs(wd)
    if t['files']:
        for f in t['files']:
            shutil.copy(os.path.join(t['srcdir'], f), wd)
    else:
        # copy the whole perf/compiler source tree (sources + generators); --make
        # only follows imports from the root module, so extra files are harmless.
        shutil.copytree(t['srcdir'], wd, dirs_exist_ok=True,
                        ignore=shutil.ignore_patterns('*.cg', 'cg-out', 'work'))
    return wd

def run_one(t, force):
    cg = os.path.join(OUT, t['name'] + '.cg')
    if os.path.exists(cg) and not force:
        print(f"= {t['name']}: skip (cg exists)")
        return None
    wd = prep_workdir(t)
    if t['pre']:
        r = subprocess.run(t['pre'], shell=True, cwd=wd)
        if r.returncode != 0:
            print(f"! {t['name']}: pre_cmd failed ({t['pre']})")
            return dict(name=t['name'], ok=False, stage='pre_cmd')
    cmd = ghc_cmd(t)
    vg = [sys.executable, CGWORK, 'run', cg, '--'] + cmd
    print(f"+ {t['name']}: {' '.join(cmd)}")
    t0 = time.time()
    r = subprocess.run(vg, cwd=wd)
    dt = time.time() - t0
    # compile_fail returns non-zero by design; the .cg is still produced.
    if not os.path.exists(cg):
        print(f"! {t['name']}: no .cg produced (rc={r.returncode})")
        return dict(name=t['name'], ok=False, stage='cgwork', rc=r.returncode)
    fns, summary = cgwork.parse(cg)
    gc, rts, comp = cgwork.buckets(fns)
    rec = dict(name=t['name'], ok=True, rc=r.returncode, xfail=t['xfail'],
               cmd=' '.join(cmd), seconds=round(dt, 1),
               total=gc + rts + comp, gc=gc, rts=rts, comp=comp)
    print(f"  -> COMPILER {comp:,}  ({dt:.0f}s)")
    return rec

def load_manifest():
    p = os.path.join(OUT, 'manifest.json')
    if os.path.exists(p):
        return json.load(open(p))
    return {}

def save_manifest(m):
    json.dump(m, open(os.path.join(OUT, 'manifest.json'), 'w'), indent=2)

def cmd_list():
    for t in TESTS:
        print(f"{t['name']:24} {t['kind']:12} {' '.join(ghc_cmd(t)[1:])}")
    print(f"\n{len(TESTS)} in scope; out of scope: {', '.join(OUT_OF_SCOPE)}")

def cmd_run(names, force):
    os.makedirs(OUT, exist_ok=True)
    os.makedirs(WORK, exist_ok=True)
    sel = TESTS if not names else [t for t in TESTS if t['name'] in names]
    if names and len(sel) != len(names):
        missing = set(names) - {t['name'] for t in sel}
        sys.exit(f"unknown test(s): {', '.join(sorted(missing))}")
    man = load_manifest()
    for t in sel:
        rec = run_one(t, force)
        if rec is not None:
            man[t['name']] = rec
            save_manifest(man)
    print(f"\nmanifest: {os.path.join(OUT, 'manifest.json')}")

def main():
    ap = argparse.ArgumentParser()
    sub = ap.add_subparsers(dest='cmd', required=True)
    sub.add_parser('list')
    rp = sub.add_parser('run')
    rp.add_argument('names', nargs='*')
    rp.add_argument('--force', action='store_true')
    a = ap.parse_args()
    if a.cmd == 'list':
        cmd_list()
    else:
        cmd_run(a.names, a.force)

if __name__ == '__main__':
    main()
