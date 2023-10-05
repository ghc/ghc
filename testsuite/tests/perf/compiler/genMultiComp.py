#! /usr/bin/env python

# Generates a set of interdependent units for testing any obvious performance cliffs
# with multiple component support.
# The structure of each unit is:
# * A Top module, which imports the rest of the modules in the unit
# * A number of modules names Mod_<pid>_<mid>, each module imports all the top
#   modules beneath it, and all the modules in the current unit beneath it.

import os
import stat

modules_per = 20
packages = 20
total = modules_per * packages

def unit_dir(p):
    return "p" + str(p)

def unit_fname(p):
    return "unitp" + str(p)

def top_fname(p):
    return "Top" + str(p)

def mod_name(p, k):
    return "Mod_%d_%d" % (p, k)

def flatten(t):
    return [item for sublist in t for item in sublist]

def mk_unit_file(p):
    fname = top_fname(p)
    deps = flatten([["-package-id", unit_dir(k)] for k in range(p)])
    opts = ["-working-dir", unit_dir(p), "-this-unit-id", unit_dir(p), fname] + deps
    with open(unit_fname(p), 'w') as fout:
        fout.write(' '.join(opts))

def mk_top_mod(p):
    pdir = unit_dir(p)
    topfname = os.path.join(pdir, top_fname(p) + '.hs')
    header = 'module %s where' % top_fname(p)
    imports = ['import %s' % mod_name(p, m) for m in range(modules_per)]
    with open(topfname, 'w') as fout:
        fout.write(header + '\n')
        fout.write('\n'.join(imports))

def mk_mod(p, k):
    pdir = unit_dir(p)
    fname = os.path.join(pdir, mod_name(p, k) + '.hs')
    header = 'module %s where' % mod_name(p,k)
    imports1 = ['import ' + top_fname(pn) for pn in range(p)]
    imports2 = ['import ' + mod_name(p, kn) for kn in range(k)]
    with open(fname, 'w') as fout:
        fout.write(header + '\n')
        fout.write('\n'.join(imports1))
        fout.write('\n')
        fout.write('\n'.join(imports2))

def mk_run():
    all_units = flatten([['-unit', '@'+unit_fname(pn)] for pn in range(packages)])
    with open('run', 'w') as fout:
        fout.write("$TEST_HC $TEST_HC_OPTS -fno-code -fwrite-interface ")
        fout.write(" ".join(all_units))

    st = os.stat('run')
    os.chmod('run', st.st_mode | stat.S_IEXEC)


for p in range(packages):
    os.mkdir(unit_dir(p))
    mk_unit_file(p)
    mk_top_mod(p)
    for k in range(modules_per):
        mk_mod(p, k)
mk_run()


