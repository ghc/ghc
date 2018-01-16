#!/nix/store/4wqa1whb42khic67rv9jgip30nxmaii8-python3-3.6.4/bin/python

import sys, time

def benchmark_once(f):
    start = time.time()
    f()
    end = time.time()
    return end - start

def benchmark(f):
    runs = 100
    total = 0.0
    for i in range(runs):
        result = benchmark_once(f)
        sys.stderr.write('Run {0}: {1}\n'.format(i, result))
        total += result
    return total / runs
