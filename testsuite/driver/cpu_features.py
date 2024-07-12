import os
from testglobals import config
import subprocess
import re

# Feature names generally follow the naming used by Linux's /proc/cpuinfo.
SUPPORTED_CPU_FEATURES = {
    # These aren't comprehensive; they are only CPU features that we care about

    # x86:
    'sse', 'sse2', 'sse3', 'ssse3', 'sse4_1', 'sse4_2',
    'avx', 'avx2', 'avx512f',
    'fma',
    'popcnt', 'bmi1', 'bmi2'
}

cpu_feature_cache = None

def get_cpu_features():
    try:
        # This import might fail, e.g. if "ctypes" is not available,
        # in which case we report the empty set of features.
        import cpuinfo
        info = cpuinfo.get_cpu_info()
        features = info.get('flags', [])
        return features
    except:
        print('get_cpu_features: Lacking support for your platform')
        return []

def have_cpu_feature(feature):
    """
    A testsuite predicate for testing the availability of CPU features.
    """
    assert feature in SUPPORTED_CPU_FEATURES
    global cpu_feature_cache
    if cpu_feature_cache is None:
        cpu_feature_cache = get_cpu_features()
        print('Found CPU features:', ' '.join(cpu_feature_cache))

    return feature in cpu_feature_cache


if __name__ == '__main__':
    import sys
    config.os = sys.argv[1]
    print(get_cpu_features())
