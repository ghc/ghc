import os
from testglobals import config

# Feature names generally follow the naming used by Linux's /proc/cpuinfo.
SUPPORTED_CPU_FEATURES = {
    # These aren't comprehensive; they are only CPU features that we care about

    # x86:
    'sse', 'sse2', 'sse3', 'ssse3', 'sse4_1', 'sse4_2',
    'avx1', 'avx2',
    'popcnt', 'bmi1', 'bmi2'
}

cpu_feature_cache = None

def get_cpu_features():
    if config.os in ['Windows', 'Linux'] and os.path.exists('/proc/cpuinfo'):
        import re
        f = open('/proc/cpuinfo').read()
        flags = re.search(r'flags\s*:\s*.*$', f, re.M)
        if flags is None:
            print('get_cpu_features: failed to find cpu features')
            return {}
        flags = set(flags[0].split())
        if 'pni' in flags:
            flags.add('sse3')
            flags.remove('pni')
        return flags

    elif config.os == 'Darwin':
        import subprocess
        out, err = subprocess.check_output(['sysctl', 'hw'])
        features = {}
        def check_feature(darwin_name, our_name=None):
            if re.find(r'hw\.optional.%s:\s*1' % darwin_name, out):
                features.add(darwin_name if our_name is None else our_name)

        for feature in SUPPORTED_CPU_FEATURES:
            check_feature(feature)

        # A few annoying cases
        check_feature('avx1_0', 'avx1')
        check_feature('avx2_0', 'avx2')
        return features

    else:
        # TODO: Add {Open,Free}BSD support
        print('get_cpu_features: Lacking support for your platform')

    return {}

def have_cpu_feature(feature):
    """
    A testsuite predicate for testing the availability of CPU features.
    """
    assert feature in SUPPORTED_CPU_FEATURES
    if cpu_feature_cache is None:
        cpu_feature_cache = get_cpu_features()
        print('Found CPU features:', ' '.join(cpu_feature_cache))
        # Sanity checking
        assert all(feat in SUPPORTED_CPU_FEATURES
                   for feat in cpu_feature_cache)

    return feature in cpu_feature_cache


if __name__ == '__main__':
    import sys
    config.os = sys.argv[1]
    print(get_cpu_features())

