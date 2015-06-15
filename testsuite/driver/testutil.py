# -----------------------------------------------------------------------------
# Utils

import subprocess

def version_to_ints(v):
    return [ int(x) for x in v.split('.') ]

def version_lt(x, y):
    return version_to_ints(x) < version_to_ints(y)

def version_le(x, y):
    return version_to_ints(x) <= version_to_ints(y)

def version_gt(x, y):
    return version_to_ints(x) > version_to_ints(y)

def version_ge(x, y):
    return version_to_ints(x) >= version_to_ints(y)

def strip_quotes(s):
    # Don't wrap commands to subprocess.call/Popen in quotes.
    return s.strip('\'"')

def getStdout(cmd_and_args):
    # Can't use subprocess.check_output as it's not available in Python 2.6;
    # It's also not quite the same as check_output, since we also verify that
    # no stderr was produced
    p = subprocess.Popen([strip_quotes(cmd_and_args[0])] + cmd_and_args[1:],
                         stdout=subprocess.PIPE,
                         stderr=subprocess.PIPE)
    (stdout, stderr) = p.communicate()
    r = p.wait()
    if r != 0:
        raise Exception("Command failed: " + str(cmd_and_args))
    if stderr != '':
        raise Exception("stderr from command: " + str(cmd_and_args))
    return stdout
