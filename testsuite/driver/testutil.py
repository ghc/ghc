# -----------------------------------------------------------------------------
# Utils
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

