# -----------------------------------------------------------------------------
# Utils

def id(a):
    return a

def eq(x):
    return lambda y,z=x: y == z

def neq(x):
    return lambda y,z=x: y != z

def append(x,y):
    return x + y

def concat(xs):
    return reduce(append,xs,[])

def chop(s):
    if s[len(s)-1:] == '\n':
        return s[:len(s)-1]
    else:
        return s
    
def all(p,xs):
    for x in xs:
        if not p(x):
            return False
    return True

def elem(xs):
    return lambda x: x in xs

def notElem(xs):
    return lambda x: x not in xs

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

