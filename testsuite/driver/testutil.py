# -----------------------------------------------------------------------------
# Utils

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
