# -----------------------------------------------------------------------------
# Utils

def append(x,y):
    return x + y

def concat(xs):
    return reduce(append,xs)

def chop(s):
    if s[len(s)-1:] == '\n':
        return s[:len(s)-1]
    else:
        return s
    
