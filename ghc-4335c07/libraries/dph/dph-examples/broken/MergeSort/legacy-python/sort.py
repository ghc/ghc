def evenodd_mergesort(x):
    if len(x)<=1:
        return x
    else:
        first = evenodd_mergesort(x[:len(x)/2])
        second = evenodd_mergesort(x[len(x)/2:])
        return evenodd_merge(first + second)
 
def evenodd_merge(x):
    if len(x) == 2:
        if x[1]< x[0]: #swap 
          x[0], x[1] = x[1], x[0]
        return x        
    else:                                          
        e = evenodd_merge(extract(0,x))  #extract even indexed and sort
        o = evenodd_merge(extract(1,x))  #extract odd indexed and sort
        result = interleave(e,o)
        return evenodd_compare(result)   
 
def evenodd_compare(x):
    for i in range(1,len(x)-1,2):
        if x[i+1] < x[i]:  #swap
            x[i+1], x[i] = x[i],x[i+1]
    return x
 
def interleave(x,y):
    res = range(len(x)+len(y))
    for i in range(len(x)):
        res[2*i]=x[i]; res[2*i+1] = y[i]
    return res
 
def extract(start, x):
    res = range(len(x)/2)
    ind = range(start,len(x),2)
    for i in ind: res[i/2]=x[i]
    return res 
 