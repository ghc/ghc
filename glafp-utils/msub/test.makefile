foo = ${bar}
bar = ${ugh}
ugh = Huh?
space = x   
a = ${wildcard x}

c :	${xyz}

xyz = abc


dollar = $$
doubledollar = ${dollar}${dollar}

X1 = Y
X2 = 1
Y1 = y1
Y2 = ${${X1}${X2}}
