# SHADE presumably set by an environment variable
#
SPIX   = ${SHADE}/spixtools
#
SHADE_L = ${SHADE}/lib
SPIX_L  = ${SPIX}/lib

SHADE_H = ${SHADE}/src/include
SPIX_H  = ${SPIX}/src/include
GHC_H	= ${bghc}/includes

CC=gcc
CFLAGS = -I${GHC_H} -I${SHADE_H} -I${SPIX_H} -g -O -ansi

.c.o:
	${CC} ${CFLAGS} -c $*.c
	
.o:
	${CC} -static -o $* spatmain.o $*.o ${SHADE_L}/libshade.a ${SPIX_L}/libspix.a
