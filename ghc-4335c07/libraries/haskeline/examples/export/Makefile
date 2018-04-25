
all: main

HaskelineExport.o: HaskelineExport.hs
	ghc --make -XForeignFunctionInterface HaskelineExport.hs

main: HaskelineExport.o main.c
	ghc -no-hs-main -package haskeline -o main \
		HaskelineExport.o HaskelineExport_stub.o \
		main.c -package haskeline

clean:
	rm -f *.o *.hi *_stub.* main
