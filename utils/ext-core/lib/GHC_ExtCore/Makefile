all:	Handle.hs IO.hs Unicode.hs
	ghc -c -fext-core -package-name base-extcore Handle.hs IO.hs Unicode.hs -cpp -i../

clean:
	rm -f *.hcr *.hi *.o
