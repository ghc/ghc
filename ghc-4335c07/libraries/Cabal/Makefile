.PHONY : all lexer lib exe doctest

LEXER_HS:=Cabal/Distribution/Parsec/Lexer.hs

all : exe lib

lexer : $(LEXER_HS)

$(LEXER_HS) : boot/Lexer.x
	alex --latin1 --ghc -o $@ $^

lib : $(LEXER_HS)
	cabal new-build --enable-tests Cabal

exe : $(LEXER_HS)
	cabal new-build  --enable-tests cabal

doctest :
	doctest --fast Cabal/Distribution Cabal/Language
