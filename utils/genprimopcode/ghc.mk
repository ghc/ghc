utils/genprimopcode_dist_MODULES = Lexer Main ParserM Parser Syntax
utils/genprimopcode_dist_PROG    = $(GHC_GENPRIMOP_PGM)

$(eval $(call build-prog,utils/genprimopcode,dist,0))
