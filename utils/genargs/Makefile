comma = ,
BAR= "-L\"foo bar\""
FOO= $(patsubst %,$(comma)"%",$(BAR))

test:
	@echo "$(FOO)"
	@echo "$(BAR)" | $(PERL) genargs.pl -comma
	@echo
