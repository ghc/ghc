
# Here's an interesting rule!
# The .hi file depends on the .o file,
# so if the .hi file is dated earlier than the .o file (commonly the case,
# when interfaces are stable) this rule just makes sure that the .o file,
# is up to date.  Then it does nothing to generate the .hi file from the
# .o file, because the act of making sure the .o file is up to date also
# updates the .hi file (if necessary).

define hi-rule # $1 = way
%.$$($1_hisuf) : %.$$($1_osuf)
	@if [ ! -f $$@ ] ; then \
	    echo Panic! $$< exists, but $$@ does not.; \
	    exit 1; \
	else exit 0 ; \
	fi

%.$$($1_way_)hi-boot : %.$$($1_way_)o-boot
	@if [ ! -f $$@ ] ; then \
	    echo Panic! $$< exists, but $$@ does not.; \
	    exit 1; \
	else exit 0 ; \
	fi
endef

