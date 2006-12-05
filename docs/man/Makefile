TOP = ../..
include $(TOP)/mk/boilerplate.mk

GHC_COMMANDS=ghc ghci

UNCENSORED_MANPAGES=ghc.1

ifeq '$(GhcManpages)' 'YES'
MANPAGES=$(UNCENSORED_MANPAGES)
else
MANPAGES=
endif

boot:
	@:

all: $(MANPAGES)
	@:

install: $(addprefix install-,$(MANPAGES))
	@:

clean:
	rm -f flags.xml flags.xsl $(UNCENSORED_MANPAGES)

# The work for building

ghc.1: flags.xsl flags.xml
	$(XSLTPROC) $(XSLTPROC_OPTS) flags.xsl flags.xml > $@

flags.xsl: gen_flags.xsl.pl
	perl gen_flags.xsl.pl "$(GHC_COMMANDS)" "$(libdir)" > $@

flags.xml: ../users_guide/flags.xml
	rm -f $@
	head -1 $< >> $@
	echo "<!DOCTYPE sect1 [<!ENTITY ndash  \"-\"> \
	                       <!ENTITY ldquo  \"\`\"> \
	                       <!ENTITY rdquo  \"'\">]>" >> $@
	tail -n +2 $< >> $@

# The work for installing

.PHONY: install-%

install-mandir:
	$(INSTALL_DIR) $(mandir)

install-%: install-mandir $*
	$(INSTALL_MAN) $* $(mandir)

