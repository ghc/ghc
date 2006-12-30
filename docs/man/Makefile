TOP = ../..
include $(TOP)/mk/boilerplate.mk

# The commands which should be mentioned in the man page
GHC_COMMANDS = ghc ghci

# The man page we are generating
MAN_PAGE = ghc

# The manual section
MAN_SECTION = 1

CLEAN_FILES += $(MAN_PAGE).$(MAN_SECTION) flags.xsl flags.xml

$(MAN_PAGE).$(MAN_SECTION): flags.xsl flags.xml
	$(XSLTPROC) $(XSLTPROC_OPTS) flags.xsl flags.xml > $@

# Insert the commands and the library directory into the man page
flags.xsl: gen_flags.xsl.pl
	perl gen_flags.xsl.pl "$(GHC_COMMANDS)" "$(libdir)" > $@

# Re-use the flags documentation from the user's guide by injecting some
# entities after the XML declaration to make it a stand-alone document.
flags.xml: ../users_guide/flags.xml
	$(RM) $@
	head -n 1 $< >> $@
	echo "<!DOCTYPE sect1 [<!ENTITY ndash  \"-\"> \
	                       <!ENTITY ldquo  \"\`\"> \
	                       <!ENTITY rdquo  \"'\">]>" >> $@
	tail -n +2 $< >> $@

install-docs:: $(MAN_PAGE).$(MAN_SECTION)
	$(INSTALL_DIR) $(mandir)
	$(INSTALL_DIR) $(mandir)/man$(MAN_SECTION)
	$(INSTALL_MAN) $(INSTALL_OPTS) $(MAN_PAGE).$(MAN_SECTION) $(mandir)/man$(MAN_SECTION)

include $(TOP)/mk/target.mk
