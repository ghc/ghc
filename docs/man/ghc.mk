
ifeq "$(BUILD_MAN)" ""
ifeq "$(strip $(XSLTPROC))" ""
BUILD_MAN = NO
else
BUILD_MAN = YES
endif
endif

# The commands which should be mentioned in the man page
MAN_GHC_COMMANDS = ghc ghci

# The man page we are generating
MAN_PAGE = ghc

# The manual section
MAN_SECTION = 1

MAN_PATH = docs/man/$(MAN_PAGE).$(MAN_SECTION)

ifneq "$(BINDIST)" "YES"
$(MAN_PATH): docs/man/flags.xsl docs/man/flags.xml
	$(XSLTPROC) $(XSLTPROC_OPTS) $^ > $@
endif

# Insert the commands and the library directory into the man page
docs/man/flags.xsl: docs/man/gen_flags.xsl.sh
	$(SHELL) $< "$(MAN_GHC_COMMANDS)" "$(libdir)" > $@

# Re-use the flags documentation from the user's guide by injecting some
# entities after the XML declaration to make it a stand-alone document.
docs/man/flags.xml: docs/users_guide/flags.xml
	$(call removeFiles,$@)
	head -n 1 $< >> $@
	echo "<!DOCTYPE sect1 [<!ENTITY ndash  \"-\"> \
	                       <!ENTITY ldquo  \"\`\"> \
	                       <!ENTITY rdquo  \"'\">]>" >> $@
# "sed 1d" == "tail -n +2", but Solaris apparently rejects the latter
	sed 1d $< >> $@

ifeq "$(BUILD_MAN)" "YES"
ifeq "$(phase)" "final"
$(eval $(call all-target,docs/man,$(MAN_PATH)))
endif

INSTALL_MANPAGES += $(MAN_PATH)

install: install_man

.PHONY: install_man
install_man: $(MAN_PATH)
	$(call INSTALL_DIR,"$(DESTDIR)$(mandir)")
	$(call INSTALL_DIR,"$(DESTDIR)$(mandir)/man$(MAN_SECTION)")
	$(call INSTALL_MAN,$(INSTALL_OPTS),$(MAN_PATH),"$(DESTDIR)$(mandir)/man$(MAN_SECTION)")
endif

$(eval $(call clean-target,docs/man,,$(MAN_PATH) docs/man/flags.xsl docs/man/flags.xml))

