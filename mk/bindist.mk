
LIB_DIST_DIR = dist
EXE_DIST_DIR = dist-install

# XXX This is a bit of a mess. Really we should be just asking Cabal
# what it would install and putting those files in the bindist.
binary-dist:
ifeq "$(WHERE_AM_I)" ""
	echo "I don't know where I am" >&2
	exit 1
endif
	# General bits
	-$(FIND) . -name Makefile                      -exec echo $(WHERE_AM_I)/{} \; >> $(BIN_DIST_LIST) 2> /dev/null
	-$(FIND) . -name LICENSE                       -exec echo $(WHERE_AM_I)/{} \; >> $(BIN_DIST_LIST) 2> /dev/null
	# Libraries
	-$(FIND) $(LIB_DIST_DIR)/setup-config          -exec echo $(WHERE_AM_I)/{} \; >> $(BIN_DIST_LIST) 2> /dev/null
	-$(FIND) $(LIB_DIST_DIR)/installed-pkg-config  -exec echo $(WHERE_AM_I)/{} \; >> $(BIN_DIST_LIST) 2> /dev/null
	-$(FIND) $(LIB_DIST_DIR)/build -name "HS*.o"   -exec echo $(WHERE_AM_I)/{} \; >> $(BIN_DIST_LIST) 2> /dev/null
	-$(FIND) $(LIB_DIST_DIR)/build -name "HS*.p_o" -exec echo $(WHERE_AM_I)/{} \; >> $(BIN_DIST_LIST) 2> /dev/null
	-$(FIND) $(LIB_DIST_DIR)/build -name "*.a"     -exec echo $(WHERE_AM_I)/{} \; >> $(BIN_DIST_LIST) 2> /dev/null
	-$(FIND) $(LIB_DIST_DIR)/build -name "*.p_a"   -exec echo $(WHERE_AM_I)/{} \; >> $(BIN_DIST_LIST) 2> /dev/null
	-$(FIND) $(LIB_DIST_DIR)/build -name "*.hi"    -exec echo $(WHERE_AM_I)/{} \; >> $(BIN_DIST_LIST) 2> /dev/null
	-$(FIND) $(LIB_DIST_DIR)/build -name "*.p_hi"  -exec echo $(WHERE_AM_I)/{} \; >> $(BIN_DIST_LIST) 2> /dev/null
	-$(FIND) include -name "*.h"                   -exec echo $(WHERE_AM_I)/{} \; >> $(BIN_DIST_LIST) 2> /dev/null
	# Executables
	-$(FIND) . -name "*.wrapper"                   -exec echo $(WHERE_AM_I)/{} \; >> $(BIN_DIST_LIST) 2> /dev/null
	-$(FIND) $(EXE_DIST_DIR)/setup-config          -exec echo $(WHERE_AM_I)/{} \; >> $(BIN_DIST_LIST) 2> /dev/null
	# We want the executable files, which in theory would be -perm /a+x
	# ("any execute bit is set") but that doesn't work on some solaris
	# and OS X machines, so we use -perm -100 instead ("the user execute
	# bit is set"). In practice, this is extremely unlikely not to be the
	# same set of files.
	-$(FIND) $(EXE_DIST_DIR) -type f -perm -100    -exec echo $(WHERE_AM_I)/{} \; >> $(BIN_DIST_LIST) 2> /dev/null
	# Docs
	# This gives us both docbook docs, and haddock docs
	$(FIND) . -name "*.haddock"                    -exec echo $(WHERE_AM_I)/{} \; >> $(BIN_DIST_LIST) 2> /dev/null
	$(FIND) . -name "*.html"                       -exec echo $(WHERE_AM_I)/{} \; >> $(BIN_DIST_LIST) 2> /dev/null
	$(FIND) . -name "*.css"                        -exec echo $(WHERE_AM_I)/{} \; >> $(BIN_DIST_LIST) 2> /dev/null
	$(FIND) . -name "*.gif"                        -exec echo $(WHERE_AM_I)/{} \; >> $(BIN_DIST_LIST) 2> /dev/null
	$(FIND) . -name "*.js"                         -exec echo $(WHERE_AM_I)/{} \; >> $(BIN_DIST_LIST) 2> /dev/null
	# And anything else
ifneq "$(strip $(BINDIST_EXTRAS))" ""
	for FILE in $(BINDIST_EXTRAS); do if [ -f $$FILE ]; then echo $(WHERE_AM_I)/$$FILE >> $(BIN_DIST_LIST); fi; done
endif

