
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
	-find . -name Makefile                      -exec echo $(WHERE_AM_I)/{} \; >> $(BIN_DIST_LIST) 2> /dev/null
	-find . -name LICENSE                       -exec echo $(WHERE_AM_I)/{} \; >> $(BIN_DIST_LIST) 2> /dev/null
	# Libraries
	-find $(LIB_DIST_DIR)/setup-config          -exec echo $(WHERE_AM_I)/{} \; >> $(BIN_DIST_LIST) 2> /dev/null
	-find $(LIB_DIST_DIR)/installed-pkg-config  -exec echo $(WHERE_AM_I)/{} \; >> $(BIN_DIST_LIST) 2> /dev/null
	-find $(LIB_DIST_DIR)/build -name "HS*.o"   -exec echo $(WHERE_AM_I)/{} \; >> $(BIN_DIST_LIST) 2> /dev/null
	-find $(LIB_DIST_DIR)/build -name "HS*.p_o" -exec echo $(WHERE_AM_I)/{} \; >> $(BIN_DIST_LIST) 2> /dev/null
	-find $(LIB_DIST_DIR)/build -name "*.a"     -exec echo $(WHERE_AM_I)/{} \; >> $(BIN_DIST_LIST) 2> /dev/null
	-find $(LIB_DIST_DIR)/build -name "*.p_a"   -exec echo $(WHERE_AM_I)/{} \; >> $(BIN_DIST_LIST) 2> /dev/null
	-find $(LIB_DIST_DIR)/build -name "*.hi"    -exec echo $(WHERE_AM_I)/{} \; >> $(BIN_DIST_LIST) 2> /dev/null
	-find $(LIB_DIST_DIR)/build -name "*.p_hi"  -exec echo $(WHERE_AM_I)/{} \; >> $(BIN_DIST_LIST) 2> /dev/null
	-find include -name "*.h"                   -exec echo $(WHERE_AM_I)/{} \; >> $(BIN_DIST_LIST) 2> /dev/null
	# Executables
	-find . -name "*.wrapper"                   -exec echo $(WHERE_AM_I)/{} \; >> $(BIN_DIST_LIST) 2> /dev/null
	-find $(EXE_DIST_DIR)/setup-config          -exec echo $(WHERE_AM_I)/{} \; >> $(BIN_DIST_LIST) 2> /dev/null
	-find $(EXE_DIST_DIR) -type f -perm /a+x    -exec echo $(WHERE_AM_I)/{} \; >> $(BIN_DIST_LIST) 2> /dev/null
	# Docs
	# This gives us both docbook docs, and haddock docs
	find . -name "*.haddock"                    -exec echo $(WHERE_AM_I)/{} \; >> $(BIN_DIST_LIST) 2> /dev/null
	find . -name "*.html"                       -exec echo $(WHERE_AM_I)/{} \; >> $(BIN_DIST_LIST) 2> /dev/null
	find . -name "*.css"                        -exec echo $(WHERE_AM_I)/{} \; >> $(BIN_DIST_LIST) 2> /dev/null
	find . -name "*.gif"                        -exec echo $(WHERE_AM_I)/{} \; >> $(BIN_DIST_LIST) 2> /dev/null
	find . -name "*.js"                         -exec echo $(WHERE_AM_I)/{} \; >> $(BIN_DIST_LIST) 2> /dev/null
	# And anything else
ifneq "$(BINDIST_EXTRAS)" ""
	for FILE in $(BINDIST_EXTRAS); do if [ -e $$FILE ]; then echo $(WHERE_AM_I)/$$FILE >> $(BIN_DIST_LIST); fi; done
endif

