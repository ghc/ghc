
define includes-sources # args: $1 = dir, $2 = distdir

ifeq "$$($1_$2_INCLUDE_DIRS)" ""
$1_$2_INCLUDE_DIRS = .
endif

$1_$2_INSTALL_INCLUDES_SRCS :=\
    $$(foreach file,$$($1_$2_INSTALL_INCLUDES),\
        $$(firstword \
            $$(wildcard \
                $$(foreach dir,$$($1_$2_INCLUDE_DIRS),\
                    $1/$$(dir)/$$(file)))))
endef
