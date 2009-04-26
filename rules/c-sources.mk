define c-sources  # args: $1 = dir, $2 = distdir
$1_$2_C_FILES = $$(patsubst %,$1/%,$$($1_$2_C_SRCS))
$1_$2_S_FILES = $$(patsubst %,$1/%,$$($1_$2_S_SRCS))
endef
