define c-objs  # args: $1 = dir, $2 = distdir
# C and S files are built only once, not once per way
$1_$2_C_OBJS = $$(patsubst %.c,$1/$2/build/%.$$(v_osuf),$$($1_$2_C_SRCS))
$1_$2_S_OBJS = $$(patsubst %.S,$1/$2/build/%.$$(v_osuf),$$($1_$2_S_SRCS))
endef
