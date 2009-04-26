
define c-suffix-rules 
# $1 = dir
# $2 = distdir
# $3 = way
# $4 = use GHC (YES/NO)

# UseGhcForCc is only relevant when not booting from HC files.
ifeq "$4" "YES"

$1/$2/build/%.$$($3_osuf) : $1/%.c $$(MKDIRHIER) $$($1_$2_HC_DEP)
	$$(RM) $$@
	$$(MKDIRHIER) $$(dir $$@)
	$$($1_$2_HC) $$($1_$2_$3_GHC_CC_OPTS) -c $$< -o $$@

$1/$2/build/%.$$($3_osuf) : $1/$2/build/%.c $$($1_$2_HC_DEP)
	$$(RM) $$@
	$$($1_$2_HC) $$($1_$2_$3_GHC_CC_OPTS) -c $$< -o $$@

$1/$2/build/%.$$($3_osuf) : $1/$2/build/%.$$($3_way_)s $$($1_$2_HC_DEP)
	$$(RM) $$@
	$$($1_$2_HC) $$($1_$2_$3_GHC_CC_OPTS) -c $$< -o $$@

$1/$2/build/%.$$($3_osuf) : $1/%.S $$(MKDIRHIER) $$($1_$2_HC_DEP)
	$$(RM) $$@
	$$(MKDIRHIER) $$(dir $$@)
	$$($1_$2_HC) $$($1_$2_$3_GHC_CC_OPTS) -c $$< -o $$@

$1/$2/build/%.$$($3_way_)s : $1/$2/build/%.c $$($1_$2_HC_DEP)
	$$(RM) $$@
	$$($1_$2_HC) $$($1_$2_$3_GHC_CC_OPTS) -S $$< -o $$@

else

$1/$2/build/%.$$($3_osuf) : $1/%.c $$(MKDIRHIER)
	$$(RM) $$@
	$$(MKDIRHIER) $$(dir $$@)
	$$(CC) $$($1_$2_$3_ALL_CC_OPTS) -c $$< -o $$@

$1/$2/build/%.$$($3_osuf) : $1/$2/build/%.c
	$$(RM) $$@
	$$(CC) $$($1_$2_$3_ALL_CC_OPTS) -c $$< -o $$@

$1/$2/build/%.$$($3_osuf) : $1/$2/build/%.$$($3_way_)s
	$$(RM) $$@
	$$(AS) $$($1_$2_$3_ALL_AS_OPTS) -o $$@ $$<

$1/$2/build/%.$$($3_osuf) : $1/%.S $$(MKDIRHIER)
	$$(RM) $$@
	$$(MKDIRHIER) $$(dir $$@)
	$$(CC) $$($1_$2_$3_ALL_CC_OPTS) -c $$< -o $$@

$1/$2/build/%.$$($3_way_)s : $1/$2/build/%.c
	$$(RM) $$@
	$$(CC) $$($1_$2_$3_ALL_CC_OPTS) -S $$< -o $$@

endif

endef

