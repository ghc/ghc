
define hs-suffix-rules  # args: $1 = dir,  $2 = distdir, $3 = way

$1/$2/build/%.$$($3_hcsuf) : $1/$2/build/%.hs $$($1_$2_HC_DEP)
	$$($1_$2_HC) $$($1_$2_$3_ALL_HC_OPTS) -C $$< -o $$@

$1/$2/build/%.$$($3_osuf) : $1/$2/build/%.hs $$($1_$2_HC_DEP)
	$$($1_$2_HC) $$($1_$2_$3_ALL_HC_OPTS) -c $$< -o $$@

$1/$2/build/%.$$($3_hcsuf) : $1/$2/build/autogen/%.hs $$($1_$2_HC_DEP)
	$$($1_$2_HC) $$($1_$2_$3_ALL_HC_OPTS) -C $$< -o $$@

$1/$2/build/%.$$($3_osuf) : $1/$2/build/autogen/%.hs $$($1_$2_HC_DEP)
	$$($1_$2_HC) $$($1_$2_$3_ALL_HC_OPTS) -c $$< -o $$@

endef # hs-suffix-rules

