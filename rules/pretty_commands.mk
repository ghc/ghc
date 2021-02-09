ifeq ($(V),0)
cmd = @"$($1)"
else ifeq ($(V),1)
cmd = "$($1)"
else
cmd = @echo '  $(if $(label_$1),$(label_$1),$1) $@'; "$($1)"
endif

label_ALEX=ALEX
label_HAPPY=HAPPY
label_hsc2hs_INPLACE=HSC2HS
