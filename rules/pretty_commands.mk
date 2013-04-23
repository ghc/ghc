ifneq ($(V),1)
cmd = @echo '  $(if $(label_$1),$(label_$1),$1) $@'; "$($1)"
else
cmd = "$($1)"
endif

label_ALEX=ALEX
label_HAPPY=HAPPY
label_hsc2hs_INPLACE=HSC2HS




