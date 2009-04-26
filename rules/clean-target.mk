
define clean-target # args: $1 = dir, $2 = key, $3 = files/dirs to clean
clean : clean_$1
.PHONY: clean_$1
clean_$1 : clean_$1_$2
.PHONY: clean_$1_$2
clean_$1_$2:
	$(RM) -rf $3
endef
