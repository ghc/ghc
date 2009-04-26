
define all-target # args: $1 = dir, $2 = deps
all : all_$1
.PHONY: all_$1
all_$1 : $2
endef

