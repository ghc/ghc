TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# We accept a 5% increase in parser allocations due to -haddock
haddock_parser_perf :
	WithHaddock=$(shell '$(TEST_HC)' $(TEST_HC_OPTS) -fno-code -fforce-recomp -Wno-all -ddump-timings -haddock -O0 Fold.hs 2>/dev/null | grep Parser | egrep -o 'alloc=[0-9]+' | cut -c7-  ) ; \
	WithoutHaddock=$(shell '$(TEST_HC)' $(TEST_HC_OPTS) -fno-code -fforce-recomp -Wno-all -ddump-timings -O0 Fold.hs 2>/dev/null | grep Parser | egrep -o 'alloc=[0-9]+' | cut -c7-  ) ;  \
	  awk "BEGIN { ratio = ($$WithHaddock / $$WithoutHaddock); if (ratio > 1.05) {print \"-haddock allocation ratio too high:\", ratio; exit 1} else {exit 0} }"

# Similarly for the renamer
haddock_renamer_perf :
	WithoutHaddock=$(shell '$(TEST_HC)' $(TEST_HC_OPTS) -fno-code -fforce-recomp -Wno-all -ddump-timings -O0 Fold.hs 2>/dev/null | grep Renamer | egrep -o 'alloc=[0-9]+' | cut -c7-  ) ; \
	WithHaddock=$(shell '$(TEST_HC)' $(TEST_HC_OPTS) -fno-code -fforce-recomp -Wno-all -ddump-timings -haddock -O0 Fold.hs 2>/dev/null | grep Renamer | egrep -o 'alloc=[0-9]+' | cut -c7-  ) ;  \
	  awk "BEGIN { ratio = ($$WithHaddock / $$WithoutHaddock); if (ratio > 1.05) {print \"-haddock allocation ratio too high:\", ratio; exit 1} else {exit 0} }"
