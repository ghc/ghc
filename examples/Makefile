TOP = ..
include $(TOP)/mk/boilerplate.mk

all :: index.html

index.html : $(HS_SRCS) $(HADDOCK_INPLACE)
	$(HADDOCK_INPLACE) -h $(HS_SRCS)

CLEAN_FILES += index.html

include $(TOP)/mk/target.mk
