TOP = ..
include $(TOP)/mk/boilerplate.mk

all :: index.html

index.html : $(HS_SRCS)
	$(HADDOCK_INPLACE) -h $(HS_SRCS)

include $(TOP)/mk/target.mk
