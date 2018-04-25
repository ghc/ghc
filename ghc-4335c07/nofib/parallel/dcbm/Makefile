TOP = ../..
include $(TOP)/mk/boilerplate.mk

Primitives.hi : Primitives.lhi
	cp $< $@

depend :: Primitives.hi

SRC_HC_OPTS += -cpp

include $(TOP)/mk/target.mk

