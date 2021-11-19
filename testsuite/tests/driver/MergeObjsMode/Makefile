TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

MergeObjsMode :
	"$(TEST_HC)" $(TEST_HC_OPTS) -c A.hs
	"$(TEST_HC)" $(TEST_HC_OPTS) -c B.hs
	"$(TEST_HC)" $(TEST_HC_OPTS) -c Main.hs
	"$(TEST_HC)" $(TEST_HC_OPTS) -merge-objs -o HSlib.o A.o B.o
	"$(TEST_HC)" $(TEST_HC_OPTS) -o Main Main.o HSlib.o
	./Main

