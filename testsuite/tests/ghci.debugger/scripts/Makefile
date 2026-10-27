TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# ^C while :continue waits after resuming a thread forked by the evaluation
break050ext: break050_OPTS = -fexternal-interpreter
break050 break050ext:
	rm -f break050.started
	"$(TEST_HC)" $(TEST_HC_OPTS_INTERACTIVE) $(break050_OPTS) < break050.script & \
	while [ ! -e break050.started ] && kill -0 $$! 2>/dev/null; do sleep 0.1; done; \
	kill -INT $$! $$(pgrep -P $$!); wait
