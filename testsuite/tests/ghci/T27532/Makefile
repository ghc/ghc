TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

.PHONY: T27532
T27532:
	'$(TEST_HC)' $(TEST_HC_OPTS_INTERACTIVE) -ignore-dot-ghci -v0 < T27532.script 2>/dev/null | grep '^instance' > all.txt
	n=$$(wc -l < all.txt); \
	test $$n -gt 0 -a $$(($$n % 2)) -eq 0 || { echo "bad instance count: $$n"; exit 1; }; \
	awk -v h=$$(($$n / 2)) 'NR<=h' all.txt > before.txt; \
	awk -v h=$$(($$n / 2)) 'NR>h' all.txt > after.txt; \
	diff before.txt after.txt || true
