TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# -----------------------------------------------------------------------------
# Testing RTS linker object resolution
#

CC=$(TEST_CC)

.PHONY: t_11223_simple_link
t_11223_simple_link:
	$(RM) -f foo_simple.o foo.hi foo.o
	"$(TEST_CC)" -c foo.c -o foo_simple.o
	echo main | "$(TEST_HC)" $(filter-out -rtsopts, $(TEST_HC_OPTS_INTERACTIVE)) foo_simple.o foo.hs

.PHONY: t_11223_simple_link_lib
t_11223_simple_link_lib:
	$(RM) -f foo_lib.o foo.hi foo.o libfoo_lib.a
	"$(TEST_CC)" -c foo.c -o foo_lib.o
	"$(AR)" rs libfoo_lib.a foo_lib.o 2> /dev/null
	echo main | "$(TEST_HC)" $(filter-out -rtsopts, $(TEST_HC_OPTS_INTERACTIVE)) foo.hs -lfoo_lib -L"$(PWD)"

.PHONY: t_11223_simple_duplicate
t_11223_simple_duplicate:
	$(RM) -f foo_dup.o bar_dup.o foo.hi foo.o
	"$(TEST_CC)" -c foo.c -o foo_dup.o
	"$(TEST_CC)" -c bar.c -o bar_dup.o
	echo main | "$(TEST_HC)" $(filter-out -rtsopts, $(TEST_HC_OPTS_INTERACTIVE)) foo_dup.o bar_dup.o foo.hs

.PHONY: t_11223_simple_duplicate_lib
t_11223_simple_duplicate_lib:
	$(RM) -f foo_dup_lib.o bar_dup_lib.o foo.hi foo.o libfoo_dup_lib.a
	"$(TEST_CC)" -c foo.c -o foo_dup_lib.o
	"$(TEST_CC)" -c bar.c -o bar_dup_lib.o
	"$(AR)" rs libfoo_dup_lib.a foo_dup_lib.o 2> /dev/null
	echo main | "$(TEST_HC)" $(filter-out -rtsopts, $(TEST_HC_OPTS_INTERACTIVE)) bar_dup_lib.o foo.hs -lfoo_dup_lib -L"$(PWD)"

.PHONY: t_11223_simple_unused_duplicate_lib
t_11223_simple_unused_duplicate_lib:
	$(RM) -f foo_dup_lib.o bar_dup_lib.o foo.hi foo.o libbar_dup_lib.a
	"$(TEST_CC)" -c foo.c -o foo_dup_lib.o
	"$(TEST_CC)" -c bar.c -o bar_dup_lib.o
	"$(AR)" rs libbar_dup_lib.a bar_dup_lib.o 2> /dev/null
	echo main | "$(TEST_HC)" $(filter-out -rtsopts, $(TEST_HC_OPTS_INTERACTIVE)) foo_dup_lib.o foo.hs -lbar_dup_lib -L"$(PWD)"

.PHONY: t_11223_link_order_a_b_succeed
t_11223_link_order_a_b_succeed:
	$(RM) -f foo_link_lib_1.o bar_link_lib_1.o foo.hi foo.o libbar_link_lib_1.a libfoo_link_lib_1.a
	"$(TEST_CC)" -c foo.c -o foo_link_lib_1.o
	"$(TEST_CC)" -c bar.c -o bar_link_lib_1.o
	"$(AR)" rs libbar_link_lib_1.a bar_link_lib_1.o 2> /dev/null
	"$(AR)" rs libfoo_link_lib_1.a foo_link_lib_1.o 2> /dev/null
	echo main | "$(TEST_HC)" $(filter-out -rtsopts, $(TEST_HC_OPTS_INTERACTIVE)) foo2.hs -lbar_link_lib_1 -lfoo_link_lib_1 -L"$(PWD)"

.PHONY: t_11223_link_order_b_a_succeed
t_11223_link_order_b_a_succeed:
	$(RM) -f foo_link_lib_2.o bar_link_lib_2.o foo.hi foo.o libbar_link_lib_2.a libfoo_link_lib_2.a
	"$(TEST_CC)" -c foo.c -o foo_link_lib_2.o
	"$(TEST_CC)" -c bar.c -o bar_link_lib_2.o
	"$(AR)" rs libbar_link_lib_2.a bar_link_lib_2.o 2> /dev/null
	"$(AR)" rs libfoo_link_lib_2.a foo_link_lib_2.o 2> /dev/null
	echo main | "$(TEST_HC)" $(filter-out -rtsopts, $(TEST_HC_OPTS_INTERACTIVE)) foo2.hs -lfoo_link_lib_2 -lbar_link_lib_2 -L"$(PWD)"

.PHONY: t_11223_link_order_a_b_2_fail
t_11223_link_order_a_b_2_fail:
	$(RM) -f foo_link_lib_3.o bar_link_lib_3.o foo.hi foo.o libbar_link_lib_3.a libfoo_link_lib_3.a
	"$(TEST_CC)" -c foo.c -o foo_link_lib_3.o
	"$(TEST_CC)" -c bar.c -o bar_link_lib_3.o
	"$(AR)" rs libbar_link_lib_3.a bar_link_lib_3.o 2> /dev/null
	"$(AR)" rs libfoo_link_lib_3.a foo_link_lib_3.o 2> /dev/null
	echo main | "$(TEST_HC)" $(filter-out -rtsopts, $(TEST_HC_OPTS_INTERACTIVE)) foo3.hs -lbar_link_lib_3 -lfoo_link_lib_3 -L"$(PWD)"

.PHONY: t_11223_link_order_b_a_2_succeed
t_11223_link_order_b_a_2_succeed:
	$(RM) -f foo_link_lib_4.o bar_link_lib_4.o foo.hi foo.o libbar_link_lib_4.a libfoo_link_lib_4.a
	"$(TEST_CC)" -c foo.c -o foo_link_lib_4.o
	"$(TEST_CC)" -c bar.c -o bar_link_lib_4.o
	"$(AR)" rs libbar_link_lib_4.a bar_link_lib_4.o 2> /dev/null
	"$(AR)" rs libfoo_link_lib_4.a foo_link_lib_4.o 2> /dev/null
	echo main | "$(TEST_HC)" $(filter-out -rtsopts, $(TEST_HC_OPTS_INTERACTIVE)) foo3.hs -lfoo_link_lib_4 -lbar_link_lib_4 -L"$(PWD)"

# -----------------------------------------------------------------------------
# Testing RTS weak symbols resolution
#

.PHONY: t_11223_weak_only_link_fail
t_11223_weak_only_link_fail:
	$(RM) -f power_w1.o power.hi power.o
	"$(TEST_CC)" -c power.c -DWEAK -o power_w1.o
	echo main | "$(TEST_HC)" $(filter-out -rtsopts, $(TEST_HC_OPTS_INTERACTIVE)) power.hs power_w1.o

.PHONY: t_11223_weak_only_link_succeed
t_11223_weak_only_link_succeed:
	$(RM) -f power_w2.o power3.hi power3.o
	"$(TEST_CC)" -c power_slow.c -DWEAK -o power_w2.o
	echo main | "$(TEST_HC)" $(filter-out -rtsopts, $(TEST_HC_OPTS_INTERACTIVE)) power3.hs power_w2.o

.PHONY: t_11223_weak_both_link_order_a_b_succeed
t_11223_weak_both_link_order_a_b_succeed:
	$(RM) -f fast_power_w3.o slow_power_w3.o power3.hi power3.o
	"$(TEST_CC)" -c power_slow.c -DWEAK -o slow_power_w3.o
	"$(TEST_CC)" -c power.c -DWEAK -o fast_power_w3.o
	echo main | "$(TEST_HC)" $(filter-out -rtsopts, $(TEST_HC_OPTS_INTERACTIVE)) power3.hs fast_power_w3.o slow_power_w3.o

.PHONY: t_11223_weak_both_link_order_b_a_succeed
t_11223_weak_both_link_order_b_a_succeed:
	$(RM) -f fast_power_w4.o slow_power_w4.o power3.hi power3.o
	"$(TEST_CC)" -c power_slow.c -DWEAK -o slow_power_w4.o
	"$(TEST_CC)" -c power.c -DWEAK -o fast_power_w4.o
	echo main | "$(TEST_HC)" $(filter-out -rtsopts, $(TEST_HC_OPTS_INTERACTIVE)) power3.hs slow_power_w4.o fast_power_w4.o

.PHONY: t_11223_weak_single_link_order_a_b_succeed
t_11223_weak_single_link_order_a_b_succeed:
	$(RM) -f fast_power_w5.o slow_power_w5.o power3.hi power3.o
	"$(TEST_CC)" -c power_slow.c -o slow_power_w5.o
	"$(TEST_CC)" -c power.c -DWEAK -o fast_power_w5.o
	echo main | "$(TEST_HC)" $(filter-out -rtsopts, $(TEST_HC_OPTS_INTERACTIVE)) power3.hs fast_power_w5.o slow_power_w5.o

.PHONY: t_11223_weak_single_link_order_b_a_succeed
t_11223_weak_single_link_order_b_a_succeed:
	$(RM) -f fast_power_w6.o slow_power_w6.o power3.hi power3.o
	"$(TEST_CC)" -c power_slow.c -DWEAK -o slow_power_w6.o
	"$(TEST_CC)" -c power.c -o fast_power_w6.o
	echo main | "$(TEST_HC)" $(filter-out -rtsopts, $(TEST_HC_OPTS_INTERACTIVE)) power3.hs slow_power_w6.o fast_power_w6.o

