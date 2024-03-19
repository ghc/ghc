TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

T14434:
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T14434.hs -ddump-simpl | grep toStringX
         # Expecting toStringX = toString, not discarding argument

T14774:
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T14774.hs -ddump-simpl | grep p1D
         # Expecting no superclass selections to actually happen

tc170:
	$(RM) Tc170_Aux.hi Tc170_Aux.o tc170.hi tc170.o
	'$(TEST_HC)' $(TEST_HC_OPTS) -c Tc170_Aux.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c tc170.hs

tc173:
	$(RM) Tc173a.o Tc173a.hi Tc173b.o Tc173b.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) -c Tc173a.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c Tc173b.hs

T2412:
	$(RM) -f T2412.hi-boot T2412.o-boot T2412A.hi T2412A.o T2412.hi T2412.o
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T2412.hs-boot
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T2412A.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T2412.hs

tc245:
	$(RM) -f Tc245_A.hi Tc245_A.o tc245.hi tc245.o
	'$(TEST_HC)' $(TEST_HC_OPTS) --make tc245
	$(RM) -f tc245.hi tc245.o
	'$(TEST_HC)' $(TEST_HC_OPTS) --make tc245

# #5792 gave an error on the second compilation,
# presumably because of the .hi file
T5792:
	$(RM) -f T5792.o T5792.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T5792.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T5792.hs -fforce-recomp

T7171:
	$(RM) -f T7171.hi-boot T7171.o-boot T7171a.hi T7171a.o
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T7171a.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T7171.hs

# I got this one from a infinite loop during validate
tc266:
	$(RM) -f Tc266.hi-boot Tc266.o-boot Tc266a.hi Tc266a.o Tc266.hi Tc266.o
	'$(TEST_HC)' $(TEST_HC_OPTS) -c Tc266.hs-boot
	'$(TEST_HC)' $(TEST_HC_OPTS) -c Tc266a.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c Tc266.hs

Tc267:
	$(RM) -f Tc267a.hi-boot Tc267a.o-boot Tc267b.hi-boot Tc267b.o-boot
	$(RM) -f Tc267a.hi Tc267a.o Tc267b.hi Tc267b.o
	'$(TEST_HC)' $(TEST_HC_OPTS) -c Tc267a.hs-boot
	'$(TEST_HC)' $(TEST_HC_OPTS) -c Tc267b.hs-boot
	'$(TEST_HC)' $(TEST_HC_OPTS) -c Tc267a.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c Tc267b.hs

Tc271:
	$(RM) -f Tc271.hi-boot Tc271.o-boot Tc271a.hi Tc271a.o Tc271.hi Tc271.o
	'$(TEST_HC)' $(TEST_HC_OPTS) -c Tc271.hs-boot
	'$(TEST_HC)' $(TEST_HC_OPTS) -c Tc271a.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c Tc271.hs

T13585:
	$(RM) -f T13585a.o T13585a.hi T13585b.o T13585b.hi T13585.o T13585.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T13585a.hs -O
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T13585b.hs -O
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T13585.hs  -O

T14934:
	$(RM) -f T14934a.o T14934a.hi T14934.o T14934.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T14934a.hs -O
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T14934.hs  -O

T17723:
	$(RM) -f T17723a.o T17723a.hi T17723.o T17723.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T17723a.hs -O
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T17723.hs  -O

T17566:
	$(RM) -f T17566a.o T17566a.hi T17566.o T17566.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T17566a.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T17566.hs

# In the InlinePatSyn tests, we're interested in whether the pattern synonym (whose name always contains the string 'Pattern').
# is inlined or not. To determine this, we use sed to isolate lines between the start and end of the test definition. That is,
# from a line starting with 'test', to a blank line. We then use grep to determine if 'Pattern' occurs anywhere in the definition
# in the core. If it was inlined, it naturally won't occur, so grep -v will succeed, if it wasn't then plain grep will succeed.

InlinePatSyn_InlinableBuilder:
	$(RM) -f InlinePatSyn_InlinableBuilder.o InlinePatSyn_InlinableBuilder.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) -c InlinePatSyn_InlinableBuilder.hs  -O -dsuppress-all -ddump-hi | grep -q 'Inline:'

InlinePatSyn_InlinableMatcher:
	$(RM) -f InlinePatSyn_InlinableMatcher.o InlinePatSyn_InlinableMatcher.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) -c InlinePatSyn_InlinableMatcher.hs  -O -dsuppress-all -ddump-hi | grep -q 'Inline:'

InlinePatSyn_InlineBuilder:
	$(RM) -f InlinePatSyn_InlineBuilder.o InlinePatSyn_InlineBuilder.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) -c InlinePatSyn_InlineBuilder.hs  -O -dsuppress-all -ddump-simpl | sed -n '/^test/,/^$$/p' | grep -vq 'Pattern'

InlinePatSyn_InlineMatcher:
	$(RM) -f InlinePatSyn_InlineMatcher.o InlinePatSyn_InlineMatcher.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) -c InlinePatSyn_InlineMatcher.hs  -O -dsuppress-all -ddump-simpl | sed -n '/^test/,/^$$/p' | grep -vq 'Pattern'

InlinePatSyn_NoInlineBuilder:
	$(RM) -f InlinePatSyn_NoInlineBuilder.o InlinePatSyn_NoInlineBuilder.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) -c InlinePatSyn_NoInlineBuilder.hs  -O -dsuppress-all -ddump-simpl | sed -n '/^test/,/^$$/p' | grep -q 'Pattern'

InlinePatSyn_NoInlineMatcher:
	$(RM) -f InlinePatSyn_NoInlineMatcher.o InlinePatSyn_NoInlineMatcher.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) -c InlinePatSyn_NoInlineMatcher.hs  -O -dsuppress-all -ddump-simpl | sed -n '/^test/,/^$$/p' | grep -q 'Pattern'

InlinePatSyn_ExplicitBidiBuilder:
	$(RM) -f InlinePatSyn_ExplicitBidiBuilder.o InlinePatSyn_ExplicitBidiBuilder.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) -c InlinePatSyn_ExplicitBidiBuilder.hs  -O -dsuppress-all -ddump-simpl | sed -n '/^test/,/^$$/p' | grep -vq 'Pattern'

InlinePatSyn_ExplicitBidiMatcher:
	$(RM) -f InlinePatSyn_ExplicitBidiMatcher.o InlinePatSyn_ExplicitBidiMatcher.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) -c InlinePatSyn_ExplicitBidiMatcher.hs  -O -dsuppress-all -ddump-simpl | sed -n '/^test/,/^$$/p' | grep -vq 'Pattern'

T24566:
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T24566.hs -fno-code -dno-typeable-binds -ddump-if-trace 2>&1 | grep Integer || true
         # Not expecting any mention of Integer in the interface loading trace
