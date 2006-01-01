TOP=../../..

all :: generate

include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

generate: tools/instancesTestGen tools/soundnessTestGen
	tools/soundnessTestGen reference/Map.hs templates/TestSoundMap.hs '["fromAscList","fromList","keysSet","lookup"]'
	tools/soundnessTestGen reference/Set.hs templates/TestSoundSet.hs '["fromAscList","fromList"]'
	tools/instancesTestGen templates/Test*.hs > "generated.T"

benchmark:
	tools/instancesTestGen templates/Benchmark*.hs > "benchmark.T"


tools/soundnessTestGen: tools/SoundnessTestGen.hs
	ghc --make $< -o $@

tools/instancesTestGen: tools/InstancesTestGen.hs
	ghc --make $< -o $@


%.test : test_%.hs LibTest.hs 
	$(TARGET_HC) --make -O -fglasgow-exts -package QuickCheck -o $@ $< 

veryclean: clean
	-rm test_*
	-rm *.T

clean:
	-rm $(find . -name "*.o")
	-rm $(find . -name "*.hi")

distclean: veryclean
	-rm *~