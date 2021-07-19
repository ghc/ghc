# To enable profiling you must pass the --enable-library-profiling
# flag to "cabal configure".
ghc-prof-flags :=
ifdef ENABLE_PROFILING
	ghc-prof-flags += -prof -hisuf p_hi -osuf p_o -fprof-auto
	lib-suffix := _p
else
	lib-suffix :=
endif

# To enable event logging you must pass the --ghc-option=-eventlog
# flag to "cabal configure".
ifdef ENABLE_EVENTLOG
	ghc-prof-flags += -eventlog
endif

INPLACE := ./../../../inplace
ghc := $(INPLACE)/bin/ghc-stage2
ghc-opt-flags := -O2
ghc-base-flags := -funbox-strict-fields -fno-ignore-asserts
ghc-base-flags += -Wall -fno-warn-orphans -fno-warn-missing-signatures
ghc-flags := $(ghc-base-flags) $(ghc-prof-flags) \
				-package network-3.1.2.1 \
				-package unix
ghc-hpc-flags := $(ghc-base-flags) -fhpc -fno-ignore-asserts

cabal := $(shell which cabal 2>/dev/null)

%.o: %.hs
	$(ghc) $(ghc-flags) $(ghc-opt-flags) -c $< -o $@

%.hs: %.hsc
	hsc2hs $<

# package network-bytestring is deprecated into network
# and bytestring-show is redundant
ghc-bench-flags := -package mtl \
					-package attoparsec \

ifdef USE_GHC_IO_MANAGER
	ghc-bench-flags += -DUSE_GHC_IO_MANAGER
endif

programs := dead-conn deadconn pong-server signal simple static-http \
			thread-delay timers http-client

.PHONY: bench

bench: all clean
all: $(programs)

cc = gcc
cc-opt-flags = -O2
%.o: %.c
	$(cc) -c $(cc-opt-flags) $< -o $@

dead-conn: ghc-flags += $(ghc-bench-flags)
dead-conn: Args.o EventUtil.o EventSocket.o DeadConn.o
	$(ghc) $(ghc-flags) -threaded -o $@ $(filter %.o,$^)

# A C version of the above tool.
deadconn: deadconn.o
	$(cc) $(cc-opt-flags) -o $@ $(filter %.o,$^)

pong-server: ghc-flags += $(ghc-bench-flags)
pong-server: Args.o EventUtil.o EventSocket.o PongServer.o
	$(ghc) $(ghc-flags) -threaded -o $@ $(filter %.o,$^)

event-http: ghc-flags += $(ghc-bench-flags)
event-http: EventUtil.o EventFile.o EventSocket.o NoPush.o RFC2616.o EventHttp.o
	$(ghc) $(ghc-flags) -threaded -o $@ $(filter %.o,$^)

static-http: ghc-flags += $(ghc-bench-flags)
static-http: EventUtil.o EventFile.o EventSocket.o NoPush.o RFC2616.o StaticHttp.o
	$(ghc) $(ghc-flags) -threaded -o $@ $(filter %.o,$^)

http-client: ghc-flags += $(ghc-bench-flags) -package parsec-3.1.14.0
http-client: Args.o EventUtil.o EventSocket.o RFC2616.o HttpClient.o
	$(ghc) $(ghc-flags) -threaded -o $@ $(filter %.o,$^)

signal: Signal.o
	$(ghc) $(ghc-flags) -threaded -o $@ $(filter %.o,$^)

simple: Args.o Simple.o
	$(ghc) $(ghc-flags) -threaded -o $@ $(filter %.o,$^)

thread-delay: ghc-flags += -package stm
thread-delay: Args.o ThreadDelay.o
	$(ghc) $(ghc-flags) -threaded -o $@ $(filter %.o,$^)

timers: Args.o Timers.o
	$(ghc) $(ghc-flags) -threaded -o $@ $(filter %.o,$^)


.PHONY: cleanall

cleanall: clean data_clean
	-find . \( -name 'events' -o -name 'threads' -o -name 'new-thread-delay' -o -name 'old-thread-delay' \) -exec rm {} \;
	-rm -f $(programs)

clean:
	-find . \( -name '*.o' -o -name '*.dyn_o' -o -name '*.hi' \) -exec rm {} \;

data_clean:
	-find . \( -name '*.dat' -o -name '*.png' \) -exec rm {} \;
