clean: ## Remove compilation artifacts
	@cabal clean

repl: ## Start a REPL
	@cabal repl

test: ## Run the test suite
	@cabal test

lint: ## Run the code linter (HLint)
	@find driver haddock-api haddock-library -name "*.hs" | xargs -P $(PROCS) -I {} hlint --refactor-options="-i" --refactor {}

style: ## Run the code styler (fourmolu and cabal-fmt)
	@cabal-fmt -i **/*.cabal
	@fourmolu -q --mode inplace driver haddock-api haddock-library

style-check: ## Check the code's style (fourmolu and cabal-fmt)
	@cabal-fmt -c **/*.cabal
	@fourmolu -q --mode check driver haddock-api haddock-library

style-quick: ## Run the code styler on modified files tracked by git
	@cabal-fmt -i **/*.cabal
	@git diff origin --name-only driver haddock-api haddock-library | xargs -P $(PROCS) -I {} fourmolu -q -i {}

tags: ## Generate ctags and etags for the source code (ghc-tags)
	@ghc-tags -e -c

help: ## Display this help message
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.* ?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

.PHONY: all $(MAKECMDGOALS)

.DEFAULT_GOAL := help

ifeq ($(UNAME), Darwin)
    PROCS := $(shell sysctl -n hw.logicalcpu)
else
    PROCS := $(shell nproc)
endif
