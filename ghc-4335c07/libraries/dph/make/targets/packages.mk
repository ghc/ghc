

dph_packages_backend = \
	dph-base \
	dph-prim-interface \
	dph-prim-seq \
	dph-prim-par
	
dph_packages_frontend = \
	dph-lifted-base \
	dph-lifted-boxed \
	dph-lifted-copy \
	dph-lifted-vseg

dph_packages = \
	$(dph_packages_backend) \
	$(dph_packages_frontend)
	
dph_packages_dbs = \
	$(patsubst %,%/dist/package.conf.inplace,$(dph_packages))
	

# Build all the packages
.PHONY 	   : packages
packages   : $(dph_packages_dbs)


# Clean and unregister all the DPH packages
.PHONY	   : unregsiter
unregister : clean
	@for p in $(dph_packages); do \
		$(GHC_PKG) unregister $$p --force; \
	done


# Build the backend packages	
%/dist/package.conf.inplace : %
	@cd $(patsubst %/dist/package.conf.inplace,%,$@) ; \
	 $(GHC_DPH) --make Setup.hs ; \
	 ./Setup configure --user ; \
	 ./Setup build ; \
	 ./Setup install
	
	@echo

