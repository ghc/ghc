
.PHONY	: clean
clean :
	@echo "* Cleaning up"
	@find . \
			-name "*.deps" \
		-o      -name "*.deps.inc" \
		-o      -name "*.deps.bak" \
		-o	-name "*.o" \
		-o      -name "*.o-boot" \
		-o	-name "*.hi" \
		-o	-name "*.hi-boot" \
		-o	-name "*.bin" \
		-o	-name "Main" \
		-o	-name "Setup" \
		-follow | xargs -n 1 rm -f

	@rm -f dph-test/bin/war

	@find . \
			-name "dist"  -type d \
		-o	-name "war-*" -type d \
		-follow | xargs -n 1 rm -Rf

	@rm -Rf sdist
	@rm -Rf dph-lifted-copy-seq
	@rm -Rf dph-lifted-copy-par
	@rm -Rf dph-lifted-vseg-seq
	@rm -Rf dph-lifted-vseg-par

	@echo
