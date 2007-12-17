READLINE_DIR = readline-5.2
GMP_DIR = gmp-4.2.1

all: GMP.framework GNUreadline.framework

$(GMP_DIR).tar.gz:
	cp ../../gmp/$(GMP_DIR).tar.gz .

$(READLINE_DIR).tar.gz:
	curl -O ftp://ftp.cwru.edu/pub/bash/$(READLINE_DIR).tar.gz

$(GMP_DIR): $(GMP_DIR).tar.gz
	tar -xzvf $(GMP_DIR).tar.gz

$(READLINE_DIR): $(READLINE_DIR).tar.gz
	tar -xzvf $(READLINE_DIR).tar.gz

GMP.framework: $(GMP_DIR)
	sh build-framework-gmp.sh

GNUreadline.framework: $(READLINE_DIR)
	sh build-framework-readline.sh

clean: 
	rm -rf $(GMP_DIR) $(READLINE_DIR)
	rm -rf 	GMP.framework GNUreadline.framework
	rm -rf 	GMP-framework.zip GNUreadline-framework.zip

cleanall: clean
	rm -f $(GMP_DIR).tar.gz
	rm -f $(READLINE_DIR).tar.gz
