CC := cc
CFLAGS := -O3 -Wall
# To make the code about 6% faster:
# CFLAGS += -fwhole-program --combine
CPPFLAGS := -I$(HOME)/hg/http-parser

all: c-http

c-http: rfc2616.c http_parser.c
	$(CC) $(CFLAGS) $(CPPFLAGS) -o $@ $^

clean:
	rm -f *.hi *.o c-http

http_parser.c: http_parser.h
	curl -O https://raw.githubusercontent.com/joyent/http-parser/master/http_parser.c

http_parser.h:
	curl -O https://raw.githubusercontent.com/joyent/http-parser/master/http_parser.h
