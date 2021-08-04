/*
 * This is a simple driver for the http-parser package.  It is
 * intended to read one HTTP request after another from a file,
 * nothing more.
 *
 * For "feature parity" with the Haskell code in RFC2616.hs, we
 * allocate and populate a simple structure describing each request,
 * since that's the sort of thing that many real applications would
 * themselves do and the library doesn't do this for us.
 *
 * For the http-parser source, see
 * https://github.com/joyent/http-parser/blob/master/http_parser.c
 */

/*
 * Turn off this preprocessor symbol to have the callbacks do nothing
 * at all, which "improves performance" by about 50%.
 */
/*#define LOOK_BUSY*/

#include <assert.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include "http_parser.h"

struct http_string {
    size_t len;
    char value[0];
};

struct http_header {
    struct http_string *name;
    struct http_string *value;
    struct http_header *next;
};

struct http_request {
    struct http_string *method;
    struct http_string *uri;
    struct http_header *headers, *last;
};

struct data {
    size_t count;
    struct http_request req;
};

static void *xmalloc(size_t size)
{
    void *ptr;

    if ((ptr = malloc(size)) == NULL) {
	perror("malloc");
	exit(1);
    }

    return ptr;
}

static struct http_string *xstrdup(const char *src, size_t len, size_t extra)
{
    struct http_string *dst = xmalloc(sizeof(*dst) + len + extra);
    memcpy(dst->value, src, len);
    dst->len = len;
    return dst;
}

static void xstrcat(struct http_string **dst, const char *src, size_t len)
{
    struct http_string *p;

    if (*dst == NULL) {
	*dst = xstrdup(src, len, 0);
	return;
    }

    p = xstrdup((*dst)->value, (*dst)->len, len);
    memcpy(p->value + (*dst)->len, src, len);
    p->len += len;
    free(*dst);
    *dst = p;
}

static int begin(http_parser *p)
{
    struct data *data = p->data;

    data->count++;

    return 0;
}

static int url(http_parser *p, const char *at, size_t len)
{
#ifdef LOOK_BUSY
    struct data *data = p->data;

    xstrcat(&data->req.uri, at, len);
#endif

    return 0;
}

static int header_field(http_parser *p, const char *at, size_t len)
{
#ifdef LOOK_BUSY
    struct data *data = p->data;

    if (data->req.last && data->req.last->value == NULL) {
	xstrcat(&data->req.last->name, at, len);
    } else {
	struct http_header *hdr = xmalloc(sizeof(*hdr));

	hdr->name = xstrdup(at, len, 0);
	hdr->value = NULL;
	hdr->next = NULL;

	if (data->req.last != NULL)
	    data->req.last->next = hdr;
	data->req.last = hdr;
	if (data->req.headers == NULL)
	    data->req.headers = hdr;
    }
#endif

    return 0;
}

static int header_value(http_parser *p, const char *at, size_t len)
{
#ifdef LOOK_BUSY
    struct data *data = p->data;

    xstrcat(&data->req.last->value, at, len);
#endif

    return 0;
}

static int complete(http_parser *p)
{
#ifdef LOOK_BUSY
    struct data *data = p->data;
    struct http_header *hdr, *next;

    free(data->req.method);
    free(data->req.uri);

    for (hdr = data->req.headers; hdr != NULL; hdr = next) {
	next = hdr->next;
	free(hdr->name);
	free(hdr->value);
	free(hdr);
	hdr = next;
    }

    data->req.method = NULL;
    data->req.uri = NULL;
    data->req.headers = NULL;
    data->req.last = NULL;
#endif

    /* Bludgeon http_parser into understanding that we really want to
     * keep parsing after a request that in principle ought to close
     * the "connection". */
    if (!http_should_keep_alive(p)) {
	p->http_major = 1;
	p->http_minor = 1;
	p->flags &= ~6;
    }

    return 0;
}

static void parse(const char *path, int fd)
{
    struct data data;
    http_parser_settings s;
    http_parser p;
    ssize_t nread;

    memset(&s, 0, sizeof(s));
    s.on_message_begin = begin;
    s.on_url = url;
    s.on_header_field = header_field;
    s.on_header_value = header_value;
    s.on_message_complete = complete;

    p.data = &data;

    http_parser_init(&p, HTTP_REQUEST);

    data.count = 0;
    data.req.method = NULL;
    data.req.uri = NULL;
    data.req.headers = NULL;
    data.req.last = NULL;

    do {
	char buf[HTTP_MAX_HEADER_SIZE];
	size_t np;

	nread = read(fd, buf, sizeof(buf));

	np = http_parser_execute(&p, &s, buf, nread);
	if (np != nread) {
	    fprintf(stderr, "%s: parse failed\n", path);
	    break;
	}
    } while (nread > 0);

    printf("%ld\n", (unsigned long) data.count);
}

int main(int argc, char **argv)
{
    int i;

    for (i = 1; i < argc; i++) {
	int fd;

	fd = open(argv[i], O_RDONLY);
	if (fd == -1) {
	    perror(argv[i]);
	    continue;
	}
	parse(argv[i], fd);
	close(fd);
    }

    return 0;
}

/*
 * Local Variables:
 * c-file-style: "stroustrup"
 * End:
 */
