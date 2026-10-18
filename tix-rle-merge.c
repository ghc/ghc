/* Merge HPC prototype run-length-encoded .tix files (NOT part of GHC).
 *
 * Build: cc -O3 -std=c11 -Wall -Wextra -o tix-rle-merge tix-rle-merge.c
 * Usage: ./tix-rle-merge RLE_DIRECTORY OUTPUT.tix
 *
 * Input and output format (HPCTIX\2):
 *   7 bytes "HPCTIX\2", ULEB128 module count;
 *   for each module:
 *     ULEB128 name length, name bytes, little-endian uint32 hash,
 *     ULEB128 box count;
 *     repeated (ULEB128 zero-run length, ULEB128 nonzero-run length,
 *               that many positive ULEB128 Word64 counts)
 *     until exactly 'box count' counters are described.
 *
 * No terminator follows an RLE module. A trailing zero run is represented
 * by (zero-run length, 0). Modules with zero boxes contain no run records.
 *
 * The accumulator is a dense uint64_t array per distinct module, as in
 * tix-sparse-prototype.c. Checks counter overflow and malformed inputs.
 */
#define _POSIX_C_SOURCE 200809L
#include <dirent.h>
#include <errno.h>
#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

#define MAGIC "HPCTIX\0"
#define BUCKETS 8192

typedef struct { unsigned char *data; size_t len, cap; } Buffer;
typedef struct { const unsigned char *p, *end; } Reader;
typedef struct Module {
    char *name;
    uint32_t hash;
    size_t n;
    uint64_t *ticks;
    uint64_t seen_epoch;
    struct Module *next;
} Module;
typedef struct {
    Module *buckets[BUCKETS];
    Module **mods;
    size_t len, cap;
    uint64_t epoch, filecount, input_entries;
} Acc;

static const char *context;
static void die(const char *msg) {
    fprintf(stderr, "%s: %s\n", context ? context : "tix-rle-merge", msg);
    exit(1);
}
static void *xmalloc(size_t n) {
    void *p = malloc(n ? n : 1);
    if (!p) die("out of memory");
    return p;
}
static void *xrealloc(void *p, size_t n) {
    void *q = realloc(p, n ? n : 1);
    if (!q) die("out of memory");
    return q;
}
static void reserve(Buffer *b, size_t add) {
    if (add > SIZE_MAX - b->len) die("buffer too large");
    size_t need = b->len + add;
    if (need <= b->cap) return;
    size_t cap = b->cap ? b->cap : 256;
    while (cap < need) {
        if (cap > SIZE_MAX / 2) { cap = need; break; }
        cap *= 2;
    }
    b->data = xrealloc(b->data, cap);
    b->cap = cap;
}
static void bytes(Buffer *b, const void *src, size_t n) {
    reserve(b, n);
    memcpy(b->data + b->len, src, n);
    b->len += n;
}
static void byte(Buffer *b, unsigned char c) {
    reserve(b, 1);
    b->data[b->len++] = c;
}
static void putvar(Buffer *b, uint64_t x) {
    while (x >= 128) {
        byte(b, (unsigned char)((x & 127) | 128));
        x >>= 7;
    }
    byte(b, (unsigned char)x);
}
static void put32(Buffer *b, uint32_t x) {
    for (unsigned k = 0; k < 4; k++) byte(b, (unsigned char)(x >> (8*k)));
}
static uint64_t getvar(Reader *r) {
    uint64_t x = 0;
    for (unsigned k = 0; k < 10; k++) {
        if (r->p == r->end) die("truncated variable-length integer");
        unsigned c = *r->p++;
        if (k == 9 && (c & 0xfe)) die("variable-length integer overflow");
        x |= (uint64_t)(c & 127) << (7*k);
        if (!(c & 128)) {
            if (k && c == 0) die("noncanonical variable-length integer");
            return x;
        }
    }
    die("unterminated variable-length integer");
    return 0;
}
static uint32_t get32(Reader *r) {
    if ((size_t)(r->end - r->p) < 4) die("truncated hash");
    uint32_t x = 0;
    for (unsigned k = 0; k < 4; k++) x |= (uint32_t)(*r->p++) << (8*k);
    return x;
}
static unsigned char *readfile(const char *path, size_t *size) {
    FILE *f = fopen(path, "rb");
    if (!f) { perror(path); exit(1); }
    if (fseek(f, 0, SEEK_END)) die("seek failed");
    long n = ftell(f);
    if (n < 0 || fseek(f, 0, SEEK_SET)) die("size failed");
    if ((uintmax_t)n > SIZE_MAX) die("file too large");
    unsigned char *data = xmalloc((size_t)n);
    if (fread(data, 1, (size_t)n, f) != (size_t)n) die("read failed");
    if (fclose(f)) die("close failed");
    *size = (size_t)n;
    return data;
}
static void writefile(const char *path, const void *data, size_t n) {
    FILE *f = fopen(path, "wb");
    if (!f) { perror(path); exit(1); }
    if (fwrite(data, 1, n, f) != n) die("write failed");
    if (fclose(f)) die("close failed");
}
static uint64_t namehash(const char *s) {
    uint64_t h = UINT64_C(14695981039346656037);
    for (; *s; s++) { h ^= (unsigned char)*s; h *= UINT64_C(1099511628211); }
    return h;
}
static Module *module(Acc *a, const char *name, uint32_t hash, size_t n) {
    size_t bucket = namehash(name) % BUCKETS;
    for (Module *m = a->buckets[bucket]; m; m = m->next) {
        if (strcmp(m->name, name)) continue;
        if (m->hash != hash || m->n != n) die("incompatible module hash/number of boxes");
        return m;
    }
    if (n > SIZE_MAX / sizeof(uint64_t)) die("module too large");
    Module *m = xmalloc(sizeof(*m));
    m->name = strdup(name);
    if (!m->name) die("out of memory");
    m->hash = hash;
    m->n = n;
    m->ticks = calloc(n ? n : 1, sizeof(uint64_t));
    if (!m->ticks) die("out of memory");
    m->seen_epoch = 0;
    m->next = a->buckets[bucket];
    a->buckets[bucket] = m;
    if (a->len == a->cap) {
        if (a->cap > SIZE_MAX / (2 * sizeof(*a->mods))) die("too many modules");
        a->cap = a->cap ? 2 * a->cap : 256;
        a->mods = xrealloc(a->mods, a->cap * sizeof(*a->mods));
    }
    a->mods[a->len++] = m;
    return m;
}
static void mergeone(Acc *a, const char *path) {
    context = path;
    size_t size;
    unsigned char *data = readfile(path, &size);
    Reader r = {data, data + size};
    if (size < 7 || memcmp(r.p, MAGIC, 7)) die("wrong magic/version (expected HPCTIX\\2)");
    r.p += 7;
    uint64_t nmods = getvar(&r);
    if (nmods > (uint64_t)(r.end - r.p)) die("invalid module count");
    if (++a->epoch == 0) die("epoch overflow");

    for (uint64_t j = 0; j < nmods; j++) {
        uint64_t name_len = getvar(&r);
        if (name_len > SIZE_MAX - 1 || name_len > (uint64_t)(r.end - r.p))
            die("invalid name length");
        char *name = xmalloc((size_t)name_len + 1);
        memcpy(name, r.p, (size_t)name_len);
        name[name_len] = 0;
        if (memchr(name, 0, (size_t)name_len)) die("embedded NUL in module name");
        r.p += (size_t)name_len;
        uint32_t hash = get32(&r);
        uint64_t boxes = getvar(&r);
        if (boxes > SIZE_MAX / sizeof(uint64_t)) die("too many counters");
        Module *m = module(a, name, hash, (size_t)boxes);
        free(name);
        if (m->seen_epoch == a->epoch) die("duplicate module");
        m->seen_epoch = a->epoch;

        /* RLE records account for exactly 'boxes' positions. */
        size_t pos = 0;
        while (pos < m->n) {
            uint64_t zeros = getvar(&r);
            if (zeros > m->n - pos) die("zero run exceeds module box count");
            pos += (size_t)zeros;
            uint64_t nonzeros = getvar(&r);
            if (nonzeros > m->n - pos) die("nonzero run exceeds module box count");
            if (!zeros && !nonzeros) die("empty RLE record");

            size_t stop = pos + (size_t)nonzeros;
            for (; pos < stop; pos++) {
                uint64_t value = getvar(&r);
                if (!value) die("zero value in nonzero run");
                if (UINT64_MAX - m->ticks[pos] < value) die("counter overflow");
                m->ticks[pos] += value;
            }
            if (UINT64_MAX - a->input_entries < nonzeros) die("entry count overflow");
            a->input_entries += nonzeros;
        }
    }
    if (r.p != r.end) die("unexpected trailing bytes");
    free(data);
    a->filecount++;
}
static int cmpmod(const void *a, const void *b) {
    const Module *const *x = a, *const *y = b;
    return strcmp((*x)->name, (*y)->name);
}
static Buffer serialise(const Acc *a) {
    Buffer b = {0};
    bytes(&b, MAGIC, 7);
    putvar(&b, a->len);
    for (size_t j = 0; j < a->len; j++) {
        Module *m = a->mods[j];
        size_t name_len = strlen(m->name);
        putvar(&b, name_len);
        bytes(&b, m->name, name_len);
        put32(&b, m->hash);
        putvar(&b, m->n);

        size_t i = 0;
        while (i < m->n) {
            size_t start = i;
            while (i < m->n && m->ticks[i] == 0) i++;
            putvar(&b, i - start);
            start = i;
            while (i < m->n && m->ticks[i] != 0) i++;
            putvar(&b, i - start);
            for (size_t k = start; k < i; k++) putvar(&b, m->ticks[k]);
        }
    }
    return b;
}
static int is_tix(const char *s) {
    size_t n = strlen(s);
    return n >= 4 && strcmp(s + n - 4, ".tix") == 0;
}
static int cmpstr(const void *a, const void *b) {
    return strcmp(*(char *const *)a, *(char *const *)b);
}
static void merge_dir(const char *dir, const char *output) {
    DIR *d = opendir(dir);
    if (!d) { perror(dir); exit(1); }
    char **paths = NULL;
    size_t n = 0, cap = 0;
    struct dirent *e;
    struct stat outstat;
    int output_exists = stat(output, &outstat) == 0;
    while ((e = readdir(d))) {
        if (!is_tix(e->d_name)) continue;
        size_t dlen = strlen(dir), flen = strlen(e->d_name);
        if (dlen > SIZE_MAX - flen - 2) die("path too long");
        char *path = xmalloc(dlen + flen + 2);
        snprintf(path, dlen + flen + 2, "%s/%s", dir, e->d_name);
        struct stat st;
        if (stat(path, &st) || !S_ISREG(st.st_mode)) { free(path); continue; }
        if (!strcmp(path, output) ||
            (output_exists && st.st_dev == outstat.st_dev && st.st_ino == outstat.st_ino))
            die("output file is among input files");
        if (n == cap) {
            if (cap > SIZE_MAX / (2 * sizeof(*paths))) die("too many input files");
            cap = cap ? 2 * cap : 256;
            paths = xrealloc(paths, cap * sizeof(*paths));
        }
        paths[n++] = path;
    }
    closedir(d);
    if (!n) die("no input .tix files");
    qsort(paths, n, sizeof(*paths), cmpstr);
    Acc a = {0};
    for (size_t i = 0; i < n; i++) {
        mergeone(&a, paths[i]);
        free(paths[i]);
        if (a.filecount % 1000 == 0)
            fprintf(stderr, "Merged %" PRIu64 " files\n", a.filecount);
    }
    free(paths);
    qsort(a.mods, a.len, sizeof(*a.mods), cmpmod);
    Buffer out = serialise(&a);
    context = output;
    writefile(output, out.data, out.len);
    fprintf(stderr, "Merged %" PRIu64 " files, %zu modules, %" PRIu64
            " nonzero input counters; result %zu bytes\n",
            a.filecount, a.len, a.input_entries, out.len);
    free(out.data);
    for (size_t i = 0; i < a.len; i++) {
        free(a.mods[i]->ticks);
        free(a.mods[i]->name);
        free(a.mods[i]);
    }
    free(a.mods);
}
int main(int argc, char **argv) {
    if (argc != 3) {
        fprintf(stderr, "Usage: %s RLE_DIRECTORY OUTPUT.tix\n", argv[0]);
        return 2;
    }
    merge_dir(argv[1], argv[2]);
    return 0;
}
