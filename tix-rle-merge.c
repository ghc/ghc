/* Group and merge HPC binary RLE .tix files without combining incompatible
 * versions of the same module.
 *
 * Build: cc -O2 -std=c11 -Wall -Wextra -pedantic -o tix-rle-merge tix-rle-merge-grouped.c
 * Usage: tix-rle-merge INPUT_DIRECTORY EMPTY_OUTPUT_DIRECTORY
 *
 * Input format: "HPCTIX\0", ULEB128 number of modules; each module has
 * ULEB128 name length, name bytes, little-endian uint32 HPC hash, ULEB128
 * box count, then alternating (zero run, nonzero run, positive counter values)
 * encoded in ULEB128 until all box positions are covered. Output is the same.
 *
 * Group assignment is greedy, deterministic (lexicographic filename order):
 * - never group files with a shared module name but different hash/box count;
 * - require at least one shared module to merge into an existing group;
 * - prefer the compatible group sharing the most boxes, then most modules,
 *   then most existing files, then lowest group number;
 * - otherwise create a new group.
 *
 * This is a conservative heuristic, not a unique/optimal partition.
 * Results are in merged-0001.tix, ... ; assignments.tsv lists every input;
 * groups.txt gives group sizes, contents and split explanations. The output
 * directory must be empty (or nonexistent) to avoid stale results.
 *
 * Two passes: inspect all inputs and assign groups without retaining counters;
 * then merge and write one group at a time. Thus memory is bounded by the
 * metadata for the groups plus one dense set of counters.
 */
#define _POSIX_C_SOURCE 200809L
#include <dirent.h>
#include <errno.h>
#include <inttypes.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

#define MAGIC "HPCTIX\0"
#define MAGIC_SIZE 7
#define BUCKETS 8192

typedef struct { unsigned char *data; size_t len, cap; } Buffer;
typedef struct { const unsigned char *p, *end; } Reader;
typedef struct { char *name; uint32_t hash; size_t n; } InputMod;
typedef struct { InputMod *mods; size_t len, cap; } Input;
typedef struct Sig {
    char *name;
    uint32_t hash;
    size_t n;
    const char *first_path;   /* borrowed from sorted input path list */
    struct Sig *next;
} Sig;
typedef struct {
    Sig *buckets[BUCKETS];
    size_t *indices;          /* indices into the sorted input path list */
    size_t len, cap, modules;
    char *reason;
} Group;
typedef struct {
    size_t group, shared_modules;
    uint64_t shared_boxes;
} Assignment;
typedef struct Module {
    char *name;
    uint32_t hash;
    size_t n;
    uint64_t *ticks, seen_epoch;
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
static void *xcalloc(size_t n, size_t s) {
    if (s && n > SIZE_MAX / s) die("allocation overflow");
    void *p = calloc(n ? n : 1, s ? s : 1);
    if (!p) die("out of memory");
    return p;
}
static void *xrealloc(void *p, size_t n) {
    void *q = realloc(p, n ? n : 1);
    if (!q) die("out of memory");
    return q;
}
static char *xstrdup(const char *s) {
    char *p = strdup(s);
    if (!p) die("out of memory");
    return p;
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
        if (r->p == r->end) die("truncated ULEB128");
        unsigned c = *r->p++;
        if (k == 9 && (c & 0xfe)) die("ULEB128 overflow");
        x |= (uint64_t)(c & 127) << (7*k);
        if (!(c & 128)) {
            if (k && c == 0) die("noncanonical ULEB128");
            return x;
        }
    }
    die("unterminated ULEB128");
    return 0;
}
static uint32_t get32(Reader *r) {
    if ((size_t)(r->end - r->p) < 4) die("truncated module hash");
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
static size_t bucket_of(const char *s) { return (size_t)(namehash(s) % BUCKETS); }
static char *join(const char *dir, const char *file) {
    size_t a = strlen(dir), b = strlen(file);
    if (a > SIZE_MAX - b - 2) die("path too long");
    char *p = xmalloc(a + b + 2);
    snprintf(p, a + b + 2, "%s/%s", dir, file);
    return p;
}
static int is_tix(const char *s) {
    size_t n = strlen(s);
    return n >= 4 && strcmp(s + n - 4, ".tix") == 0;
}
static int cmpstr(const void *a, const void *b) {
    return strcmp(*(char *const *)a, *(char *const *)b);
}
static int cmp_input_mod(const void *a, const void *b) {
    return strcmp(((const InputMod *)a)->name, ((const InputMod *)b)->name);
}
static void input_append(Input *in, const char *name, uint32_t hash, size_t n) {
    if (in->len == in->cap) {
        if (in->cap > SIZE_MAX / (2 * sizeof(*in->mods))) die("too many modules");
        in->cap = in->cap ? in->cap * 2 : 64;
        in->mods = xrealloc(in->mods, in->cap * sizeof(*in->mods));
    }
    in->mods[in->len++] = (InputMod){xstrdup(name), hash, n};
}
static void input_free(Input *in) {
    for (size_t i = 0; i < in->len; i++) free(in->mods[i].name);
    free(in->mods);
}
/* Validate the complete file and collect just its module signatures. */
static Input inspect(const char *path) {
    context = path;
    size_t size;
    unsigned char *data = readfile(path, &size);
    Reader r = {data, data + size};
    if (size < MAGIC_SIZE || memcmp(r.p, MAGIC, MAGIC_SIZE))
        die("wrong magic/version (expected HPCTIX\\0)");
    r.p += MAGIC_SIZE;
    uint64_t nmods = getvar(&r);
    if (nmods > (uint64_t)(r.end - r.p)) die("invalid module count");
    Input in = {0};
    for (uint64_t j = 0; j < nmods; j++) {
        uint64_t length = getvar(&r);
        if (length > SIZE_MAX - 1 || length > (uint64_t)(r.end - r.p))
            die("invalid module name length");
        char *name = xmalloc((size_t)length + 1);
        memcpy(name, r.p, (size_t)length);
        name[length] = 0;
        if (memchr(name, 0, (size_t)length)) die("embedded NUL in module name");
        r.p += (size_t)length;
        uint32_t hash = get32(&r);
        uint64_t boxes = getvar(&r);
        if (boxes > SIZE_MAX / sizeof(uint64_t)) die("too many counters");
        input_append(&in, name, hash, (size_t)boxes);
        free(name);
        size_t pos = 0;
        while (pos < (size_t)boxes) {
            uint64_t zeros = getvar(&r);
            if (zeros > boxes - pos) die("zero run exceeds box count");
            pos += (size_t)zeros;
            uint64_t nonzeros = getvar(&r);
            if (nonzeros > boxes - pos) die("nonzero run exceeds box count");
            if (!zeros && !nonzeros) die("empty RLE record");
            for (uint64_t k = 0; k < nonzeros; k++) {
                if (!getvar(&r)) die("zero counter in nonzero run");
            }
            pos += (size_t)nonzeros;
        }
    }
    if (r.p != r.end) die("unexpected trailing bytes");
    free(data);
    qsort(in.mods, in.len, sizeof(*in.mods), cmp_input_mod);
    for (size_t i = 1; i < in.len; i++)
        if (!strcmp(in.mods[i-1].name, in.mods[i].name)) die("duplicate module in input");
    return in;
}
static Sig *lookup(const Group *g, const char *name) {
    for (Sig *s = g->buckets[bucket_of(name)]; s; s = s->next)
        if (!strcmp(s->name, name)) return s;
    return NULL;
}
typedef struct {
    size_t shared, conflicts;
    uint64_t boxes;
    const InputMod *bad_input;
    const Sig *bad_group;
} Comparison;
static Comparison compare(const Group *g, const Input *in) {
    Comparison c = {0};
    for (size_t j = 0; j < in->len; j++) {
        const InputMod *m = &in->mods[j];
        const Sig *s = lookup(g, m->name);
        if (!s) continue;
        if (s->hash != m->hash || s->n != m->n) {
            c.conflicts++;
            if (!c.bad_input) { c.bad_input = m; c.bad_group = s; }
        } else {
            c.shared++;
            if (UINT64_MAX - c.boxes < m->n) c.boxes = UINT64_MAX;
            else c.boxes += m->n;
        }
    }
    return c;
}
static void group_add(Group *g, const Input *in, size_t index, const char *path) {
    for (size_t j = 0; j < in->len; j++) {
        const InputMod *m = &in->mods[j];
        if (lookup(g, m->name)) continue;
        Sig *s = xmalloc(sizeof(*s));
        *s = (Sig){xstrdup(m->name), m->hash, m->n, path, g->buckets[bucket_of(m->name)]};
        g->buckets[bucket_of(m->name)] = s;
        g->modules++;
    }
    if (g->len == g->cap) {
        if (g->cap > SIZE_MAX / (2 * sizeof(*g->indices))) die("too many input files");
        g->cap = g->cap ? 2 * g->cap : 16;
        g->indices = xrealloc(g->indices, g->cap * sizeof(*g->indices));
    }
    g->indices[g->len++] = index;
}
static void group_free(Group *g) {
    for (size_t i = 0; i < BUCKETS; i++) {
        Sig *s = g->buckets[i];
        while (s) { Sig *next = s->next; free(s->name); free(s); s = next; }
    }
    free(g->indices);
    free(g->reason);
    free(g);
}
static int better(const Comparison *a, const Group *ga,
                  const Comparison *b, const Group *gb) {
    if (a->boxes != b->boxes) return a->boxes > b->boxes;
    if (a->shared != b->shared) return a->shared > b->shared;
    return ga->len > gb->len;
}
static char *reason_for_new_group(Group **groups, size_t ng, const Input *in) {
    if (!ng) return xstrdup("first input");
    size_t nearest = SIZE_MAX, rejected = 0, unrelated = 0;
    Comparison closest = {0};
    for (size_t i = 0; i < ng; i++) {
        Comparison c = compare(groups[i], in);
        if (c.conflicts) {
            rejected++;
            if (nearest == SIZE_MAX || c.boxes > closest.boxes ||
                (c.boxes == closest.boxes && c.shared > closest.shared)) {
                nearest = i;
                closest = c;
            }
        } else if (!c.shared) unrelated++;
    }
    char msg[1024];
    if (nearest != SIZE_MAX) {
        const InputMod *m = closest.bad_input;
        const Sig *s = closest.bad_group;
        snprintf(msg, sizeof(msg),
                 "incompatible with %zu group(s); closest group %04zu shares "
                 "%zu modules (%" PRIu64 " boxes), but %zu conflict(s); "
                 "e.g. %s: existing hash=%08" PRIx32 " boxes=%zu (from %s); "
                 "incoming hash=%08" PRIx32 " boxes=%zu",
                 rejected, nearest + 1, closest.shared, closest.boxes,
                 closest.conflicts, m->name, s->hash, s->n, s->first_path,
                 m->hash, m->n);
    } else {
        snprintf(msg, sizeof(msg),
                 "no shared module signatures with %zu existing group(s) (%zu disjoint)",
                 ng, unrelated);
    }
    return xstrdup(msg);
}

/* The second pass is the original dense-counter merger, one group at a time. */
static Module *acc_module(Acc *a, const char *name, uint32_t hash, size_t n) {
    size_t b = bucket_of(name);
    for (Module *m = a->buckets[b]; m; m = m->next) {
        if (strcmp(m->name, name)) continue;
        if (m->hash != hash || m->n != n) die("grouping error: incompatible module");
        return m;
    }
    if (n > SIZE_MAX / sizeof(uint64_t)) die("module too large");
    Module *m = xmalloc(sizeof(*m));
    *m = (Module){0};
    m->name = xstrdup(name);
    m->hash = hash;
    m->n = n;
    m->ticks = xcalloc(n ? n : 1, sizeof(uint64_t));
    m->next = a->buckets[b];
    a->buckets[b] = m;
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
    if (size < MAGIC_SIZE || memcmp(r.p, MAGIC, MAGIC_SIZE)) die("wrong magic/version");
    r.p += MAGIC_SIZE;
    uint64_t nmods = getvar(&r);
    if (++a->epoch == 0) die("epoch overflow");
    for (uint64_t j = 0; j < nmods; j++) {
        uint64_t length = getvar(&r);
        if (length > SIZE_MAX - 1 || length > (uint64_t)(r.end - r.p))
            die("invalid name length");
        char *name = xmalloc((size_t)length + 1);
        memcpy(name, r.p, (size_t)length);
        name[length] = 0;
        r.p += (size_t)length;
        uint32_t hash = get32(&r);
        uint64_t boxes = getvar(&r);
        if (boxes > SIZE_MAX / sizeof(uint64_t)) die("too many counters");
        Module *m = acc_module(a, name, hash, (size_t)boxes);
        free(name);
        if (m->seen_epoch == a->epoch) die("duplicate module in input");
        m->seen_epoch = a->epoch;
        size_t pos = 0;
        while (pos < m->n) {
            uint64_t zeros = getvar(&r);
            if (zeros > m->n - pos) die("zero run exceeds box count");
            pos += (size_t)zeros;
            uint64_t nonzeros = getvar(&r);
            if (nonzeros > m->n - pos) die("nonzero run exceeds box count");
            if (!zeros && !nonzeros) die("empty RLE record");
            for (uint64_t k = 0; k < nonzeros; k++, pos++) {
                uint64_t v = getvar(&r);
                if (!v) die("zero counter in nonzero run");
                if (v > UINT64_MAX - m->ticks[pos]) die("counter overflow");
                m->ticks[pos] += v;
            }
            if (nonzeros > UINT64_MAX - a->input_entries) die("entry count overflow");
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
static Buffer serialise(Acc *a) {
    Buffer b = {0};
    qsort(a->mods, a->len, sizeof(*a->mods), cmpmod);
    bytes(&b, MAGIC, MAGIC_SIZE);
    putvar(&b, a->len);
    for (size_t j = 0; j < a->len; j++) {
        Module *m = a->mods[j];
        size_t length = strlen(m->name);
        putvar(&b, length);
        bytes(&b, m->name, length);
        put32(&b, m->hash);
        putvar(&b, m->n);
        size_t i = 0;
        while (i < m->n) {
            size_t start = i;
            while (i < m->n && !m->ticks[i]) i++;
            putvar(&b, i - start);
            start = i;
            while (i < m->n && m->ticks[i]) i++;
            putvar(&b, i - start);
            for (size_t k = start; k < i; k++) putvar(&b, m->ticks[k]);
        }
    }
    return b;
}
static void acc_free(Acc *a) {
    for (size_t i = 0; i < a->len; i++) {
        free(a->mods[i]->name);
        free(a->mods[i]->ticks);
        free(a->mods[i]);
    }
    free(a->mods);
}
static void output_name(char *buf, size_t cap, size_t group) {
    if (snprintf(buf, cap, "merged-%04zu.tix", group + 1) >= (int)cap)
        die("too many output groups");
}
static void ensure_empty_output_dir(const char *in, const char *out) {
    if (mkdir(out, 0777) && errno != EEXIST) { perror(out); exit(1); }
    struct stat a, b;
    if (stat(in, &a) || stat(out, &b) || !S_ISDIR(b.st_mode))
        die("invalid input/output directory");
    if (a.st_dev == b.st_dev && a.st_ino == b.st_ino)
        die("output directory must differ from input directory");
    DIR *d = opendir(out);
    if (!d) { perror(out); exit(1); }
    struct dirent *e;
    while ((e = readdir(d))) {
        if (!strcmp(e->d_name, ".") || !strcmp(e->d_name, "..")) continue;
        closedir(d);
        die("output directory is not empty");
    }
    closedir(d);
}
static void checked_close(FILE *f) {
    if (fclose(f)) die("could not write report");
}
static void merge_dir(const char *dir, const char *dest) {
    context = dir;
    DIR *d = opendir(dir);
    if (!d) { perror(dir); exit(1); }
    char **paths = NULL;
    size_t n = 0, cap = 0;
    struct dirent *e;
    while ((e = readdir(d))) {
        if (!is_tix(e->d_name)) continue;
        char *path = join(dir, e->d_name);
        struct stat st;
        if (stat(path, &st) || !S_ISREG(st.st_mode)) { free(path); continue; }
        if (n == cap) {
            if (cap > SIZE_MAX / (2 * sizeof(*paths))) die("too many input files");
            cap = cap ? cap * 2 : 256;
            paths = xrealloc(paths, cap * sizeof(*paths));
        }
        paths[n++] = path;
    }
    closedir(d);
    if (!n) die("no input .tix files");
    qsort(paths, n, sizeof(*paths), cmpstr);
    ensure_empty_output_dir(dir, dest);

    Group **groups = NULL;
    size_t ng = 0, gcap = 0;
    Assignment *assign = xcalloc(n, sizeof(*assign));
    for (size_t i = 0; i < n; i++) {
        Input in = inspect(paths[i]);
        size_t best = SIZE_MAX;
        Comparison bestc = {0};
        for (size_t j = 0; j < ng; j++) {
            Comparison c = compare(groups[j], &in);
            if (c.conflicts || !c.shared) continue;
            if (best == SIZE_MAX || better(&c, groups[j], &bestc, groups[best])) {
                best = j;
                bestc = c;
            }
        }
        if (best == SIZE_MAX) {
            if (ng == gcap) {
                if (gcap > SIZE_MAX / (2 * sizeof(*groups))) die("too many groups");
                gcap = gcap ? gcap * 2 : 8;
                groups = xrealloc(groups, gcap * sizeof(*groups));
            }
            Group *g = xcalloc(1, sizeof(*g));
            g->reason = reason_for_new_group(groups, ng, &in);
            groups[ng] = g;
            best = ng++;
            fprintf(stderr, "New group %04zu from %s: %s\n",
                    best + 1, paths[i], g->reason);
        }
        group_add(groups[best], &in, i, paths[i]);
        assign[i] = (Assignment){best, bestc.shared, bestc.boxes};
        input_free(&in);
        if ((i + 1) % 1000 == 0)
            fprintf(stderr, "Classified %zu/%zu inputs into %zu groups\n", i + 1, n, ng);
    }

    char *manifest_path = join(dest, "assignments.tsv");
    FILE *manifest = fopen(manifest_path, "wb");
    if (!manifest) { perror(manifest_path); exit(1); }
    fprintf(manifest, "output\tinput\tshared_modules\tshared_boxes\n");
    for (size_t i = 0; i < n; i++) {
        char name[80];
        output_name(name, sizeof(name), assign[i].group);
        fprintf(manifest, "%s\t%s\t%zu\t%" PRIu64 "\n",
                name, paths[i], assign[i].shared_modules, assign[i].shared_boxes);
    }
    checked_close(manifest);
    free(manifest_path);

    char *report_path = join(dest, "groups.txt");
    FILE *report = fopen(report_path, "wb");
    if (!report) { perror(report_path); exit(1); }
    fprintf(report, "%zu input files -> %zu output groups\n", n, ng);
    fprintf(report, "Assignments: largest shared box count, then module count, "
                    "then existing file count; no incompatible modules permitted.\n\n");

    for (size_t gi = 0; gi < ng; gi++) {
        Group *g = groups[gi];
        Acc *a = xcalloc(1, sizeof(*a));
        for (size_t j = 0; j < g->len; j++) mergeone(a, paths[g->indices[j]]);
        if (a->len != g->modules) die("grouping error: module count changed");
        Buffer b = serialise(a);
        char name[80];
        output_name(name, sizeof(name), gi);
        char *target = join(dest, name);
        context = target;
        writefile(target, b.data, b.len);
        fprintf(stderr, "%s: %zu input files, %zu modules, %" PRIu64
                        " nonzero input counters, %zu output bytes\n",
                target, g->len, a->len, a->input_entries, b.len);
        fprintf(report, "%s: %zu files, %zu modules, %zu bytes\n",
                name, g->len, a->len, b.len);
        fprintf(report, "  Started by: %s\n  Reason: %s\n",
                paths[g->indices[0]], g->reason);
        for (size_t j = 0; j < g->len; j++) {
            size_t index = g->indices[j];
            fprintf(report, "  %s [matched %zu modules, %" PRIu64 " boxes]\n",
                    paths[index], assign[index].shared_modules, assign[index].shared_boxes);
        }
        fputc('\n', report);
        free(target);
        free(b.data);
        acc_free(a);
        free(a);
    }
    checked_close(report);
    fprintf(stderr, "Done: %zu files -> %zu output groups; diagnostics: %s\n", n, ng, report_path);
    free(report_path);
    for (size_t i = 0; i < ng; i++) group_free(groups[i]);
    free(groups);
    free(assign);
    for (size_t i = 0; i < n; i++) free(paths[i]);
    free(paths);
}
int main(int argc, char **argv) {
    if (argc != 3) {
        fprintf(stderr, "Usage: %s INPUT_DIRECTORY EMPTY_OUTPUT_DIRECTORY\n", argv[0]);
        return 2;
    }
    merge_dir(argv[1], argv[2]);
    return 0;
}
