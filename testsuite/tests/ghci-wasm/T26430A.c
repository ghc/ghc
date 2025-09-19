void foo(void);

void bar(void (*)(void));

__attribute__((export_name("baz"))) void baz(void) { bar(foo); }
