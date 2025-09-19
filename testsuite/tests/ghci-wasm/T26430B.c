__attribute__((export_name("foo"))) void foo(void) {}

__attribute__((export_name("bar"))) void bar(void (*f)(void)) { f(); }
