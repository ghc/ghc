int foo() {
#if defined(FOO)
  return 1;
#else
  return 0;
#endif
}
