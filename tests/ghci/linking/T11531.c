extern void undefined_function(void);

int some_function(int d) {
  return 64;
}

void __attribute__ ((constructor)) setup(void) {
  undefined_function();
}
