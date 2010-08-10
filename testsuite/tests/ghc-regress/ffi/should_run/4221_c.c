#include<stdio.h>
#include<stdlib.h>

typedef double (*hs_function_ptr)(double);

typedef struct {
  hs_function_ptr fn;
  void (*free_fn)(hs_function_ptr);
} fn_blob;

fn_blob* create_fn_blob(hs_function_ptr fn, void (*free_fn)(hs_function_ptr)) {
  fn_blob* new_blob = malloc(sizeof(fn_blob));
  new_blob->fn = fn;
  new_blob->free_fn = free_fn;
  return new_blob;
}

double call_fn_blob(fn_blob* fn_blob, double arg) {
  return(fn_blob->fn(arg));
}

void free_fn_blob(fn_blob* dead_blob) {
  dead_blob->free_fn(dead_blob->fn);
  free(dead_blob);
}

