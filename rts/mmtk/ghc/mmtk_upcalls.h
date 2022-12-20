#pragma once

#include "mmtk.h"

bool upcall_is_task(void* task);
MMTk_Mutator *upcall_get_mutator(void *tls);

void upcall_spawn_gc_controller(void *controller);
void upcall_spawn_gc_worker(void *worker);
