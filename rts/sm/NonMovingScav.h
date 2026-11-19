#pragma once

#include "NonMoving.h"

void nonmovingScavengeOne(StgClosure *p);
void scavengeNonmovingSegment(struct NonmovingSegment *seg);
