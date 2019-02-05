#pragma once

#include "NonMoving.h"

#include "BeginPrivate.h"

void nonmovingScavengeOne(StgClosure *p);
void scavengeNonmovingSegment(struct NonmovingSegment *seg);

#include "EndPrivate.h"
