/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2020
 *
 * Public API of closure printing functions.
 *
 * ---------------------------------------------------------------------------*/

#pragma once

// findPtrCb takes a callback of type FindPtrCb, so external tools (such as
// ghc-debug) can invoke it and intercept the intermediate results.
// When findPtrCb successfully finds a closure containing an address then the
// callback is called on the address of that closure.
// The `StgClosure` argument is an untagged closure pointer.
// `user` points to any data provided by the caller. It's not used internally.
typedef void (*FindPtrCb)(void *user, StgClosure *);

void findPtrCb(FindPtrCb cb, void *, P_ p);

// Special case of findPtrCb: Uses a default callback, that prints the closure
// pointed to by p.
void findPtr(P_ p, int follow);
