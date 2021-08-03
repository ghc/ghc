/*
  simple set with reasonably fast iteration though an array, which may contain nulls
  elements must be objects that have a unique _key property
  collections are expected to be homogeneous

  when iterating over a set with an iterator, the following operations are safe:

   - adding an element to the set (the existing iterator will iterate over the new elements)
   - removing the last returned element through the iterator

   behaviour for deleting elements is unpredictable and unsafe
*/

/** @constructor */
function h$Set(s) {
    this._vals = [];
    this._keys = [];
    this._size = 0;
}

h$Set.prototype.size = function() {
    return this._size;
}

h$Set.prototype.add = function(o) {
#ifdef GHCJS_STRUCTS_ASSERTS
    if((typeof o !== 'object' && typeof o !== 'function') || typeof o._key !== 'number') throw ("h$Set.add: invalid element: " + o);
    if(this._size > 0) {
//        if(this._storedProto !== o.prototype) throw ("h$Set.add: unexpected element prototype: " + o)
    } else {
        this._storedProto = o.prototype;
    }
    if(this._keys[o._key] !== undefined && this._vals[this._keys[o._key]] !== o) throw ("h$Set.add: duplicate key: " + o);
#endif
    var k = this._keys, v = this._vals;
    if(k[o._key] === undefined) {
        k[o._key] = this._size;
        v[this._size++] = o;
    }
}

h$Set.prototype.remove = function(o) {
    if(this._size === 0) return;
    var k = this._keys, v = this._vals, x = k[o._key];
    if(x !== undefined) {
        delete k[o._key];
        var ls = --this._size;
        if(ls !== x) {
            var l = v[ls];
            v[x]      = l;
            k[l._key] = x;
        }
        v[ls] = undefined;
        if(v.length > 10 && 2 * v.length > 3 * ls) this._vals = v.slice(0, ls);
    }
}

h$Set.prototype.has = function(o) {
    return this._keys[o._key] !== undefined;
}

h$Set.prototype.clear = function() {
    if(this._size > 0) {
	this._keys = [];
	this._vals = [];
	this._size = 0;
    }
}

h$Set.prototype.iter = function() {
    return new h$SetIter(this);
}

// returns an array with all values, might contain additional nulls at the end
h$Set.prototype.values = function() {
    return this._vals;
}

/** @constructor */
function h$SetIter(s) {
    this._n = 0;
    this._s = s;
    this._r = true;
}

h$SetIter.prototype.next = function() {
    if(this._n < this._s._size) {
        this._r = false;
        return this._s._vals[this._n++];
    } else {
        this._r = true;
        return null;
    }
}

h$SetIter.prototype.peek = function() {
    if(this._n < this._s._size) {
        return this._s._vals[this._n];
    } else {
        return null;
    }
}

// remove the last element returned
h$SetIter.prototype.remove = function() {
    if(!this._r) {
        this._s.remove(this._s._vals[--this._n]);
        this._r = true;
    }
}

/*
  map, iteration restrictions are the same as for set
  keys need to be objects with a unique _key property

  keys are expected to have the same prototype

  values may be anything (but note that the values array might have additional nulls)
*/

/** @constructor */
function h$Map() {
    this._pairsKeys   = [];
    this._pairsValues = [];
    this._keys        = [];
    this._size        = 0;
}

h$Map.prototype.size = function() {
    return this._size;
}

h$Map.prototype.put = function(k,v) {
#ifdef GHCJS_STRUCTS_ASSERTS
    if((typeof k !== 'object' && typeof k !== 'function') || typeof k._key !== 'number') throw ("h$Map.add: invalid key: " + k);
    if(this._size > 0) {
        if(this._storedProto !== k.prototype) throw ("h$Map.add: unexpected key prototype: " + k)
    } else {
        this._storedProto = k.prototype;
    }
    if(this._keys[k._key] !== undefined && this._pairsKeys[this._keys[k._key]] !== k) throw ("h$Map.add: duplicate key: " + k);
#endif
    var ks = this._keys, pk = this._pairsKeys, pv = this._pairsValues, x = ks[k._key];
    if(x === undefined) {
        var n = this._size++;
        ks[k._key] = n;
        pk[n] = k;
        pv[n] = v;
    } else {
        pv[x] = v;
    }
}

h$Map.prototype.remove = function(k) {
    var kk = k._key, ks = this._keys, pk = this._pairsKeys, pv = this._pairsValues, x = ks[kk];
    if(x !== undefined) {
        delete ks[kk];
        var ss = --this._size;
        if(ss !== x) {
            var pks      = pk[ss];
            pk[x]        = pks;
            pv[x]        = pv[ss];
            ks[pks._key] = x;
        }
        pv[ss] = undefined;
        pk[ss] = undefined;

        if(pk.length > 10 && 2 * pk.length > 3 * this._size) {
            this._pairsKeys   = pk.slice(0,ss);
            this._pairsValues = pv.slice(0,ss);
        }
    }
}

h$Map.prototype.has = function(k) {
    return this._keys[k._key] !== undefined;
}

h$Map.prototype.get = function(k) {
    var n = this._keys[k._key];
    if(n !== undefined) {
        return this._pairsValues[n];
    } else {
        return null;
    }
}

h$Map.prototype.iter = function() {
    return new h$MapIter(this);
}

// returned array might have some trailing nulls
h$Map.prototype.keys = function () {
    return this._pairsKeys;
}

// returned array might have some trailing nulls
h$Map.prototype.values = function() {
    return this._pairsValues;
}

/** @constructor */
function h$MapIter(m) {
    this._n = 0;
    this._m = m;
}

h$MapIter.prototype.next = function() {
    return this._n < this._m._size ? this._m._pairsKeys[this._n++] : null;
}

h$MapIter.prototype.nextVal = function() {
    return this._n < this._m._size ? this._m._pairsValues[this._n++] : null;
}

h$MapIter.prototype.peek = function() {
    return this._n < this._m._size ? this._m._pairsKeys[this._n] : null;
}

h$MapIter.prototype.peekVal = function() {
    return this._n < this._m._size ? this._m._pairsValues[this._n] : null;
}

/*
  simple queue, returns null when empty
  it's safe to enqueue new items while iterating, not safe to dequeue
  (new items will not be iterated over)
*/
#ifndef GHCJS_QUEUE_BLOCK_SIZE
#define GHCJS_QUEUE_BLOCK_SIZE 1000
#endif

/** @constructor */
function h$Queue() {
    var b = { b: [], n: null };
    this._blocks = 1;
    this._first  = b;
    this._fp     = 0;
    this._last   = b;
    this._lp     = 0;
}

h$Queue.prototype.length = function() {
    return GHCJS_QUEUE_BLOCK_SIZE * (this._blocks - 1) + this._lp - this._fp;
}

h$Queue.prototype.isEmpty = function() {
    return this._blocks === 1 && this._lp >= this._fp;
}

h$Queue.prototype.enqueue = function(o) {
    if(this._lp === GHCJS_QUEUE_BLOCK_SIZE) {
        var newBlock = { b: [o], n: null };
        this._blocks++;
        this._last.n = newBlock;
        this._last = newBlock;
        this._lp = 1;
    } else {
        this._last.b[this._lp++] = o;
    }
}

h$Queue.prototype.dequeue = function() {
    if(this._blocks === 1 && this._fp >= this._lp) {
        return null;
    } else {
        var qfb = this._first.b, r = qfb[this._fp];
        qfb[this._fp] = null;
        if(++this._fp === GHCJS_QUEUE_BLOCK_SIZE) {
            if(this._blocks === 1) {
                this._lp = 0;
            } else {
                this._blocks--;
                this._first = this._first.n;
            }
            this._fp = 0;
        } else if(this._blocks === 1 && this._fp >= this._lp) {
            this._lp = this._fp = 0;
        }
        return r;
    }
}

h$Queue.prototype.peek = function() {
    if(this._blocks === 0 || (this._blocks === 1 && this._fp >= this._lp)) {
        return null;
    } else {
        return this._first.b[this._fp];
    }
}

h$Queue.prototype.iter = function() {
    var b = this._first, bp = this._fp, lb = this._last, lp = this._lp;
    return function() {
        if(b === null || (b === lb && bp >= lp)) {
            return null;
        } else {
            var r = b.b[bp];
            if(++bp === GHCJS_QUEUE_BLOCK_SIZE) {
                b = b.n;
                bp = 0;
                if(b === null) lb = null;
            }
            return r;
        }
    }
}

/*
   binary min-heap / set
   - iteration is not in order of priority
   - values can be removed, need to have the ._key property
*/

/** @constructor */
function h$HeapSet() {
    this._keys  = [];
    this._prios = [];
    this._vals  = [];
    this._size = 0;
}

h$HeapSet.prototype.size = function() {
    return this._size;
}

// add a node, if it already exists, it's moved to the new priority
h$HeapSet.prototype.add = function(op,o) {
#ifdef GHCJS_STRUCTS_ASSERTS
    if((typeof o !== 'object' && typeof o !== 'function') || typeof o._key !== 'number') throw ("h$HeapSet.add: invalid element: " + o);
    if(this._size > 0) {
        if(this._storedProto !== o.prototype) throw ("h$HeapSet.add: unexpected element prototype: " + o)
    } else {
        this._storedProto = o.prototype;
    }
    if(this._keys[o._key] !== undefined && this._vals[this._keys[o._key]] !== o) throw ("h$Set.add: duplicate key: " + o);
#endif
    var p = this._prios, k = this._keys, v = this._vals, x = k[o._key];
    if(x !== undefined) { // adjust node
        var oop = p[x];
        if(oop !== op) {
            p[x] = op;
            if(op < oop) {
                this._upHeap(x);
            } else {
                this._downHeap(x, this._size);
            }
        }
    } else { // new node
        var s = this._size++;
        k[o._key] = s;
        p[s]      = op;
        v[s]      = o;
        this._upHeap(s);
    }
}

h$HeapSet.prototype.has = function(o) {
    return this._keys[o._key] !== undefined;
}

h$HeapSet.prototype.prio = function(o) {
    var x = this._keys[o._key];
    if(x !== undefined) {
        return this._prios[x];
    } else {
        return null;
    }
}

h$HeapSet.prototype.peekPrio = function() {
    return this._size > 0 ? this._prios[0] : null;
}

h$HeapSet.prototype.peek = function() {
    return this._size > 0 ? this._vals[0] : null;
}

h$HeapSet.prototype.pop = function() {
    if(this._size > 0) {
        var v = this._vals[0];
        this._removeNode(0);
        return v;
    } else {
        return null;
    }
}

h$HeapSet.prototype.remove = function(o) {
    var x = this._keys[o._key];
    if(x !== undefined) this._removeNode(x);
}

h$HeapSet.prototype.iter = function() {
    var n = 0, v = this._vals, s = this._size;
    return function() {
        return n < s ? v[n++] : null;
    }
}

// may be longer than this.size(), remainder is filled with nulls
h$HeapSet.prototype.values = function() {
    return this._vals;
}

h$HeapSet.prototype._removeNode = function(i) {
    var p = this._prios, v = this._vals, s = --this._size, k = this._keys;
    delete k[v[i]._key];
    if(i !== s) {
        v[i]         = v[s];
        p[i]         = p[s];
        k[v[i]._key] = i;
    }
    v[s]         = null;
    p[s]         = null;
    this._downHeap(i,s);
}

h$HeapSet.prototype._downHeap = function(i,s) {
    var p = this._prios, v = this._vals, k = this._keys;
    var j,l,r,ti,tj;
    while(true) {
        j = i, r = 2*(i+1), l = r-1;
        if(l < s && p[l] < p[i]) i = l;
        if(r < s && p[r] < p[i]) i = r;
        if(i !== j) {
            ti         = v[i];
            tj         = v[j];
            v[j]       = ti;
            v[i]       = tj;
            k[ti._key] = j;
            k[tj._key] = i;
            ti         = p[i];
            p[i]       = p[j];
            p[j]       = ti;
        } else {
            break;
        }
    }
}

h$HeapSet.prototype._upHeap = function(i) {
    var ti, tj, j, p = this._prios, v = this._vals, k = this._keys;
    while(i !== 0) {
        j = (i-1) >> 1;
        if(p[i] < p[j]) {
            ti         = v[i];
            tj         = v[j];
            v[j]       = ti;
            v[i]       = tj;
            k[ti._key] = j;
            k[tj._key] = i;
            ti         = p[i];
            p[i]       = p[j];
            p[j]       = ti;
            i          = j;
        } else {
            break;
        }
    }
}
