#include <ghcjs/rts.h>

// JS Objects stuff

function h$isFloat (n) {
  return n===+n && n!==(n|0);
}

function h$isInteger (n) {
  return n===+n && n===(n|0);
}

/*
        -- 0 - null, 1 - integer,
        -- 2 - float, 3 - bool,
        -- 4 - string, 5 - array
        -- 6 - object
*/
function h$typeOf(o) {
    if (!(o instanceof Object)) {
        if (o == null) { 
            return 0; 
        } else if (typeof o == 'number') {
            if (h$isInteger(o)) {
                return 1;
            } else {
                return 2;
            }
        } else if (typeof o == 'boolean') {
            return 3;
        } else {
            return 4;
        }
    } else {
        if (Object.prototype.toString.call(o) == '[object Array]') {
            // it's an array
            return 5;
        } else if (!o) {
            // null 
            return 0;
        } else {
            // it's an object
            return 6;
        }
    }
}

function h$flattenObj(o) {
    var l = [], i = 0;
    for (var prop in o) {
        l[i++] = [prop, o[prop]];
    }
    return l;
}

/*

  build an object from key/value pairs:
    var obj = h$buildObject(key1, val1, key2, val2, ...);

  note: magic name:
    invocations of this function are replaced by object literals wherever
    possible

 */
function h$buildObject() {
    var r = {}, l = arguments.length;
    for(var i = 0; i < l; i += 2) {
        var k = arguments[i], v = arguments[i+1];
        r[k] = v;
    }
    return r;
}

// same as above, but from a list: [k1,v1,k2,v2,...]
function h$buildObjectFromList(xs) {
    var r = {}, k, v, t;
    while(IS_CONS(xs)) {
        xs = CONS_TAIL(xs);
        t = CONS_TAIL(xs);
        if(IS_CONS(t)) {
            k  = CONS_HEAD(xs);
            v  = CONS_HEAD(t);
            xs = CONS_TAIL(t);
            r[k] = v;
        } else {
            return r;
        }
    }
    return r;
}

// same as above, but from a list of tuples [(k1,v1),(k2,v2),...]
function h$buildObjectFromTupList(xs) {
    var r = {};
    while(IS_CONS(xs)) {
	var h = CONS_HEAD(xs);
	xs = CONS_TAIL(xs);
	r[JSVAL_VAL(TUP2_1(h))] = JSVAL_VAL(TUP2_2(h));
    }
    return r;
}
