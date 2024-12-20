//#OPTIONS: CPP

/*
   Runtime inspection of Haskell data.

   The code generator can emit calls to these functions
 */
/*
function h$verify_rep_int64(x, y) {

}

function h$verify_rep_word64(x, y) {

}
*/

/*
  an int rep is an integer in range [-2^31..2^31-1]
    (for Word# values, the value is treated as unsigned by the RTS. From
     JavaScript it still looks signed.
    )
 */
function h$verify_rep_int(x) {
  if(typeof x === 'number' &&
     (x|0)    === x
    ) return;
  throw new Error("invalid int rep " + h$show_val(x));
}

/*
function h$verify_rep_word(x, y) {

}
*/
/*
   a long rep is two integers in rage [-2^31..2^31-1]
 */
function h$verify_rep_long(x, y) {
  if(typeof x === 'number' &&
     typeof y === 'number' &&
     (x|0)    === x        &&
     (y|0)    === y
    ) return;
  throw new Error("invalid long rep " + h$show_val(x) + " " + h$show_val(y));
}

/*
function h$verify_rep_float(x) {

}
*/

function h$verify_rep_double(x) {
  if(typeof x === 'number') return;
  throw new Error("invalid double rep " + h$show_val(x));
}

/*
  an array rep is a JavaScript array. The elements are other
  array reps or heap objects.
 */
function h$verify_rep_arr(x) {
  if(h$verify_rep_is_arr(x)) return;
  throw new Error("invalid array rep " + h$show_val(x));
}

function h$verify_rep_is_arr(x) {
  // XXX check the elements?
  return (typeof x === 'object'
          && x
          && Array.isArray(x)
          // XXX enable this check
          // && x.__ghcjsArray === true
        );
}

function h$verify_rep_rtsobj(x) {
  // unspecified unlifted value
}

/*
  an rts object rep is one of the known RTS object types
 */
function h$verify_rep_is_rtsobj(o) {
 return (o instanceof h$MVar ||
         o instanceof h$MutVar ||
         o instanceof h$TVar ||
         o instanceof h$Transaction ||
         o instanceof h$Thread ||
         o instanceof h$Weak ||
         o instanceof h$StableName ||
         h$verify_rep_is_bytearray(o) ||
         h$verify_rep_is_arr(o));
}

function h$verify_rep_is_bytearray(o) {
  return (typeof o === 'object' &&
          o &&
          typeof o.buf === 'object' &&
          o.buf &&
          o.buf instanceof ArrayBuffer &&
          typeof o.len === 'number');
}

/*
  a heap object rep is either an object or an unboxed heap object

  unboxed heap objects store evaluated values of type 'number' or 'boolean'
  without wrapping them in a normal heap object. this is only done for
  data types with a single constructor and a single field of an appropriate type
 */
function h$verify_rep_heapobj(o) {
  // possibly an unlifted rts object
  // XXX: we should do a different check for these
  if(h$verify_rep_is_rtsobj(o)) return h$verify_rep_rtsobj(o);
  // unboxed rep
  if(typeof o === 'number' || typeof o === 'boolean') return;
  // boxed rep
  if(typeof o      === 'object'   &&
     o                         &&
     typeof o.f    === 'function' &&
     typeof o.f.a  === 'number'   &&
     (typeof o.m === 'number' || (typeof o.m === 'object' && o.m))
   ) return;
  throw new Error("invalid heapobj rep " + h$show_val(o));
}

/*
   an addr rep is a data object and an integer offset
 */
function h$verify_rep_addr(v, o) {
  if(typeof o === 'number' &&
     (o|0)    === o        &&
     // o        >=  0        && // XXX we could treat it as unsigned, should we?
     typeof v === 'object'
    ) return;
  throw new Error("invalid addr rep " + h$show_val(v) + " " + o);
}

/*
   v must be a value of type tc that can be matched
 */
function h$verify_match_alg(tc, v) {
  if(typeof v === 'boolean') {
    if(tc === "ghc-internal:GHC.Internal.Types.Bool") return;
    throw new Error("invalid pattern match boolean rep " + tc);
  } else if(typeof v === 'number') {
    // h$log("h$verify_match_alg number: " + tc);
    return;
  } else if(typeof v === 'object') {
    // h$log("verify_match_alg_obj: " + tc);
    if(!(typeof v.f    === 'function' &&
         typeof v.f.a  === 'number'   &&
         typeof v.f.t  === 'number'   &&
         v.f.t         === 2 /// con
       )) {
         throw new Error("not a data constructor " + tc + ": " + h$show_val(v));
    }
    // XXX add check for the type
    return;
  }
  throw new Error("invalid pattern match rep " + tc + ": " + h$show_val(v));
}

/*
   debug show object
 */

function h$show_val(o) {
  if(typeof o === 'undefined') return '<undefined>'
  if(o === null) return '<null>'
  if(typeof o !== 'object') return '[' + (typeof o) + ': ' + o + ']'
  return '' + o + ' [' + o.constructor.name + '] ' + h$collectProps(o);
}
