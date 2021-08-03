// some Enum conversion things

// an array of generic enums
var h$enums = [];
function h$initEnums() {
  for(var i=0;i<256;i++) {
    h$enums[i] = h$makeEnum(i);
  }
}
h$initStatic.push(h$initEnums);

function h$makeEnum(tag) {
  var f = function() {
    return h$stack[h$sp];
  }
  h$setObjInfo(f, 2, "Enum", [], tag+1, 0, [1], null);
#ifdef GHCJS_PROF
  return h$c0(f, h$CCS_SYSTEM);
#else
  return h$c0(f);
#endif
}

// used for all non-Bool enums
function h$tagToEnum(tag) {
  if(tag >= h$enums.length) {
    return h$makeEnum(tag);
  } else {
    return h$enums[tag];
  }
}

function h$dataTag(e) {
  return (e===true)?1:((typeof e !== 'object')?0:(e.f.a-1));
}

