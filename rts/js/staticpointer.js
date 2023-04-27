//#OPTIONS: CPP

// static pointers
var h$static_pointer_table      = null;
var h$static_pointer_table_keys = null;

function h$hs_spt_insert(key1,key2,key3,key4,ref) {
    //  h$log("hs_spt_insert: " + key1 + " " + key2 + " " + key3 + " " + key4 + " -> " + h$collectProps(ref));
    if(!h$static_pointer_table) {
	h$static_pointer_table      = [];
	h$static_pointer_table_keys = [];
    }
    if(!h$hs_spt_lookup_key(key1,key2,key3,key4)) {
        var ba = h$newByteArray(16);
        ba.i3[0] = key2;
        ba.i3[1] = key1;
        ba.i3[2] = key4;
        ba.i3[3] = key3;
	h$static_pointer_table_keys.push(ba);
        h$retain({ root: ref, _key: -1 });
    }
    var s = h$static_pointer_table;
    if(!s[key1])             s[key1]             = [];
    if(!s[key1][key2])       s[key1][key2]       = [];
    if(!s[key1][key2][key3]) s[key1][key2][key3] = [];
    s[key1][key2][key3][key4] = ref;
}

function h$hs_spt_key_count() {
    return h$static_pointer_table_keys ?
              h$static_pointer_table_keys.length : 0;
}

function h$hs_spt_keys(tgt_d, tgt_o, n) {
    var ks = h$static_pointer_table_keys;
    for(var i=0;(i<n&&i<ks.length);i++) {
      PUT_ADDR(tgt_d, tgt_o+4*i, ks[i], 0);
    }
    return Math.min(n,ks.length);
}

function h$hs_spt_lookup(key_v,key_o) {
    // We know that the array is freshly allocated so we don't have to care
    // about the offset (should be 0).
    //
    // note that the order of the keys is weird due to endianness
    var key2 = key_v.i3[0] >>> 0;
    var key1 = key_v.i3[1] >>> 0;
    var key4 = key_v.i3[2] >>> 0;
    var key3 = key_v.i3[3] >>> 0;
    RETURN_UBX_TUP2(h$hs_spt_lookup_key(key1,key2,key3,key4), 0);
}

function h$hs_spt_lookup_key(key1,key2,key3,key4) {
    // h$log("hs_spt_lookup_key: " + key1 + " " + key2 + " " + key3 + " " + key4);
    var s = h$static_pointer_table;
    if(s && s[key1] && s[key1][key2] && s[key1][key2][key3] &&
       s[key1][key2][key3][key4]) return s[key1][key2][key3][key4];
    return null;
}
