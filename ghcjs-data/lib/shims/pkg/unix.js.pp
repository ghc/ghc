function h$mkdir(dir_d, dir_o, mode) {
#ifndef GHCJS_BROWSER
  if(h$isNode) {
    return h$handleErrno(-1, function() {
      h$fs.mkdirSync(h$decodeUtf8z(dir_d, dir_o), mode);
      return 0;
     });
  } else
#endif
    return h$unsupported(-1);
}

// TODO: stub, add real implementation
function h$geteuid() {
  return 1;
}

// TODO: stub, add real implementation
function h$sysconf() {
  return 0;
}

// TODO: stub, add real implementation
function h$getpwuid_r(uid, pwd_d, pwd_o, buf_d, buf_o, buflen, result_d, result_o) {
  var i, name = h$encodeUtf8("user"), max = Math.min(72, pwd_d.len);
  if(!result_d.arr) result_d.arr = [];
  result_d.arr[0] = [pwd_d, pwd_o];
  if(!pwd_d.arr) pwd_d.arr = [];
  // we don't really know where the pointers to strings are supposed to go,
  // so we just point to our dummy string everywhere
  for(i = 0; i < max; i+=4) pwd_d.arr[i+pwd_o] = [name, 0];
  for(i = 0; i < (max>>2); i++) pwd_d.i3[i+(pwd_o>>2)] = 1;
  return 0;
}
