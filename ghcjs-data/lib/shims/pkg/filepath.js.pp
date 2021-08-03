function h$filepath_isWindows() {
#ifndef GHCJS_BROWSER
    if(h$isNode && process.platform === 'win32') return true;
#endif
  return false;
}
