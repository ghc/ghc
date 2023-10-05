int readBss(int i) {
  static int bss[1 << 20];
  return bss[i];
}
