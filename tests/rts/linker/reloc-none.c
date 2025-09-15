static int a[0];

int foo(){
    asm(".reloc ., R_AARCH64_NONE, 10");
    asm(".reloc ., R_AARCH64_NONE, a");
    asm(".reloc ., R_AARCH64_NONE, a+10");
    return a[0];
}
