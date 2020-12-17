## Nix

```
nix-shell -I nixpkgs=~/src/nixpkgs/ shell-cross-mips.nix
```

## Configure

```
./boot && PATH=$PATH:~/src/ghc-mips-llvm/cross-bin ./configure --target=mips64el-unknown-linux-gnuabi64
```

```
./configure --target=mips64el-unknown-linux-gnu CONF_CC_STAGE1="-mips64r2 -mxgot -mlong-calls"
```


---

```
mips64el-unknown-linux-gnu-cc -iquotecompiler/GHC/Builtin -Iincludes -Iincludes/dist -Iincludes/dist-derivedconstants/header -Iincludes/dist-ghcconstants/header -Icompiler/stage2/build -Icompiler/stage2/build/./autogen -Icompiler/. -Icompiler/stage2 -Icompiler/stage2/build/. -Icompiler/stage2/build/stage2 -Iincludes/dist-install/build -Icompiler/stage2/build -no-pie -fno-PIC -mips64r2 -x assembler -c compiler/GHC/Builtin/PrimOps.s -o compiler/stage2/build/GHC/Builtin/PrimOps.o.tmp
```

```
mips64el-unknown-linux-gnu-cc -iquotecompiler/GHC/Builtin -Iincludes -Iincludes/dist -Iincludes/dist-derivedconstants/header -Iincludes/dist-ghcconstants/header -Icompiler/stage2/build -Icompiler/stage2/build/./autogen -Icompiler/. -Icompiler/stage2 -Icompiler/stage2/build/. -Icompiler/stage2/build/stage2 -Iincludes/dist-install/build -Icompiler/stage2/build -no-pie -fno-PIC -mips64r2 -x assembler -c compiler/GHC/Builtin/PrimOps.s -o compiler/stage2/build/GHC/Builtin/PrimOps.o.tmp
compiler/GHC/Builtin/PrimOps.s: Assembler messages:
compiler/GHC/Builtin/PrimOps.s:637578: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:637609: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:637677: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:637751: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:637822: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:637893: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:637964: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:638069: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:638174: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:638279: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:638380: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:638494: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:638608: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:638722: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:638836: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:638950: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:639073: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:639144: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:639215: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:639286: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:639357: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:639428: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:639499: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:639604: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:639709: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:639810: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:639924: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:640038: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:640152: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:640266: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:640380: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:640503: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:640574: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:640645: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:640716: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:640787: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:640858: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:640929: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:641034: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:641139: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:641240: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:641354: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:641468: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:641582: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:641696: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:641810: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:641933: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:642004: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:642075: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:642146: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:642217: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:642288: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:642359: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:642464: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:642569: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:642670: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:642784: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:642898: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:643012: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:643126: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:643240: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:643363: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:643434: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:643505: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:643576: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:643647: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:643718: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:643789: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:643903: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:644017: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:644131: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:644254: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:644368: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:644482: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:644596: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:644719: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:644833: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:644947: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:645061: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:645162: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:645263: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:645386: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:645509: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:645580: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:645651: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:645722: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:645793: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:645864: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:645935: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:646040: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:646145: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:646250: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:646355: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:646460: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:646565: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:646679: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:646793: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:646907: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:647021: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:647144: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:647267: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:647390: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:647504: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:647618: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:647741: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:647855: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:647969: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:648092: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:648228: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:648342: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:648456: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:648570: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:648671: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:648789: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:648907: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:649012: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:649083: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:649154: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:649225: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:649296: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:649367: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:649438: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:649539: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:649640: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:649741: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:649846: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:649947: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:650061: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:650175: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:650289: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:650403: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:650517: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:650631: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:650745: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:650859: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:650973: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:651087: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:651188: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:651289: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:651390: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:651495: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:651596: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:651697: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:651798: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:651899: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:652004: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:652105: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:652206: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:652307: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:652408: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:652509: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:652610: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:652711: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:652812: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:652913: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:653014: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:653115: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:653216: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:653317: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:653418: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:653519: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:653620: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:653691: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:653762: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:653833: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:653904: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:653975: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:654046: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:654160: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:654274: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:654388: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:654502: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:654603: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:654704: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:654809: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:654914: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:655015: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:655116: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:655217: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:655318: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:655419: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:655520: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:655621: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:655722: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:655823: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:655924: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:656025: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:656126: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:656227: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:656328: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:656429: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:656530: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:656631: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:656745: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:656855: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:656965: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:657036: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:657107: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:657178: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:657249: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:657320: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:657391: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:657505: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:657619: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:657733: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:657847: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:657948: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:658049: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:658154: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:658255: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:658356: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:658457: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:658558: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:658659: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:658760: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:658861: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:658962: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:659063: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:659164: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:659265: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:659366: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:659467: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:659568: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:659669: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:659770: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:659871: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:659985: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:660090: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:660200: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:660383: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:660549: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:660737: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:660942: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:661069: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:661213: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:661362: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:661533: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:661704: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:661940: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:662176: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:662338: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:662539: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:662740: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:662941: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:663159: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:663342: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:663508: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:663696: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:663884: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:664089: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:664216: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:664360: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:664531: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:664680: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:664851: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:665022: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:665258: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:665494: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:665656: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:665857: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:666058: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:666259: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:666477: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:666626: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:666775: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:666937: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:667064: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:667169: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:667274: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:667423: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:667594: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:667765: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:667919: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:668024: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:668151: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:668305: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:668427: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:668549: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:668667: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:668789: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:668911: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:669033: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:669155: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:669299: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:669417: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:669535: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:669653: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:669775: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:669897: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:670019: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:670141: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:670263: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:670385: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:670507: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:670625: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:670747: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:670869: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:670991: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:671113: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:671257: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:671375: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:671493: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:671615: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:671737: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:671859: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:671981: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:672152: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:672323: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:672494: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:672665: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:672836: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:673007: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:673178: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:673366: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:673537: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:673708: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:673879: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:674050: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:674221: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:674392: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:674563: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:674734: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:674905: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:675076: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:675247: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:675418: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:675589: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:675760: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:675931: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:676119: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:676290: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:676461: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:676632: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:676803: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:676974: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:677145: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:677333: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:677521: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:677705: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:677893: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:678081: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:678269: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:678457: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:678667: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:678851: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:679035: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:679219: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:679407: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:679595: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:679783: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:679971: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:680159: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:680347: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:680535: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:680719: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:680907: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:681095: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:681283: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:681471: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:681681: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:681865: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:682049: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:682237: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:682425: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:682613: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:682801: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:682958: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:683172: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:683391: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:683587: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:683788: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:683989: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:684186: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:684357: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:684541: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:684738: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:684922: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:685106: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:685290: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:685474: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:685658: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:685842: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:685991: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:686140: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:686294: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:686399: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:686526: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:686648: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:686766: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:686937: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:687108: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:687279: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:687450: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:687638: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:687831: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:688019: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:688212: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:688426: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:688645: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:688763: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:688881: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:688999: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:689104: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:689209: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:689280: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:689351: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:689422: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:689493: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:689564: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:689635: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:689757: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:689879: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:689997: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:690119: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:690237: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:690359: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:690481: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:690625: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:690743: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:690861: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:690979: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:691101: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:691223: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:691345: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:691467: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:691589: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:691755: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:691921: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:692087: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:692253: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:692419: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:692585: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:692751: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:692934: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:693100: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:693266: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:693432: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:693598: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:693764: Error: branch out of range 3
compiler/GHC/Builtin/PrimOps.s:693930: Error: branch out of range 3
```

```
nix-shell -I nixpkgs=~/src/nixpkgs/ "<nixpkgs>" -p pkgsCross.svenmips.buildPackages.binutils pkgsCross.svenmips.buildPackages.gcc8
```

---

`mips64el-linux-gnuabi64-objdump -D ./rts/dist/build/AutoApply.o`
```
00000000000000c0 <stg_ap_v64_info>:
        ...
  c8:   00003fc8        0x3fc8
  cc:   00000000        nop
  d0:   0000001e        ddiv    zero,zero,zero
        ...
```

```
	.type	stg_ap_v64_info,@object # @stg_ap_v64_info
	.data
	.globl	stg_ap_v64_info
	.p2align	4
stg_ap_v64_info:
	.8byte	stg_ap_v64_ret$def
	.8byte	16328                   # 0x3fc8
	.4byte	30                      # 0x1e
	.4byte	0                       # 0x0
	.size	stg_ap_v64_info, 24
```
