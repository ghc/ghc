TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# Test that a .gbc file is emitted.
bytecode_object1:
	"$(TEST_HC)" $(TEST_HC_OPTS) BytecodeTest.hs -c -fbyte-code -fwrite-byte-code
	@[ -f BytecodeTest.gbc ] || (echo "ERROR: Expected BytecodeTest.gbc file not found"; echo "Files in current directory:"; ls -la; exit 1)

# Test that a -gbcdir works
bytecode_object2:
	"$(TEST_HC)" $(TEST_HC_OPTS) -c BytecodeTest.hs -gbcdir=bytecode -fbyte-code -fwrite-byte-code
	@[ -d bytecode ] || (echo "ERROR: Expected bytecode directory not found"; echo "Directories in current directory:"; ls -la | grep "^d"; exit 1)
	@[ -f bytecode/BytecodeTest.gbc ] || (echo "ERROR: Expected bytecode/BytecodeTest.gbc file not found"; echo "Files in bytecode directory:"; ls -la bytecode/; exit 1)

# Test that a -gbcsuf works
bytecode_object3:
	"$(TEST_HC)" $(TEST_HC_OPTS) -c BytecodeTest.hs -gbcsuf=bc -fbyte-code -fwrite-byte-code
	@[ -f BytecodeTest.bc ] || (echo "ERROR: Expected BytecodeTest.bc file not found"; echo "Files in current directory:"; ls -la; exit 1)

# Test that a .gbc file is emitted in --make mode.
bytecode_object4:
	"$(TEST_HC)" $(TEST_HC_OPTS) --make -no-link BytecodeTest.hs -fbyte-code -fwrite-byte-code
	@[ -f BytecodeTest.gbc ] || (echo "ERROR: Expected BytecodeTest.gbc file not found"; echo "Files in current directory:"; ls -la; exit 1)

# Test that a -gbcdir works in --make mode
bytecode_object5:
	"$(TEST_HC)" $(TEST_HC_OPTS) --make -no-link BytecodeTest.hs -fhide-source-paths -gbcdir=bytecode -fbyte-code -fwrite-byte-code
	@[ -d bytecode ] || (echo "ERROR: Expected bytecode directory not found"; echo "Directories in current directory:"; ls -la | grep "^d"; exit 1)
	@[ -f bytecode/BytecodeTest.gbc ] || (echo "ERROR: Expected bytecode/BytecodeTest.gbc file not found"; echo "Files in bytecode directory:"; ls -la bytecode/; exit 1)

# Test that a -gbcsuf works in --make mode
bytecode_object6:
	"$(TEST_HC)" $(TEST_HC_OPTS) --make -no-link BytecodeTest.hs -gbcsuf=bc -fbyte-code -fwrite-byte-code
	@[ -f BytecodeTest.bc ] || (echo "ERROR: Expected BytecodeTest.bc file not found"; echo "Files in current directory:"; ls -la; exit 1)

# Test that both .o and .gbc files are emitted with -fbyte-code-and-object-code
bytecode_object7:
	"$(TEST_HC)" $(TEST_HC_OPTS) -c BytecodeTest.hs -fbyte-code-and-object-code -fwrite-byte-code
	@[ -f BytecodeTest.o ] || (echo "ERROR: Expected BytecodeTest.o file not found"; echo "Files in current directory:"; ls -la; exit 1)
	@[ -f BytecodeTest.gbc ] || (echo "ERROR: Expected BytecodeTest.gbc file not found"; echo "Files in current directory:"; ls -la; exit 1)

# Test that -gbcdir works with -fbyte-code-and-object-code
bytecode_object8:
	"$(TEST_HC)" $(TEST_HC_OPTS) -c BytecodeTest.hs -gbcdir=bytecode -fbyte-code-and-object-code -fwrite-byte-code
	@[ -f BytecodeTest.o ] || (echo "ERROR: Expected BytecodeTest.o file not found"; echo "Files in current directory:"; ls -la; exit 1)
	@[ -d bytecode ] || (echo "ERROR: Expected bytecode directory not found"; echo "Directories in current directory:"; ls -la | grep "^d"; exit 1)
	@[ -f bytecode/BytecodeTest.gbc ] || (echo "ERROR: Expected bytecode/BytecodeTest.gbc file not found"; echo "Files in bytecode directory:"; ls -la bytecode/; exit 1)

# Test that -gbcsuf works with -fbyte-code-and-object-code
bytecode_object9:
	"$(TEST_HC)" $(TEST_HC_OPTS) -c BytecodeTest.hs -gbcsuf=bc -fbyte-code-and-object-code -fwrite-byte-code
	@[ -f BytecodeTest.o ] || (echo "ERROR: Expected BytecodeTest.o file not found"; echo "Files in current directory:"; ls -la; exit 1)
	@[ -f BytecodeTest.bc ] || (echo "ERROR: Expected BytecodeTest.bc file not found"; echo "Files in current directory:"; ls -la; exit 1)

# Test that -fbyte-code alone doesn't produce .hi or .o files
bytecode_object10:
	"$(TEST_HC)" $(TEST_HC_OPTS) -c BytecodeTest.hs -fbyte-code
	@[ ! -f BytecodeTest.hi ] || (echo "ERROR: Unexpected BytecodeTest.hi file found"; echo "Files in current directory:"; ls -la; exit 1)
	@[ ! -f BytecodeTest.o ] || (echo "ERROR: Unexpected BytecodeTest.o file found"; echo "Files in current directory:"; ls -la; exit 1)

# Test that -fbyte-code alone in --make mode doesn't produce .hi or .o files
bytecode_object11:
	"$(TEST_HC)" $(TEST_HC_OPTS) --make -no-link -v0 BytecodeTest.hs -fbyte-code
	@[ ! -f BytecodeTest.hi ] || (echo "ERROR: Unexpected BytecodeTest.hi file found"; echo "Files in current directory:"; ls -la; exit 1)
	@[ ! -f BytecodeTest.o ] || (echo "ERROR: Unexpected BytecodeTest.o file found"; echo "Files in current directory:"; ls -la; exit 1)

# Test what happens with -fbyte-code and a Main module in --make mode
bytecode_object12:
	"$(TEST_HC)" $(TEST_HC_OPTS) --make -v0 BytecodeMain.hs -fbyte-code
	@[ ! -f BytecodeMain.hi ] || (echo "ERROR: Unexpected BytecodeMain.hi file found"; echo "Files in current directory:"; ls -la; exit 1)
	@[ ! -f BytecodeMain.o ] || (echo "ERROR: Unexpected BytecodeMain.o file found"; echo "Files in current directory:"; ls -la; exit 1)
	@[ ! -f BytecodeMain ] || (echo "ERROR: Unexpected BytecodeMain executable found"; echo "Files in current directory:"; ls -la; exit 1)

# Test recompilation by generating .gbc first, then starting GHCi
bytecode_object13:
	"$(TEST_HC)" $(TEST_HC_OPTS) $(ghciWayFlags) -c BytecodeTest.hs -fbyte-code -fwrite-byte-code -fwrite-interface
	@[ -f BytecodeTest.gbc ] || (echo "ERROR: Expected BytecodeTest.gbc file not found"; echo "Files in current directory:"; ls -la; exit 1)
	@echo "Testing recompilation in GHCi..."
	"$(TEST_HC)" $(TEST_HC_OPTS_INTERACTIVE) -v1  -fbyte-code -fwrite-byte-code -fwrite-interface BytecodeTest.hs -e ":quit"

# Test recompilation after touching .hi file to make it newer than .gbc file
bytecode_object14:
	"$(TEST_HC)" $(TEST_HC_OPTS) $(ghciWayFlags) -c BytecodeTest.hs -fbyte-code -fwrite-byte-code -fwrite-interface
	@[ -f BytecodeTest.gbc ] || (echo "ERROR: Expected BytecodeTest.gbc file not found"; echo "Files in current directory:"; ls -la; exit 1)
	@[ -f BytecodeTest.hi ] || (echo "ERROR: Expected BytecodeTest.hi file not found"; echo "Files in current directory:"; ls -la; exit 1)
	@touch BytecodeTest.hi
	@echo "Testing recompilation in GHCi after touching .hi file..."
	"$(TEST_HC)" $(TEST_HC_OPTS_INTERACTIVE) -v1 -fbyte-code -fwrite-byte-code -fwrite-interface BytecodeTest.hs -e ":quit"

# Test recompilation by generating .gbc via GHCi , then starting GHCi again
bytecode_object15:
	"$(TEST_HC)" $(TEST_HC_OPTS_INTERACTIVE) -v1 -fbyte-code -fwrite-byte-code -fwrite-interface BytecodeTest.hs -e ":quit"
	@[ -f BytecodeTest.gbc ] || (echo "ERROR: Expected BytecodeTest.gbc file not found"; echo "Files in current directory:"; ls -la; exit 1)
	@echo "Testing recompilation in GHCi..."
	"$(TEST_HC)" $(TEST_HC_OPTS_INTERACTIVE) -v1 -fbyte-code -fwrite-byte-code -fwrite-interface BytecodeTest.hs -e ":quit"

# Test what happens when .gbc file is deleted before starting GHCi
bytecode_object16:
	"$(TEST_HC)" $(TEST_HC_OPTS) $(ghciWayFlags) -c BytecodeTest.hs -fbyte-code -fwrite-byte-code -fwrite-interface
	@[ -f BytecodeTest.gbc ] || (echo "ERROR: Expected BytecodeTest.gbc file not found"; echo "Files in current directory:"; ls -la; exit 1)
	@rm BytecodeTest.gbc
	@echo "Testing GHCi startup without .gbc file..."
	"$(TEST_HC)" $(TEST_HC_OPTS_INTERACTIVE) -v1 -fbyte-code -fwrite-byte-code -fwrite-interface BytecodeTest.hs -e ":quit"

# Test what happens with interface file but no bytecode file
# This should recompute from scratch since we are missing the .gbc file
bytecode_object17:
	"$(TEST_HC)" $(TEST_HC_OPTS) $(ghciWayFlags) -c BytecodeTest.hs -fwrite-if-simplified-core
	@[ -f BytecodeTest.hi ] || (echo "ERROR: Expected BytecodeTest.hi file not found"; echo "Files in current directory:"; ls -la; exit 1)
	@[ ! -f BytecodeTest.gbc ] || (echo "ERROR: Unexpected BytecodeTest.gbc file not found"; echo "Files in current directory:"; ls -la; exit 1)
	@echo "Testing GHCi startup with interface file but no bytecode file..."
	"$(TEST_HC)" $(TEST_HC_OPTS_INTERACTIVE) -v1 -fbyte-code -fwrite-byte-code -fwrite-interface -fwrite-if-simplified-core BytecodeTest.hs -e ":quit"

# Test what happens with interface file but no bytecode file (without -fwrite-byte-code)
# This should use the bindings in the .hi file to avoid recomputing from scratch.
bytecode_object18:
	"$(TEST_HC)" $(TEST_HC_OPTS) $(ghciWayFlags) -c BytecodeTest.hs -fwrite-if-simplified-core
	@[ -f BytecodeTest.hi ] || (echo "ERROR: Expected BytecodeTest.hi file not found"; echo "Files in current directory:"; ls -la; exit 1)
	@[ ! -f BytecodeTest.gbc ] || (echo "ERROR: Unexpected BytecodeTest.gbc file not found"; echo "Files in current directory:"; ls -la; exit 1)
	@echo "Testing GHCi startup with interface file but no bytecode file (without -fwrite-byte-code)..."
	"$(TEST_HC)" $(TEST_HC_OPTS_INTERACTIVE) -v1 -fbyte-code -fwrite-if-simplified-core -fwrite-interface BytecodeTest.hs -e ":quit"

# Test removing .gbc file and reloading in GHCi
# The module should be recompiled.
bytecode_object19:
	@echo "Testing GHCi with .gbc file removal and reload..."
	@cat bytecode_object19.script | "$(TEST_HC)" $(TEST_HC_OPTS_INTERACTIVE) -v1  -fbyte-code -fwrite-byte-code -fwrite-interface BytecodeTest.hs

# Test that -bytecodelib and -o pkg.bytecode in --make mode produces a bytecodelib
bytecode_object20:
	"$(TEST_HC)" $(TEST_HC_OPTS) --make -bytecodelib -o pkg.bytecode BytecodeTest.hs -fbyte-code -fwrite-byte-code
	@[ -f pkg.bytecode ] || (echo "ERROR: Expected pkg.bytecode file not found"; echo "Files in current directory:"; ls -la; exit 1)

# Test that -bytecodelib without -fbyte-code results in an error
bytecode_object21:
	! "$(TEST_HC)" $(TEST_HC_OPTS) --make -bytecodelib -o pkg.bytecode BytecodeTest.hs

# Test that you can link together .gbc files with -c and -bytecodelib
bytecode_object22:
	"$(TEST_HC)" $(TEST_HC_OPTS) -c BytecodeTest.hs -fbyte-code -fwrite-byte-code
	"$(TEST_HC)" $(TEST_HC_OPTS) -c -bytecodelib -o linked.bytecode BytecodeTest.gbc
	@[ -f linked.bytecode ] || (echo "ERROR: Expected linked.bytecode file not found"; echo "Files in current directory:"; ls -la; exit 1)

# Test that linking a bytecode library again doesn't trigger recompilation
bytecode_object23:
	"$(TEST_HC)" $(TEST_HC_OPTS) -bytecodelib -o linked.bytecode BytecodeTest.hs -fwrite-byte-code -fbyte-code -fwrite-interface
	"$(TEST_HC)" $(TEST_HC_OPTS) -bytecodelib -o linked.bytecode BytecodeTest.hs -fwrite-byte-code -fbyte-code -fwrite-interface

# Test that changing ByteCode.hs causes bytecode library to be recompiled
bytecode_object24:
	@echo "Testing that bytecode library is recompiled when BytecodeTest.hs changes..."
	"$(TEST_HC)" $(TEST_HC_OPTS) -v1 -bytecodelib -o linked.bytecode BytecodeTest.hs -fwrite-byte-code -fbyte-code -fwrite-interface
	@echo "new_defn = ()" >> BytecodeTest.hs
	"$(TEST_HC)" $(TEST_HC_OPTS) -v1 -bytecodelib -o linked.bytecode BytecodeTest.hs -fwrite-byte-code -fbyte-code -fwrite-interface

# Test that bytecode generation works with foreign functions
bytecode_object25:
	"$(TEST_HC)" $(TEST_HC_OPTS) -c BytecodeForeign.hs -fbyte-code -fwrite-byte-code -fwrite-interface $(ghciWayFlags)
	"$(TEST_HC)" $(TEST_HC_OPTS_INTERACTIVE) -v1  -fbyte-code -fwrite-byte-code -fwrite-interface BytecodeForeign.hs -e "testForeign"

