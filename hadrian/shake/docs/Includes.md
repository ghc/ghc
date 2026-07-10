# Include Files

The [manual](Manual.md) describes how do do simple integration with `gcc -M`. This page describes how to go further, describing how to integrate with the Visual Studio alternative, and how to deal with generated imports.

The C pattern of `#include` files is probably the most brutal, so we focus on it first.

This pattern also generalising to linking, for example with Haskell.

#### Include files with Visual Studio

While `gcc` has the `-MMD` flag to generate a Makefile, the Visual Studio compiler `cl` does not. However, it does have a flag `-showincludes` which writes the include files on stdout as they are used. The initial example could be written using `cl` as:

    Stdout stdout <- cmd "cl -showincludes -c" [input] ["-Fo" ++ output]
    need [ dropWhile isSpace x
         | x <- lines stdout
         , Just x <- [stripPrefix "Note: including file:" x]]

The `stripPrefix` function is available in the `Data.List` module. One warning: the "including file" message is localised, so if your developers are using non-English versions of Visual Studio the prefix string will be different

#### Generated imports

The initial example compiles the C file, then calls `need` on all its source and header files. This works fine if the header files are all source code. However, if any of the header files are _generated_ by the build system then when the compilation occurs they will not yet have been built. In general it is important to `need` any generated files _before_ they are used.

To detect the included headers without using the compiler we can define `usedHeaders` as a top-level function:

    usedHeaders src = [init x | x <- lines src, Just x <- [stripPrefix "#include \"" x]]

This function takes the source code of a C file (`src`) and finds all lines that begin `#include "`, then takes the filename afterwards. This function does not work for all C files, but for most projects it is usually feasible to write such a function that covers everything allowed by your coding standards.

Assuming all interesting headers are only included directly by the C file (a restriction we remove in the next section), we can write the build rule as:

    "_build//*.o" %> \out -> do
        let c = dropDirectory1 $ out -<.> "c"
        src <- readFile' c
        need $ usedHeaders src
        cmd "gcc -c" [c] "-o" [out]


This code calls `readFile'` (which automatically calls `need` on the source file), then uses calls `need` on all headers used by the source file, then calls `gcc`. All files have `need` called on them before they are used, so if the C file or any of the header files have build system rules they will be run.

#### Generated transitive imports

The previous section described how to deal with generated include files, but only coped with headers included directly by the C file. This section describes how to extend that to work with generated headers used either in C or header files, even when used by headers that were themselves generated. We can write:

    ["*.c.dep","*.h.dep"] |%> \out -> do
        src <- readFile' $ dropExtension out
        writeFileLines out $ usedHeaders src

    "*.deps" %> \out -> do
        dep <- readFileLines $ out -<.> "dep"
        deps <- mapM (readFileLines . (<.> "deps")) dep
        writeFileLines out $ nub $ dropExtension out : concat deps

    "*.o" %> \out -> do
        deps <- readFileLines $ out -<.> "c.deps"
        need deps
        cmd "gcc -c" [dropExtension out] "-o" out

For simplicity, this code assumes all files are in a single directory and all objects are generated files are placed in the same directory. We define three rules:

* The `*.c.dep` and `*.h.dep` rule uses `|%>`, which defines a single action that matches multiple patterns. The file `foo.h.dep` contains a list of headers directly included by `foo.h`, using `usedHeaders` from the previous section.
* The `*.deps` rule takes the transitive closure of dependencies, so `foo.h.deps` contains `foo.h` and all headers that `foo.h` pulls in. The rule takes the target file, and all the `.deps` for anything in the `.dep` file, and combines them. More abstractly, the rule calculates the transitive closure of _a_, namely _a*_, by taking the dependencies of _a_ (say _b_ and _c_) and computing _a\* = union(a, b\*, c\*)_.
* The `*.o` rule reads the associated `.deps` file (ensuring it is up to date) and then depends on its contents.

The pattern of `*.deps` files occurs frequently, for example when linking Haskell files.
