
Until the build works properly. do

```
alex Lexer.x
```

in this directory to make the lexer.

And

```
happy --ghc Parser.y
```

in this directory to make the parser.

For debug

    happy --ghc -ad -i Parser.y
