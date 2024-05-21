# Diagrams

The diagrams are generated using Calligraphy[^1].

To re-generate the diagrams, run the following command on the distributed versions of haddock-library and haddock-api from Hackage:

```
# In the haddock-library directory
$ calligraphy --output-mermaid ~/haddock-library.mmd --collapse-modules
# In the haddock-api directory
$ calligraphy --output-mermaid ~/haddock-api.mmd --collapse-modules
```

[1]: https://github.com/jonascarpay/calligraphy#readme
