# Compiling

* Install [node](https://nodejs.org/) and [npm](https://www.npmjs.com)
* Run `npm install` and `npm install gulp-cli -g` in this directory.
* Run `gulp` in this directory. This rebuilds the minified JS files.

# Development and manual testing

Generate Haddock docs for some Haskell project. Start an HTTP server in the Haddock docs directory.
(The `file://` protocol doesn't work since it doesn't allow AJAX requests.)

After each change to the TypeScript sources, compile and copy the generated files (JS and sourcemaps for better debugging) to the Haddock directory:

```
gulp && cp *.min.js path-to/generated-haddock-docs && cp *.js.map path-to/generated-haddock-docs
```