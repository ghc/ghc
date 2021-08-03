{- |
Module      :  Language.Javascript.JMacro
Copyright   :  (c) Gershom Bazerman, 2010
License     :  BSD 3 Clause
Maintainer  :  gershomb@gmail.com
Stability   :  experimental

Simple DSL for lightweight (untyped) programmatic generation of Javascript.

A number of examples are available in the source of "Language.Javascript.JMacro.Prelude".

Functions to generate generic RPC wrappers (using json serialization) are available in
"Language.Javascript.JMacro.Rpc".

usage:

> renderJs [$jmacro|fun id x -> x|]

The above produces the id function at the top level.

> renderJs [$jmacro|var id = \x -> x;|]

So does the above here. However, as id is brought into scope by the keyword var, you do not get a variable named id in the generated javascript, but a variable with an arbitrary unique identifier.

> renderJs [$jmacro|var !id = \x -> x;|]

The above, by using the bang special form in a var declaration, produces a variable that really is named id.

> renderJs [$jmacro|function id(x) {return x;}|]

The above is also id.

> renderJs [$jmacro|function !id(x) {return x;}|]

As is the above (with the correct name).

> renderJs [$jmacro|fun id x {return x;}|]

As is the above.

> renderJs [$jmacroE|foo(x,y)|]

The above is an expression representing the application of foo to x and y.

> renderJs [$jmacroE|foo x y|]]

As is the above.

> renderJs [$jmacroE|foo (x,y)|]

While the above is an error. (i.e. standard javascript function application cannot seperate the leading parenthesis of the argument from the function being applied)

> \x -> [$jmacroE|foo `(x)`|]

The above is a haskell expression that provides a function that takes an x, and yields an expression representing the application of foo to the value of x as transformed to a Javascript expression.

> [$jmacroE|\x ->`(foo x)`|]

Meanwhile, the above lambda is in Javascript, and brings the variable into scope both in javascript and in the enclosed antiquotes. The expression is a Javascript function that takes an x, and yields an expression produced by the application of the Haskell function foo as applied to the identifier x (which is of type JExpr -- i.e. a Javascript expression).

Other than that, the language is essentially Javascript (1.5). Note however that one must use semicolons in a principled fashion -- i.e. to end statements consistently. Otherwise, the parser will mistake the whitespace for a whitespace application, and odd things will occur. A further gotcha exists in regex literals, whicch cannot begin with a space. @x / 5 / 4@ parses as ((x / 5) / 4). However, @x /5 / 4@ will parse as x(/5 /, 4). Such are the perils of operators used as delimeters in the presence of whitespace application.

Additional features in jmacro (documented on the wiki) include an infix application operator, and an enhanced destructuring bind.

Additional datatypes can be marshalled to Javascript by proper instance declarations for the ToJExpr class.

An experimental typechecker is available in the "Language.Javascript.JMacro.Typed" module.

-}

-- module names have been changed for temporary inclusion in GHCJS tree
module Compiler.JMacro (
  module Compiler.JMacro.Base,
  module Compiler.JMacro.Lens,
  module Compiler.JMacro.QQ
 ) where

import Compiler.JMacro.Base hiding (expr2stat)
import Compiler.JMacro.Lens
import Compiler.JMacro.QQ
