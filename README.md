# What I Wish I Knew When Learning PureScript

A concise overview of the PureScript language and ecosystem, in the same style
as ["What I Wish I Knew When Learning
Haskell"](http://dev.stephendiehl.com/hask/) by Stephen Diehl.

## Basics

### Install PureScript

```bash
$ npm install -g purescript pulp psc-package
# Optional:
$ npm install -g purescript-psa 
```

### Starting a new project

Now we can use [Pulp](https://github.com/purescript-contrib/pulp) (PureScript's
build tool) to create a new empty project.

```bash
$ mkdir purescript-hello
$ cd purescript-hello/
$ pulp --psc-package init
$ pulp run
```

Note that we told Pulp to use
[psc-package](https://github.com/purescript/psc-package) as our package manager.
We are going to use it to install some additional dependencies. Do not include
the `purescript-` prefix of the package when using psc-package, like so:

```bash
$ psc-package install maybe
```

Note: you could also use Bower as a package manager, but psc-package problably
is a better choice as of now.

### PSCi

PSCi is the REPL for PureScript:

```bash
$ pulp psci
```

Importing modules on the REPL uses the same syntax as in the source code. In
PSCi you do not use `let` to bind variables (as of version 0.11). So, you can
write:

```purescript
> import Data.Maybe
> foo = Just 1
```

If you try to reassign an existing binding PSCi will complain. You either have
to chose a new variable name or you can optionally `:reload`. Which will remove
all bindings and reimports all your imported modules (compiling when necessary).

You can see the type of an expression with `:t` (or `:type`):

```purescript
> :t Just 1
Maybe Int
```

Another handy feature is `:paste` mode, which allows you to paste multiple lines
of code into PSCi, or to type a statement with multiple lines. You can finish
input by pressing `Ctrl-D` while on the last empty line.


## Types

The built-in types are defined in the
[Prim](https://pursuit.purescript.org/builtins/docs/Prim) module that is
embedded in the PureScript compiler (this module is implicitly imported in every
module).

### Number

A double precision floating point number (IEEE 754).

```purescript
> :t 42.0
Number
```

### Int

A 32-bit signed integer.

```purescript
> :t 42
Int
```

You can also use hexadecimal notation for Integer literals:

```purescript
> 0xff
255

> :t 0xff
Int
```

Note that you can't mix `Int` and `Number` in expressions like `add` and `div`. Use
`toNumber` from `Data.Int` (package `purescript-integers`) to convert an `Int`
to a `Number`.

### String

Strings are a built-in type in PureScript and correspond to the native string in
JavaScript. So, unlike Haskell they're not stored as a list of characters.

```purescript
> :t "Hello world!"
String
```

Multi-line string literals are also supported with triple quotes ("""):

```purescript
> :paste  -- paste mode allows us to type multi-line statements in PSCi
> multiline = """Hello
… world!"""
…  -- press Ctrl-D now to stop paste mode
> multiline
"Hello\nworld!"
```

String utility functions can be found in
[`purescript-strings`](https://github.com/purescript/purescript-strings). It
also contains functions for the `Char` type.

### Char

A single character (UTF-16 code unit). The JavaScript representation is a normal
String, which is guaranteed by the PureScript type system to contain one code
unit.

```purescript
> :t 'a'
Char
```

### Boolean

Either `true` or `false`. Note that the values are written in lowercase like in
JavaScript, in contrast with Haskell where they are written capitalized. Also,
the type is called `Boolean` instead of `Bool` as in Haskell.

```purescript
> true == false  -- equal
false

> true /= false  -- not equal
true

> true || false  -- or
true

> true && false  -- and
false

> not true       -- negation
false
```

### Array

Arrays are implemented using Javascript arrays, but must be homogeneous (all
elements must be of the same type). They support efficient random access. The
`Data.Array` module from
[`purescript-arrays`](https://github.com/purescript/purescript-arrays) provides
many functions for working with arrays.

```purescript
> import Data.Array
> xs = [1, 2, 3, 4, 5] 
> :t xs
Array Int
> head xs  -- head is a total function in PS
Just 1
```

### Records

Records correspond to JavaScript's objects, and record literals have the same
syntax as JavaScript's object literals:

```purescript
> lang = { title: "PureScript", strictEval: true, pure: true }
> lang.title
"PureScript"
```

<!--
TODO: pattern matching
TODO: record puns
TODO: polymorphic records
-->

### List

Lists are not a built-in type in PureScript, but are provided by the library
[`purescript-lists`](https://github.com/purescript/purescript-lists). Unlike in
Haskell, these lists are strict.

### Unit

PureScript has a type `Unit` used in place of Haskell's `()`. The Prelude module
provides a value `unit` that inhabits this type.

## Modules

### Defining a module

A source file must contain exactly one module. A module declaration looks like
this:

```purescript
module Main where

import Prelude
```

Module names do not need to match the filename, but it's
recommended. Module names should be unique within a project.

### Prelude

In PureScript the Prelude libraries are not bundled with the compiler. You need
to install the `purescript-prelude` library. Also, the prelude is not imported
automatically, just add the following line to the top of your module.

```purescript
import Prelude
```

### Main

The function `main` in the module with the name `Main` is the entry point of a
script.

```purescript
module Main where

import Control.Monad.Eff.Console (CONSOLE, log)

main :: forall eff. Eff (console :: CONSOLE | eff) Unit
main = log "Hello world!"
```

As you can see here in the type of `main`, PureScript has a type `Unit` used in
place of Haskell's `()`. The Prelude module provides a value `unit` that
inhabits this type.

### Importing modules

Imports must appear before other declarations in a module.

To open import a module:

```purescript
import Prelude
```

PureScript allows one open import per module. Usually this is `Prelude` (or an
alternative prelude like `neon` or `batteries`).

To import a specific set of members:
```purescript
import Prelude (head, tail)
```

Import one data constructor of a given type constructor:
```purescript
import Data.Maybe (Maybe(Just))
```

Importing all data constructors for a given type constructor:
```purescript
import Data.Maybe (Maybe(..))
```

Importing type classes:
```purescript
import Prelude (class Show)
```

Importing qualified:
```purescript
import Data.Maybe as Data.Maybe
```
Note that PureScript does not have the `qualified` keyword as in Haskell.

Only names that have been imported into a module can be referenced, and you can
only reference things exactly as you imported them.

Some examples:

| Import statement           | Exposed members |
|----------------------------|-----------------|
| `import X`                 | `A`, `f`        |
| `import X as Y`            | `Y.A`, `Y.f`    |
| `import X (A)`             | `A`             |
| `import X (A) as Y`        | `Y.A`           |
| `import X hiding (f)`      | `A`             |
| `import Y hiding (f) as Y` | `Y.A`           |

### Exporting modules

Export only a set of it's members:
```purescript
module A (runFoo, Foo(..)) where
```

Export a type class:
```purescript
module A (class B) where
```

Re-export a module in it's entirety:
```purescript
module A (module B) where
import B
```

Re-export the module itself in it's entirety:
```purescript
module A (module A, module B) where
import B
data ...
```


## Common type classes

### Relationships

![Type class hierarchy](http://g.gravizo.com/g?digraph%20G%20{%22Semigroupoid%22%20-%3E%20%22Category%22%22Functor%22%20-%3E%20%22Apply%22%22Apply%22%20-%3E%20%22Applicative%22%22Semigroup%22%20-%3E%20%22Monoid%22%22Monoid%22%20-%3E%20%22Foldable%22%20[style=dotted]%22Functor%22%20-%3E%20%22Traversable%22%22Foldable%22%20-%3E%20%22Traversable%22%22Applicative%22%20-%3E%20%22Traversable%22%20[style=dotted]%22Applicative%22%20-%3E%20%22Monad%22%22Apply%22%20-%3E%20%22Bind%22%22Bind%22%20-%3E%20%22Monad%22})

<!--
![Type class hierarchy](http://g.gravizo.com/g?
  digraph G {
    "Semigroupoid" -> "Category"
    "Functor" -> "Apply"
    "Apply" -> "Applicative"
    "Semigroup" -> "Monoid"
    "Monoid" -> "Foldable" [style=dotted]
    "Functor" -> "Traversable"
    "Foldable" -> "Traversable"
    "Applicative" -> "Traversable" [style=dotted]
    "Applicative" -> "Monad"
    "Apply" -> "Bind"
    "Bind" -> "Monad"
  }
)
-->

### Semigroupoid *(purescript-prelude)*

A Semigroupoid is similar to a Category but does not require an identity
element, just composable morphisms.

```purescript
class Semigroupoid a where
  compose :: forall b c d. a c d -> a b c -> a b d
```
`(<<<)` is an alias for `compose`. `(>>>)` is an alias for `flip compose`. So,
function composition is done with the `(<<<)` operator unlike `(.)` in Haskell.
The `.` is used for record field access in PureScript.

### Category *(purescript-prelude)*

`Category`s consist of objects and composable morphisms between them, and as
such are `Semigroupoid`s, but unlike `Semigroupoid`s must have an identity
element.

```purescript
class (Semigroupoid a) <= Category a where
  id :: forall t. a t t
```

### Semigroup *(purescript-prelude)*

The Semigroup type class identifies those types which support an append
operation to combine two values.

```purescript
class Semigroup a where
  append :: a -> a -> a
```

`(<>)` is an alias for `append`. The `(++)` operator as an alias for `append` is
removed in PureScript 0.9.1.

### Monoid *(purescript-monoid)*

The `Monoid` type class extends the `Semigroup` type class with the concept of
an empty value, called `mempty`.

```purescript
class Semigroup m <= Monoid m where
  mempty :: m
```

### Functor *(purescript-prelude)*

The map function allows a function to be “lifted” over a data structure.

```purescript
class Functor f where
  map :: forall a b. (a -> b) -> f a -> f b
```
`(<$>)` is an alias for `map`. `(<#>)` is an alias for `map` with its arguments
reversed.

### Foldable *(purescript-foldable-traversable)*

If the `Monoid` type class identifies those types which act as the result of a
fold, then the `Foldable` type class identifies those type constructors which
can be used as the source of a fold.

```purescript
class Foldable f where
  foldr :: forall a b. (a -> b -> b) -> b -> f a -> b
  foldl :: forall a b. (b -> a -> b) -> b -> f a -> b
  foldMap :: forall a m. Monoid m => (a -> m) -> f a -> m
```

### Apply *(purescript-prelude)*

The `Apply` type class is a subclass of `Functor`, and defines an additional
function `apply`. The difference between `map` and `apply` is that `map` takes a
function as an argument, whereas the first argument to `apply` is wrapped in the
type constructor `f`.

```purescript
class Functor f <= Apply f where
  apply :: forall a b. f (a -> b) -> f a -> f b
```

`(<*>)` is an alias for `apply`.

### Applicative *(purescript-prelude)*

Applicative is a subclass of `Apply` and defines the `pure` function. `pure`
takes a value and returns a value whose type has been wrapped with the type
constructor `f`.

```purescript
class Apply f <= Applicative f where
  pure :: forall a. a -> f a
```

### Traversable *(purescript-foldable-traversable)*

A traversable functor provides the ability to combine a collection of
side-effects which depend on its structure.

```purescript
class (Functor t, Foldable t) <= Traversable t where
  traverse :: forall a b f. Applicative f => (a -> f b) -> t a -> f (t b)
  sequence :: forall a f. Applicative f => t (f a) -> f (t a)
```

### Monad *(purescript-prelude)*

```purescript
class Apply m <= Bind m where
  bind :: forall a b. m a -> (a -> m b) -> m b

class (Applicative m, Bind m) <= Monad m
```

`(>>=)` is an alias for `bind`. Note: PureScript does not have `return` as an
alias for `pure`.


## Function types

### Function application

```purescript
($)   :: forall a b.                    (a -> b) ->   a ->   b
(<$>) :: forall a b f. (Functor f) =>   (a -> b) -> f a -> f b
(<*>) :: forall a b f. (Apply f)   => f (a -> b) -> f a -> f b
(=<<) :: forall m a b. (Bind m)    => (a -> m b) -> m a -> m b
(>>=) :: forall a b m. (Bind m)    => m a -> (a -> m b) -> m b
traverse :: forall a b m t. (Traversable t, Applicative m) => (a -> m b) -> t a -> m (t b)
foldMap  :: forall a m f.   (Foldable f, Monoid m)         => (a -> m)   -> f a -> m
```
Note that in PureScript `map` can be used instead of `liftA` or `liftM` in
Haskell. And that `traverse` replaces `mapM`.

### Composition

```purescript
(<<<) :: forall b c d a. (Semigroupoid a) => a c d -> a b c -> a b d
(>>>) :: forall a b c d. (Semigroupoid a) => a b c -> a c d -> a b d
(<=<) :: forall a b c m. (Bind m) => (b -> m c) -> (a -> m b) -> a -> m c
(>=>) :: forall a b c m. (Bind m) => (a -> m b) -> (b -> m c) -> a -> m c
```

### Discarding one of two values

```purescript
const :: forall a b.                    a ->   b ->   a
(<$)  :: forall f a b. (Functor f) =>   a -> f b -> f a
($>)  :: forall f a b. (Functor f) => f a ->   b -> f b
(<*)  :: forall a b f. (Apply f)   => f a -> f b -> f a
(*>)  :: forall a b f. (Apply f)   => f a -> f b -> f b
```

Note that Purescript does not have `(>>)` or `(<<)` as `Apply` is a superclass
of `Monad`.

### Restructuring

```purescript
sequence :: forall a m t. (Traversable t, Applicative m) => t (m a) -> m (t a)
join     :: forall a m.   (Bind m)                       => m (m a) -> m a
```

### Identity
```purescript
id   :: forall t a. (Category a)    => a t t
pure :: forall a f. (Applicative f) => a -> f a
```
