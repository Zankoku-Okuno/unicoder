Unicoder
==========

The unicoder reads in a source file and makes replacements. The goal is to 
allow ascii interfaces to be able to insert unicode easily, thus enabling 
nice-looking programming language syntax. The replacements overwrite the 
original file.

Note that when cleaning `somefile`, `somefile.tmp` is created, which may 
overwrite something you wanted to keep around. You have been warned.

Entering a unicode character is now as easy as writing something that matches 
'/\\\w+(\s|\.|$)' and running unicoder. The name is looked up in a config file, 
but does no replacement if the name is undefined. The purpose of the dot is to 
separate names from the following other letters. Really, just see the examples 
and everything will be clear.

The symbol configuration files are stored under `/etc/zankoku-okuno/unicoder/` 
and have a `.conf` extension. The format is simple: each non-blank line 
contains an alphabetic name, some whitespace, then the replacement unicode text.

Rather than providing the user a load of options, I expect users to hack the 
code if they need something customized. As you can see, the codebase is neither 
large nor complex. The one option I have added is to select between different 
configuration files.

Examples
--------

Assuming a config file that looks like this:

```
lambda λ
pi π
```

we can write this with a normal keyboard:

```
\lambda.x. x + \pi
```

and after unicodizing, we will get:

```
λx. x + π
```

and celebrate the nice, clean lambda-calculus.

Have no fear, however, code such as this:

```
id = \x -> x
newline_period = "\n."
```

Will remain unchanged, as `x` and `n` are not in the config file.

Pitfalls
--------

Even in something as simple as this, you may want to be aware of a few facts:

 * Whitespace or dot is needed after any replacement. `\lambda(x)` is not 
   replaced with `λ(x)`. Use `\lambda.(x)` in this case, and the dot will 
   gladly disappear.
 * Beware of adding names like `n` or `t` in your config file. If you are using 
   a language that isn't esoteric, you will probably change the meaning of your 
   code.
 * It _is_ still possible to mess up strings. For example, `"\neq."` → `"≠"` 
   instead of being equivalent to `"\n" ++ "eq."`. I conjecture that there is 
   no way to solve this problem without sacrificing idempotence.
 * When a replacement is made at the end of a file, a newline is added. This 
   really shouldn't hurt anything, which is exactly when you know unicoder will 
   in fact hurt something.
 * I've made no attempt to ensure safety, other than using Haskell. Make 
   backups if you are wary (and you editor doesn't already).

Thankfully, the pitfalls are realistically enumerable.

Changes
=======

v0.3.0
------

 * More than one configuration file can be stored and selected between on the 
   command line.
 * The role of `symbols.conf` is now dealt with by `default.conf`.

v0.2.0
------

 * There are no longer any failure modes of the parser. One could probably do a 
   formal proof that given finite input and no OS errors, unicoder will 
   complete without failure in a finite time.
 * No special handling of strings. Unicoder is therefore now language-agnostic.
