Unicoder
==========

The unicoder reads in a source file and makes replacements. The goal is to 
allow ascii interfaces to be able to insert unicode easily, thus enabling 
nice-looking programming language syntax. The replacements overwrite the 
original file.

Note that when cleaning `somefile`, `somefile.unicoder.tmp` is created, which may 
overwrite something you wanted to keep around. You have been warned.

Entering a unicode character is now as easy as writing something that matches 
`/\\\w+(\s|\{\}|$)/` and running unicoder, though the actual regex is
configurable. The name is looked up in a config file, but does no replacement 
if the name is undefined. The purpose of the braces is to separate names from 
the following other letters. Really, just see the examples and everything will 
be clear.

The format for configuration files is simple: each non-blank line 
contains an alphabetic name, some whitespace, then the replacement unicode text.
If you want to use a config file stored in a different location, make sure
there's a slash in the filepath you pass.

Rather than providing the user a load of options, I expect users to hack the 
code if they need something customized. As you can see, the codebase is neither 
large nor complex. The one option I have added is to select between different 
configuration files.

Examples
--------

Assuming a config file that looks like this:

```
. abcdefghijklmnopqrstuvwxyz

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

 * Beware of adding names like `n` or `t` in your config file. If you are using 
   a language that isn't esoteric, you will probably change the meaning of your 
   code.
 * It _is_ still possible to mess up strings. For example, `"\neq"` → `"≠"` 
   instead of being equivalent to `"\n" ++ "eq"`. I conjecture that there is 
   no way to solve this problem without sacrificing idempotence.
 * I've made little attempt to ensure safety, other than using Haskell. Make 
   backups if you are wary (and your editor doesn't already).

Thankfully, the pitfalls are realistically enumerable.

Changes
=======

v0.3.1
------
 * Use config files from locations other than `/etc/zanoku-okuno/unicoder/`.
 * Use a `.unicoder.tmp` extension instead of `.tmp` so that there's less
   chance of name-collision with the tempfile.

v0.3.0
------
 * Each configuration file can specify separator string and valid name
   characters.
 * More than one configuration file can be stored and selected between on the 
   command line.
 * The role of `symbols.conf` is now dealt with by `default.conf`.
 * No more extra newline at end of file

v0.2.0
------

 * There are no longer any failure modes of the parser. One could probably do a 
   formal proof that given finite input and no OS errors, unicoder will 
   complete without failure in a finite time.
 * No special handling of strings. Unicoder is therefore now language-agnostic.
