Unicodizer
==========

The unicoder reads in a source file and makes replacements. The goal is to allow 
ascii interfaces to be able to insert unicode easily, thus enabling nice-looking 
programming language syntax. The replacements overwrite the original file.

Note that when cleaning `somefile`, `somefile.tmp` is created, which may 
overwrite something you wanted to keep around. You have been warned.

There are two ways to input unicode characters: either `/\\\d+\.?/`, which 
inserts the unicode indexed by that decimal code point; or `/\\\w+\.?/`, which 
looks up the name in a symbol configuration file. The purpose of the dot is to 
separate names from the following other letters. Really, just see the examples
and everything will be clear.

The symbol configuration file is assumed to be in 
`/etc/zankoku-okuno/cleaner/symbols.conf`. The format is simple: each non-blank 
line contains an alphabetic name, some whitespace, then the replacement unicode 
text.

Rather than providing the user a load of options, I expect users to hack the 
code if they need something customized. As you can see, the codebase is neither 
large nor complex. Perhaps the most common customization will be changing where 
to look for a symbol file.

Examples
--------

Assuming a symbols.conf that looks like this:

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