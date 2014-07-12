Unicoder
========

The unicoder reads in a source file and makes replacements in-place. The goal
is to allow ascii interfaces to be able to insert unicode without taking your
hands off the keyboard. This can allow for unicode to be entered into source
code or any other text document you're editing.

Entering unicode is not as easy as typing a special string (default backslash)
followed by an identifier. There's also syntax for wrapping content inside a
pair of unicode strings.
For example, with the default configuration, unicoder turns
`\floor{x} \def \lambda x. (floor x)` into `⌊x⌋ ≡ λ x. (floor x)`.
Admitedly, this is not a great syntax for some kinds of documents (esp. XeLaTeX),
but that's why we've allowed for configuration of each of the special marks as
well as the identifier character set, so Unicoder can be relevant to any type of
text data.

There's more documentation on our
[Viewdocs](http://zankoku-okuno.viewdocs.io/unicoder/).
If yo're learning to use Unicoder, I would especiallt recommend our
[examples](http://zankoku-okuno.viewdocs.io/unicoder/examples.md).

Examples
--------

Assuming a config file that looks like this:

```
\ . { } a-z

lambda λ
pi π
bag ⟅ ⟆
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

There are also two-part replacements. These take a single (non-nested) argument, transforming

```
\bag{black}
```

into

```
⟅black⟆
```

You can also use each half of a two-part replacement individually. This is especially
usefule for nesting, but also when you simply have argument-close marks in the argument:

```
\{bag {} \}bag
```

becomes

```
⟅ {} ⟆
```

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


Contribute
==========

Unicoder is in the beta stage. I'm sure there's a bug or two, some cleanup to be
done, and definitely some missing features. Please add any issues or pull requests
to our [github](https://github.com/Zankoku-Okuno/unicoder). You can email me, but
that's usually higher latency than github.