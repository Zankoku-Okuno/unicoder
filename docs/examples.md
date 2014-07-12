Unicoder Examples
=================

Unicoder is best learned by example. Or at least, that's the only way I can think to explain it without recourse to some [kind of formal grammar](sepcs.md). Well, Unicoder is pretty straightforward, so examples should do pretty well.

Command-line Usage
------------------

Unicoder can process one or more files. The replacements are made in-place, so the output is stored back into the input file.

```sh
unicoder file1 dir/file2
```

You can specify to use a different configuration file. The `default` configuration is used if none is specified. You can see where configuration files are stored using `unicoder --show-config-dir`. 

```sh
# use a config file from the config-dir
unicoder -c latex some_article.tex 
# use a config file relative to the current dir
unicoder -c ./myConfig some_file.txt
```

Before using a config file, it must first exist. For now, we've only distributed `default`. We'll get into the syntax of a config file [later](#user-content-configuration), but first let's see how the algorithm works. For now, we'll assume the default configuration.


Simple Replacements
-------------------

The algorithm replaces macros in the input with a string of unicode output. Utf-8 is the only supported codec, and in the interests of pushing standardization, we will not support other codecs.

A simple macro consists of a backslash and one or more alphabetic characters. The `default` config has many macros already configured; it is easy to read it to see what it defines. When a named macro cannot be found, it is simply passed through without change. 

> `\sqrt(\pi \div 2)` becomes `√(π ÷ 2)`

> `p \to q` becomes `p → q`

Sometimes, you want to have a unicode string immediately followed by alphabetic text. For example, since there is no macro named `lambdax`, unicoder won't replace `\lambdax` at all. In these cases, you can add a period after the macro name.

> `\integral.x dx = 1/2 * x ** 2` becomes `∫x dx = 1/2 * x ** 2`

If you want a period after a unicode string, the same period-trick will work

> `\square..3` becomes `□.3`

In addition to alphabetic characters, you can also use any of `~!%^&*_=+<>/?|-` in macro names.

> `p \-> q` becomes `p → q`


Bracketing Replacements
-----------------------

Often, there are pairs of unicode strings that enclose another string. For this, we allow "bracketing replacement". These macros are the same as simple replacements, but they take one argument, enclosed in braces (`{...}`). Note that, since the braces tidily set apart the macro from the rest of the text, you shouldn't insert extra periods.

> `English \angle{c} is pronounced either /k/ or /s/` becomes `English ‹c› is pronounced either /k/ or /s/`

You can also embed simple replacements in the argument,

> `\<{a \-> b}` becomes `⟨a → b⟩`

but you can't nest bracketing replacements, or even place close-braces in the argument. See how the closing bag and brace are transposed:

> `\bag{ {1,2,3} }` becomes `⟅ {1,2,3⟆ }`


Half-Brackets
-------------

To overcome the nesting problem, you can use either half of a bracketing-replacement individually. Simply use a backslash, followed by either an open or close brace, then the macro name.

> `\{Shell\floor{x/2}\}Shell` becomes `⟬⌊x/2⌋⟭`

You can also use just one at a time; they need not be paired together. Also, if you need to separate them from nearby text, you can use period, just as in simple replacements.

> `A \{ceil.Japanese quote\}floor` becomes `A ⌈Japanese quote⌋`


Configuration
-------------

Now that we've seen how the algorithm works, let's look at configuration. After all, you might want some symbols that aren't defined. Also, the syntax for a pattern might not fit with the source text: using the default config on a XeLaTeX document would probably end badly.

The first line of the configuration file is a lexer. In the default config, the lexer is

```
\ . { } a-zA-Z~!%^&*_=+<>/?|-
```

and specifies, in order, the begin-mark, end-mark, open-mark, close-mark, and the characters that can be used in a macro name. Note that you can use range notation (like `a-z` for all lowercase ascii letters) in the macro name characters. If you want to allow dashes in macro names, put the dash at the beginning or end (like we've done here). Further, the various marks need not be one character, they could be full strings.

As we mentioned, the default config steps all over the toes of LaTeX syntax. We can copy the default config and change its lexer to 

```
! . { } a-zA-Z~!%^&*_=+<>/?|-
```

Now, we can write text like `!lambda.x,y. \over{x}{y}` and after unicodizing, obtain `λx,y. \over{x}{y}`.

Although you can omit configuring some of the marks, it's recommended that you be explicit about them. If you leave them out, the end-mark and open-/close-marks won't even exist.

Now that the hard stuff is out of the way, we can move on to the easy task of adding macros.

If you were writing some chess notation, you might like some chess symbols. We can add a simple macro for a white king by adding the line `wN ♘` to the default config. Then we can write `\wN.F3` and obtain `♘F3` in documents.

If we needed a bracketing macro, we simply have two white-space separated replacements on one line, like `magic ᵐ ʰ`. Adding that line to the default config allows us to write `\magic{b}` and obtain `ᵐbʰ`. Just don't ask me what language has a series of pre-nasalized voiced aspirated plosives.

And that's it. The macro-definition syntax is just two or three whitespace-separated strings on one line. The configuration file isn't very strict about formatting. Any line that doesn't fit the syntax is just ignored.


Library Usage
-------------

In addition to being usable on the command-line, we've also exposed Unicoder as a Haskell library. You can use this library to create your own custom command-lines, or integrate Unicoder into a larger program, such as a pre-processor for a programming language.

The API documentation is hosted on [hackage](http://hackage.haskell.org/package/unicoder).
