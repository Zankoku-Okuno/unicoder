Unicoder Specification
======================

Unicoder simplifies the input of unicode characters not immediately accessible from the keyboard.
Usually, a [few examples](examples.md) are enough to learn how to use unicoder.
This document goes in-depth to explain the details of unicoder's behavior and how it can be configured.

In this document, we make use of a BNF grammar extended by Perl-style regular expressions.


Configuration File
------------------

Since keyboards and textual formats vary, nearly every piece of unicoder is configurable.

Configuration consists of a set of marks and a list of replacements.

Each mark is a piece of text not containing whitespace. The four marks are called
`begin-mark`, `end-mark`, `open-mark` and `close-mark`. Once configured, they will
be used to generate the [conversion grammar](#user-content-conversion-algorithm).

A replacement can either be a one- or two-part replacement.
A one-part replacement consists of a `key-name` and a `value`, where both are text not containing whitespace.
A two-part replacement consists of a `key-name`, an `open-value` and a `close-value`, all text not containing whitespace.
These replacements are gathered into lookup tables and used in the [conversion grammar](#user-content-conversion-algorithm).

A grammar for the configuration file is given below.
Empty lines and space at the beginning or end of a line are ignored.
Note that the definition of `key-name` relies on the value of `key-name-range`.

```
config ::= lexer newline *(definition | ignore-line)
lexer ::= [ begin-mark [space end-mark] space ]
          [ open-mark space close-mark space ]
          key-name-range
key-name-range ::= /-?([^\s-]|\S-\S)*-?/
                   ;; A single character or a character range (e.g. 'a-z').
                   ;; A literal dash may be given at either end.
definition ::= key-name space value newline
            |  key-name space open-value space close-value newline
            |  '#include' space value newline
ignore-line ::= /.*/ newline

begin-mark,
  end-mark,
  open-mark,
  close-mark ::= text
key-name ::= /[$(key-name-range)]+/
             ;; one or more characters included in key-name-range setting
value,
  open-value,
  close-value ::= text

text           ::= /\S+/
space          ::= /\s+/
newline        ::= /[\n\r]+/
```

When `begin-mark` is not explicitly initialized it takes `\` (backslash) as its default. If no replacements are given, then the lookup table remains empty. All other settings are only initialized when explicitly given.

Conversion Algorithm
--------------------

Unicodizing a piece of text works by repeatedly applying the following grammar to the prefix of an input text. The matched prefix is consumed from the input and a replacement output is generated.

The `begin-mark`, `end-mark`, `open-mark`, `close-mark` rules are determined directly from the configuration.
The `key-one` and `key-two` rules are determined by the keys available for one- and two-part replacements, respectively. For configuration values which are not initialized, the corresponding rule fails.

```
convert ::= replace-one    ;; convert to value
         |  replace-two    ;; convert to open-value + text + close-value
         |  replace-open   ;; convert to open-value
         |  replace-close  ;; convert to close-value
         |  /./            ;; passthrough when all other options exhausted
replace-one   ::= begin-mark key-one [end-mark]
replace-two   ::= begin-mark key-two begin-mark /.*?/ end-mark
replace-open  ::= begin-mark open-mark key-two [end-mark]
replace-close ::= begin-mark close-mark key-two [end-mark]
```