Changes
=======
v0.4.1
------
 * Fixed issue #1: Failure to xform inside macro argument

v0.4.0
------
 * Cabalized.
 * Config file location determined by cabal.
 * Split major functions into a library.
 * Allow two-part replacements.
 * No more tmp file needed.

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
