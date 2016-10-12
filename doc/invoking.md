# Invoking GSH

## Most common usages

### Executing a script

In order to execute for example `script.sh` do:

    $ gsh.exe script.sh

Parameters and options can optionally be passed to the script:

    $ gsh.exe script.sh param1 param2

### Executing arbitrary commands

As with other Unix shells you can use the `-c` option:

    $ gsh.exe -c 'if test $USER -ne ""; then echo "user $USER"; fi'

Necessary quoting might depend on the shell used to launch *GSH*. For example
launching the following command from `cmd.exe`:

   $ gsh.exe -c "DUMMY_VAR=OK; echo ${DUMMY_VAR:-NOTOK}"

will display `OK` whereas when launched from a Cygwin shell it will output
`NOTOK` (because in that case the expansion of ``DUMMY_VAR`` is done before
loaunching GSH - use single quote to solve the issue).

Additional positional parameters will be interpreted be used to set `$0`, `$1`,...

### Interactive Mode

GSH has a basic interactive mode. To enable it use the `-i` option.

## GSH Options Reference

* `--`

      Parameters after `--` will not be considered as options even if starting
      with a `-`.

* `-c`

      Script will be the the first positional parameter. Additional positional
      parameters will be used to set `$0`, `$1,...

* `--enable-trace NAME` | `-t NAME`

      Enable some internal traces. The option has no effect if *GSH* is not
      compiled in debug mode. Valid values for NAME are LEXER, PARSER, EVAL,
      SUBST,... For a complete list check ``src/sh-traces.ads``

* `-i`

      Enable interactive mode.

* `--login`

      Set shell as login shell (nohup)

* `-n`

      No execution mode. 0 is returned if the script can be parsed.

* `--norc`

     Disable automatic execution of startup scripts. This is currently a
     nohup. The option is present just for compatibility with other shells

* `-s`

      Read script from standard intput.

* `-x`

      Each time a command is executed a trace is displayed on standard error.

