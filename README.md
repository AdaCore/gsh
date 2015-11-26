GSH - A posix shell for Windows
===============================

General Information
-------------------

GSH is an implementation of a Posix shell developed for the Windows platform.
The current focus of the project is to provide an efficient Unix shell
implementation for Windows for non interactive usage.

GSH can be used to compile projects depending on autotools, Unix make, ...
As it targets specifically Windows platform, implementation differs quite
significantly from the most used one such as bash, zsh. For example the
shell implementation does not depends on the fork system call (and don't try
to emulate that system call as on Cygwin) and thus can achieve better
performance. The gain in time can be up to 3 or 4 in comparison to projects
such as Cygwin.

Installation
------------

Run:

    $ make
    $ make install PREFIX=<target directory>

Alternatively to just compile the gsh binary you can do:

    $ gprbuild -p -P posix_shell

License
-------

All files are provided under terms of the GNU General Public License version 3.

The project includes fragments of other projects with licenses compatible
with the GNU General Public License version 3:

* src/readline contains the source of the GNU readline library [website](https://cnswww.cns.cwru.edu/php/chet/readline/rltop.html)
* src/gnulib contains part of the GNU Portability Library [website](https://www.gnu.org/software/gnulib/)
* lua/src contains the sources of Lua 5.2 [website](http://www.lua.org/)
* gnutools contains some executables from the GNUWin project [website](http://gnuwin32.sourceforge.net/)
* os/src/ddk contains the DDK part of the mingw-w64 project [website](http://mingw-w64.org/doku.php)
