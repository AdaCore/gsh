GSH - A POSIX shell for Windows
===============================

General Information
-------------------

GSH is an implementation of a [POSIX shell](http://pubs.opengroup.org/onlinepubs/9699919799/)
developed for the Windows platform.

The aim of the project is to provide an efficient UNIX shell
instantiation for Windows, for non interactive usage.

GSH can be used to compile projects depending on autotools, UNIX make,...
As it targets specifically Windows platform, GSH differs significantly
from the most used ones such as bash, zsh. Among others, its implementation
does not depend on the ['fork system call'](https://en.wikipedia.org/wiki/Fork_%28system_call%29)
and doesn't emulate that system call (as it's done on Cygwin).
This allows better compiling performance (the build times can be up to 3 or
4 times faster than builds performed by projects such as Cygwin).

![XKCD on shell escaping](http://imgs.xkcd.com/comics/backslashes.png "Understanding shell escaping!")

Build & Installation
--------------------

Run:

    $ make
    $ make install PREFIX=<target directory>

Alternatively to just compile the `gsh` binary you can do:

    $ gprbuild -p -P posix_shell

Example of Usage
----------------

Currently the main goal of the project is to speed up builds of
[GNU](https://www.gnu.org) projects on Windows platform. As the project is
still not complete you still need a [Cygwin](http://www.cygwin.com)
installation for the tools not provided by GSH. The only requirement is that
the build should be done in a path for which Cygwin path maps directly to
a Windows path. For example if you do your build in c:/MyBuilds then in Cygwin
the path should map to /MyBuilds.

An example of build sequence that does not destroy your current Cygwin
environment:

    $ (export PATH=$GSH_INSTALL_DIR/bin:$PATH;
       export SHELL=$GSH_INSTALL_DIR/bin/gsh; 
       export CONFIG_SHELL=$GSH_INSTALL_DIR/bin/gsh;
       $CONFIG_SHELL/configure --prefix=/myinstall_dir
       make)
 

License
-------

All files are provided under terms of the
[GNU General Public License version 3](http://www.gnu.org/licenses/gpl-3.0.en.html).

The project includes fragments of other projects with licenses compatible
with the GNU General Public License version 3:

* [`src/readline`](src/readline) contains the source of the
  [GNU readline library](https://cnswww.cns.cwru.edu/php/chet/readline/rltop.html)
* [`src/gnulib`](src/gnulib) contains part of the
  [GNU Portability Library](https://www.gnu.org/software/gnulib/)
* [`lua/src`](lua/src) contains the sources of [Lua 5.2](http://www.lua.org/)
* [`gnutools`](gnutools) contains some executables from
  [GNUWin](http://gnuwin32.sourceforge.net/)
* [`os/src/ddk`](os/src/ddk) contains the DDK part of the
  [mingw-w64](http://mingw-w64.org/doku.php) project

