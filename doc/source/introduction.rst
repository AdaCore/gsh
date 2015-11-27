Introduction
============

What is GSH ?
-------------

**GSH** is an implementation of a Posix shell developed for the **Windows**
platform. The current focus of the project is to provide an efficient
**Unix** shell implementation for **Windows** for non interactive usage.

**GSH** can be used to compile projects depending on autotools, Unix make, ...
As it targets specifically **Windows** platform, implementation differs quite
significantly from the most used one such as bash, zsh. For example the shell
implementation does not depends on the fork system call (and don't try to
emulate that system call as on **Cygwin**) and thus can achieve better
performance. The gain in time can be up to 3 or 4 in comparison
to projects such as Cygwin.

