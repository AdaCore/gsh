Developer's Guide
###################

Objectives
==========

GSH aims at replacing **Cygwin** on Windows platforms for internal
usages of the *Production Team*.

Sources
=======

Here is a brief description of the **GSH** sources::

    src/                             The Ada and C sources
        posix_shell-lexer            The shell lexer
        posix_shell-parser           The shell parser
        posix_shell-tree             AST creation
        posix_shell-tree-eval        The interpreter
        posix_shell-subst            Handling of shell substitutions
                                     (paremeters, commands, ...)
        posix_shell-variables        Handling of shell states
                                     (variables, redirections, ...)
        builtins/                    Contains implementation of all builtins

tools and libraries::

    etc/                             default gsh configuration file
                                     (for interactive mode)
    gnulib/                          Used only on windows to get globbing
                                     patterns and regexp support
    gnutools/                        Windows version of unix tools not
                                     implemented as builtins (gnuwin32 project)
    readline/                        prompt support
                                     (for interactive mode)

.. _build:

Dev Hints
---------

the following points must be known by developers:

* The 'current directory' in GSH may not be the system's one.
  ALWAYS use 'Resolve_Path' function to get the absolute path
  when working on files.

* ALWAYS use the internal GSH `rm` function and not the runtime Ada rm,
  when removing files is necessary

Building
========

Just run ``make``.

In case you need a version with debugging information just do:

.. code-block:: bash

  $ make BUILD=dev


.. _install:

Installing
==========

run the following commands:

.. code-block:: bash

   $ make install

.. _test:

Testing
=======

Once gsh is build (ses :ref:`build`) and installed (ses :ref:`install`), run the following command:

.. code-block:: bash

   $ make check

testsuite framework and sources are available at::

    testsuite/

