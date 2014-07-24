Developer's Guide
###################

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


Building
========

Just run ``make``. In case you need a version with debugging information just do:

.. code-block:: bash

  make BUILD=dev


Installing
==========

TBD


Testing
=======

testsuite is available at::

    testsuite/


