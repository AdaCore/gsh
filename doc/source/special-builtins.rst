Break
=====

The break commands allows to exit from a loop (for, while, until).

The syntax is::

    break [n]

Where ``n`` is a positive integer greater or equal to 1. The command will exit
from the ``nth`` enclosing loop. The default value is ``1``. In case ``n`` is
superior to the number of enclosing loop, the shell will continue execution
right after the end of the outermost loop.

The exit status of break is ``0`` on succesful completion or if ``break`` is
not called inside a loop construct. The status will be ``1`` if ``n`` is
an integer ``< 1``. If ``n`` is not an integer then the shell will exit with
a ``128`` status.


