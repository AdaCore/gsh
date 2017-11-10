Running GNATcoll Testsuite
==========================

`The testsuite is currently under construction !`

Getting Started
-------------------

To run it you need to have **Python** installed along with the package
**e3-testsuite**.

To install e3-testsuite:

```sh
pip install git+https://github.com/AdaCore/e3-testsuite.git
```

Then do

```sh
./run-tests
```

By default the test suite will be run with the **GNATcoll** library found in
the environment.

A summary of the results will be displayed once the testsuite ends. Detailed
results and logs can be found for each test in the `out/new` subdirectory. In
this directory a **YaML** file will be created for each test.

In order to have coverage information with **gcov**, just add `--gcov`. It
will recompiles **GNATcoll** with coverage information and a summary of the
coverage information will be displayed at the end of the test suite run. Full
coverage information can be found in `gcov/results` subdirectory.

Running the testsuite
--------------------------

### Partial Runs

In some contexts, it might be useful to run only subsets of the testsuite. In
order to do so you can use two different workflows.

#### E3-test

Call ``e3-test`` from any subdirectory of the testsuite will run only the
tests contained in that directory. This workflow is useful when working for
example in a single tests. By default you the testsuite will be run with
default parameters, but you can adjust the default parameters by editing the
**YaML** file called ``e3-test.yaml`` located in the root directory of the
testsuite. The ``default_args`` can be used to add default parameters such
as ``--gcov`` for example.

#### Run-tests

Call ``./run-tests`` with a list of test directories

### Which GNATcoll library is used ?

If the testsuite is launched without any argument, then the **GNATcoll** from
the user environment will be picked. If you add ``--gcov`` switch then
**GNATcoll** will be recompiled using sources from your current checkout. This
**GNATcoll** will be used for all tests except the one with the ``no-coverage``
marker in their description (see format of ``test.yaml`` section). Adding
``--recompile`` will recompile a **GNATcoll** library in production mode to
be used by the testsuite. If both ``--gcov`` and ``--recompile`` are used then
the production mode version of the library will be used only for tests with
the ``no-coverage`` marker (might be useful for tests doing some performance
measurement).

### Reference

Run ``run-tests --help`` to get the full list of options

Writing tests
-------------

Testcases are found in the ``tests`` subdirectory. A testcase is a directory
containing the a file called `test.yaml`.

A ``test.yaml`` looks like:

```yaml
# Mandatory
description: My test description

# Specify the test driver to be used. If not specified the default driver
# called 'default' is used
driver: driver_name
```

Some additional information which is driver specific might be present. To get
a list of available drivers look for ``DRIVERS`` dictionary in run-tests
script. From there you will be able to locate the **Python** class that
implement that driver along with its docstring.

### The default driver

In this readme we will only document the default driver. The default driver follow
the following workflow:

1. Check if the test should be skipped.
2. If not build the test
3. Run the test and check that the output contains some expected patterns

A minimal test should contains only one **Ada** unit called ``test.adb`` that
contains a function ``Test``. The skeleton of that function should look like

```ada

-- Following unit is provided by the testsuite in support subdirectory
with Test_Assert;

function Test return Integer is
begin
   Test_Assert.Assert (True, "my test is ok :-)");
   return Test_Assert.Report;
end Test;
```

You can override the default project by creating a file called ``test.gpr`` in
the test directory. You can also changed the name of the executable that is
executed by setting the ``test_exe`` key in ``test.yaml`` (default value is
``obj/test``).

For some specific case for which you never want to enable coverage
instrumentation, just add ``no-coverage: True`` to ``test.yaml``.

If you need some data files while running your tests, you need to specify them
using the ``data`` key. For example:

```yaml

description: Loading projects
data:
    - "*.gpr"
```

Will copy all local ``.gpr`` files to the working directory before executing
the test.

Test can also be skipped based on a set of given conditions. For example:

```yaml

description: A test
skip:
    - ['XFAIL', 'env.build.os.name == "windows"']
```

The skip entry is a list of tuple of the form (status, condition). If the
condition (a **Python** expression) is True then test is skipped and test
status set to ``status``. Note that currently only the following symbols are
available in the conditions: ``env`` (a BaseEnv object), ``test_env`` (the
test.yaml file as a **Python** dict) and the function ``disk_space`` (return
the available disk space in the working directory).

