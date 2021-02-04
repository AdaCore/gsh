from e3.fs import sync_tree
from e3.testsuite.result import TestStatus
from drivers import GNATcollTestDriver
from drivers.valgrind import check_call_valgrind
from e3.diff import diff
from e3.os.fs import unixpath
import os


class ShellScriptDriver(GNATcollTestDriver):
    """Default GNATcoll testsuite driver.

    In order to declare a test:

    1- Create a directory with a test.yaml inside
    2- Add test sources in that directory
    3- Add a main called test.adb that use support/test_assert.ads package.
    4- Do not put test.gpr there, it breaks the test, if you need a project
       file for testing, name it something else.
    5- If you need additional files for you test, list them in test.yaml:
       data:
           - "your_file1"
           - "your_file2"
    """

    def add_test(self, dag):
        """Declare test workflow.

        The workflow is the following::

            build --> check status

        :param dag: tree of test fragment to amend
        :type dag: e3.collection.dag.DAG
        """
        self.add_fragment(dag, 'check_run')

    def check_run(self, previous_values, slot):
        """Check status fragment."""
        skip = self.should_skip()
        if skip is not None:
            self.result.set_status(skip)
            self.push_result()
            return False

        sync_tree(self.test_env['test_dir'],
                  self.test_env['working_dir'])

        process = check_call_valgrind(
            self,
            [self.env.gsh, './test.sh'],
            timeout=self.process_timeout)

        with open(os.path.join(self.test_env['test_dir'],
                               'test.out'), 'r') as fd:
            expected_output = fd.read()
        actual_output = process.out.replace(self.test_env['working_dir'], '')
        actual_output = actual_output.replace('\\', '/')
        actual_output = actual_output.replace(
            unixpath(self.test_env['working_dir']),
            '')
        d = diff(actual_output, expected_output)
        if not d:
            self.result.set_status(TestStatus.PASS)
        else:
            self.result.set_status(TestStatus.FAIL, d)
        self.push_result()
