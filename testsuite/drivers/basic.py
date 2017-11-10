from e3.fs import cp
from e3.testsuite.result import TestStatus
from drivers import gprbuild, GNATcollTestDriver
from drivers.valgrind import check_call_valgrind
import os


class BasicTestDriver(GNATcollTestDriver):
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
        self.add_fragment(dag, 'build')
        self.add_fragment(dag, 'check_run', after=['build'])

        if 'test_exe' not in self.test_env:
            self.test_env['test_exe'] = 'obj/test'

    def build(self, previous_values):
        """Build fragment."""
        skip = self.should_skip()
        if skip is not None:
            self.result.set_status(skip)
            self.push_result()
            return False

        if self.test_env.get('no-coverage'):
            gpr_project_path = self.env.gsh_prod_gpr_dir
        else:
            gpr_project_path = self.env.gsh_gpr_dir
        return gprbuild(self, gcov=self.env.gcov,
                        gpr_project_path=gpr_project_path)

    def check_run(self, previous_values):
        """Check status fragment."""
        if not previous_values['build']:
            return

        for data in self.test_env.get('data', []):
            cp(os.path.join(self.test_env['test_dir'], data),
               self.test_env['working_dir'], recursive=True)

        process = check_call_valgrind(
            self,
            [os.path.join(self.test_env['working_dir'],
                          self.test_env['test_exe'])],
            timeout=self.process_timeout)
        if '<=== TEST PASSED ===>' not in process.out:
            self.result.set_status(TestStatus.FAIL)
        else:
            self.result.set_status(TestStatus.PASS)
        self.push_result()
