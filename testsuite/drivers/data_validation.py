from e3.fs import rm
from e3.testsuite.result import TestStatus, TestResult
from drivers import GNATcollTestDriver, gprbuild
from drivers.valgrind import check_call_valgrind
import os


class DataValidationDriver(GNATcollTestDriver):
    """Data validation driver.

    For each test program call the program with data file defined in
    data_files key of the test. If the program returns 0 assume that
    the test passed.
    """

    def add_test(self, dag):
        self.add_fragment(dag, 'build')

        tear_down_deps = []
        for data_file, description in self.test_env['data_files'].items():
            tear_down_deps.append(data_file)
            self.add_fragment(
                dag,
                data_file,
                fun=lambda x, y, d=data_file, m=description:
                self.run_subtest(d, m, x, y),
                after=['build'])
        self.add_fragment(dag, 'tear_down', after=tear_down_deps)

    def run_subtest(self, data_file, description, previous_values, slot):
        test_name = self.test_name + '.' + data_file
        result = TestResult(test_name, env=self.test_env)

        if not previous_values['build']:
            return TestStatus.FAIL

        process = check_call_valgrind(
            self,
            [os.path.join(self.test_env['working_dir'],
                          self.test_env.get('validator', 'obj/test')),
             os.path.join(self.test_env['test_dir'], data_file)],
            result=result,
            timeout=self.process_timeout)

        return self.validate_result(process, data_file, result)

    def validate_result(self, process, data_file, result):
        # Read data file
        if '<=== TEST PASSED ===>' in process.out:
            return TestStatus.PASS
        else:
            result.set_status(TestStatus.FAIL)
            self.push_result(result)
            return TestStatus.FAIL

    def tear_down(self, previous_values, slot):
        failures = [v for v in previous_values.values() if
                    not isinstance(v, TestStatus) or v != TestStatus.PASS]
        if failures:
            self.result.set_status(TestStatus.FAIL,
                                   msg="%s subtests failed" % len(failures))
        else:
            self.result.set_status(TestStatus.PASS)

        self.push_result()

        if self.env.enable_cleanup:
            rm(self.test_env['working_dir'], recursive=True)

    def build(self, previous_values, slot):
        return gprbuild(self, gcov=self.env.gcov,
                        gpr_project_path=self.env.gsh_gpr_dir)
