from e3.testsuite.result import TestStatus
from drivers.data_validation import DataValidationDriver
import os
import json
import logging


class JSONValidationDriver(DataValidationDriver):

    def validate_result(self, process, data_file, result):
        # Read data file
        with open(os.path.join(self.test_env['test_dir'], data_file)) as fd:
            expected = json.load(fd)

        got = json.loads(process.out)
        if got != expected:
            logging.debug('%s\n<=>\n%s', got, expected)
            result.set_status(TestStatus.FAIL)
            self.push_result(result)
            return TestStatus.FAIL
        else:
            return TestStatus.PASS
