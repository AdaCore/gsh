from gnatpython.testsuite import Testsuite
from gnatpython.testsuite.driver import TestDriver
from gnatpython.fileutils import sync_tree
from gnatpython.ex import Run, STDOUT
from gnatpython.fileutils import diff
from gnatpython.env import Env
import os


class ShellDriver(TestDriver):

    def tear_up(self):
        self.test_tmp = os.path.join(self.global_env['working_dir'],
                                     self.test_env['test_name'])
        sync_tree(self.test_env['test_dir'], self.test_tmp)

    def run(self):
        p = Run([os.environ['SHELL'], './test.sh'],
                cwd=self.test_tmp, error=STDOUT)
        self.result.actual_output = \
            p.out.replace('\r', '').replace(self.test_tmp + os.sep, '')

        with open(os.path.join(self.test_env['test_dir'], 'test.out'),
                  'rb') as fd:
            self.result.expected_output = fd.read().replace('\r', '')

    def analyze(self):

        self.result.diff = diff(self.result.actual_output.splitlines(),
                                self.result.expected_output.splitlines())
        if self.result.diff:
            self.result.set_status('FAILED', self.test_env['title'])
        else:
            self.result.set_status('PASSED', self.test_env['title'])


class GSHTestsuite(Testsuite):
    TEST_SUBDIR = 'tests'
    DRIVERS = {'runshell': ShellDriver}
    default_driver = 'runshell'

    def add_options(self):
        self.main.add_option('--with-gsh',
                             metavar='FILE',
                             default=os.path.join(os.path.dirname(__file__),
                                                  '..', 'install'))
        self.main.add_option('--enable-coverage',
                             default=False,
                             action="store_true")

    def tear_up(self):
        if self.main.options.enable_coverage:
            bin_dir = 'bin_dev'
        else:
            bin_dir = 'bin'
        os.environ['SHELL'] = os.path.join(self.main.options.with_gsh,
                                           bin_dir, 'gsh')

        Env().add_path(os.path.join(self.main.options.with_gsh, 'bin'))
        Env().add_path(os.path.dirname(os.environ['SHELL']))
