from gnatpython.testsuite import Testsuite
from gnatpython.testsuite.driver import TestDriver
from gnatpython.fileutils import sync_tree
from gnatpython.ex import Run, STDOUT
from gnatpython.env import Env
import os


class ShellDriver(TestDriver):

    def tear_up(self):
        self.test_tmp = os.path.join(self.global_env['working_dir'],
                                     self.test_env['test_name'])
        sync_tree(self.test_env['test_dir'], self.test_tmp)
        self.register_path_subst(self.test_tmp, '<TEST_DIR>')

    def run(self):
        p = Run([os.environ['SHELL'], './test.sh'],
                cwd=self.test_tmp, error=STDOUT)
        self.result.actual_output = p.out

        with open(os.path.join(self.test_env['test_dir'], 'test.out'),
                  'rb') as fd:
            self.result.expected_output = fd.read()

    def analyze(self):
        self.analyze_diff()
        self.result.msg += '(%s)' % self.test_env['title']


class GSHTestsuite(Testsuite):
    TEST_SUBDIR = 'tests'
    DRIVERS = {'runshell': ShellDriver}
    default_driver = 'runshell'

    def add_options(self):
        self.main.add_option('--with-gsh',
                             metavar='FILE',
                             default=os.path.join(os.path.dirname(__file__),
                                                  '..', 'install'),
                             help="set location of gsh installation")
        self.main.add_option('--enable-coverage',
                             default=False,
                             action="store_true",
                             help="use gsh with gcov enabled to run the test")

    def tear_up(self):
        if self.main.options.enable_coverage:
            bin_dir = 'bin_dev'
        else:
            bin_dir = 'bin'
        os.environ['SHELL'] = os.path.join(self.main.options.with_gsh,
                                           bin_dir, 'gsh')

        Env().add_path(os.path.join(self.main.options.with_gsh, 'bin'))
        Env().add_path(os.path.dirname(os.environ['SHELL']))
