from gnatpython.testsuite import Testsuite
from gnatpython.testsuite.driver import TestDriver
from gnatpython.fileutils import sync_tree, ls, rm
from gnatpython.ex import Run, STDOUT
from gnatpython.env import Env
import os
import re
import yaml
import json
from itertools import izip_longest


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

    def tear_down(self):
        if self.global_env['options'].enable_coverage:
            src_path = os.path.abspath(
                os.path.join(self.global_env['root_dir'],
                             '..', 'src'))
            result = {}
            for gcda in ls(os.path.join(self.global_env['root_dir'], '..',
                                        'obj', 'dev', '*.gcda')):
                gcov = Run(['gcov', gcda], cwd=self.global_env['working_dir'])
                gcov_out = re.findall(
                    r"File '([^']*)'\r?\nLines[^\r\n]*\r?\nCreating '([^']*)'",
                    gcov.out,
                    re.S)
                for source, gcov_file in gcov_out:
                    if os.path.relpath(source, src_path).startswith('..'):
                        # skip files outside our source directory
                        continue

                    # skip also .ads files (not interested for the moment)
                    if source.endswith('.ads'):
                        continue

                    if source not in result:
                        result[source] = {'lines': {},
                                          'subprogram': {},
                                          'line_count': 0}

                    cur = result[source]

                    with open(os.path.join(self.global_env['working_dir'],
                                           gcov_file), 'rb') as fd:
                        content = fd.read()

                    for l in content.splitlines():
                        status, line, content = l.split(':', 2)
                        status = status.strip()
                        line = int(line.strip())

                        if line != 0:
                            cur['line_count'] += 1

                        if status == '-':
                            pass

                        elif status.startswith('#') or status.startswith('='):
                            if line not in cur['lines']:
                                cur['lines'][line] = \
                                    {'status': 'NOT_COVERED',
                                     'contents': content,
                                     'coverage': 0}
                        else:
                            if line in cur['lines']:
                                cur['lines'][line]['coverage'] += int(status)
                                cur['lines'][line]['status'] = 'COVERED'
                            else:
                                cur['lines'][line] = \
                                    {'status': 'COVERED',
                                     'contents': content,
                                     'coverage': int(status)}
            # cleanup gcda files
            rm(os.path.join(self.global_env['root_dir'],
                            '..', 'obj', 'dev', '*.gcda'))

            # Dump file to json. Note that we are not using yaml here for
            # performance issues.
            with open(os.path.join(self.global_env['output_dir'],
                                   self.test_env['test_name'] + '.cov.json'),
                      'wb') as fd:
                json.dump(result, fd)

    def analyze(self):

        with open(os.path.join(self.test_env['test_dir'], 'test.out'),
                  'rb') as fd:
            self.result.expected_output = fd.read()

        self.analyze_diff()
        self.result.msg += '(%s)' % self.test_env['title']


class UnitDriver(ShellDriver):
    def run(self):
        p = Run([os.path.join(os.path.dirname(os.environ['SHELL']),
                              'gsh_unit'),
                 './test.adas'],
                cwd=self.test_tmp, error=STDOUT)
        self.result.actual_output = p.out


class TableDriver(UnitDriver):

    def deep_compare(self, obj1, obj2):
        if isinstance(obj1, list) and isinstance(obj2, list):
            for k1, k2 in izip_longest(obj1, obj2, fillvalue=None):
                if not self.deep_compare(k1, k2):
                    self.result.diff += 'list: %s differ from %s\n' % (obj1,
                                                                       obj2)
                    return False
            return True
        elif isinstance(obj1, dict) and isinstance(obj2, dict):
            keys = set(obj1.keys()) | set(obj2.keys())
            for k1, k2 in izip_longest([obj1.get(k, None) for k in keys],
                                       [obj2.get(k, None) for k in keys]):
                if not self.deep_compare(k1, k2):
                    return False
            return True
        elif isinstance(obj1, basestring) and isinstance(obj2, basestring):
            return obj1 == obj2
        else:
            self.result.diff += '%s differ from %s' % (obj1, obj2)
            return False

    def run(self):
        p = Run([os.path.join(os.path.dirname(os.environ['SHELL']),
                              'gsh_unit'),
                 './test.adas'] + [k['input'] for k in self.test_env['table']],
                cwd=self.test_tmp, error=STDOUT)
        self.result.actual_output = p.out

    def analyze(self):
        self.output_data = yaml.load(self.result.actual_output)
        self.expected_data = self.test_env['table']

        self.result.expected_output = yaml.dump(self.test_env['table'])

        if self.deep_compare(self.output_data, self.expected_data):
            self.result.set_status('PASSED')
        else:
            self.result.set_status('FAILED')


class GSHTestsuite(Testsuite):
    TEST_SUBDIR = 'tests'
    DRIVERS = {'runshell': ShellDriver,
               'unittest': UnitDriver,
               'tabletest': TableDriver}
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
            # In coverage mode force jobs to 1
            self.main.options.mainloop_jobs = 1
        else:
            bin_dir = 'bin'
        os.environ['SHELL'] = os.path.join(self.main.options.with_gsh,
                                           bin_dir, 'gsh')

        Env().add_path(os.path.join(self.main.options.with_gsh, 'bin'))
        Env().add_path(os.path.dirname(os.environ['SHELL']))
