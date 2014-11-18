from gnatpython.testsuite import Testsuite
from gnatpython.testsuite.driver import TestDriver
from gnatpython.fileutils import sync_tree, ls, rm, mkdir, cp
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

        if self.global_env['options'].enable_coverage:
            # Set GCOV_PREFIX and GCOV_PREFIX_STRIP to select location of gcda
            # files. Note that for gcov to work we need to copy in here the
            # gcno files
            gcda_default_dir = os.path.join(self.global_env['root_dir'],
                                            '..', 'obj', 'dev')
            gcda_default_dir = \
                os.path.abspath(gcda_default_dir).replace('\\', '/')
            os.environ['GCOV_PREFIX_STRIP'] = \
                str(len(gcda_default_dir.split('/')) - 1)
            self.gcov_dir = os.path.join(self.global_env['working_dir'],
                                         os.environ['WORKER_ID'] + '.cov')
            if not os.path.isdir(self.gcov_dir):
                mkdir(self.gcov_dir)
                cp(os.path.join(gcda_default_dir, '*.gcno'),
                   self.gcov_dir)
            os.environ['GCOV_PREFIX'] = self.gcov_dir

    def run(self):
        p = Run([os.environ['SHELL'], './test.sh'],
                cwd=self.test_tmp, error=STDOUT)
        self.result.actual_output = p.out

    def analyze(self):

        with open(os.path.join(self.test_env['test_dir'], 'test.out'),
                  'rb') as fd:
            self.result.expected_output = fd.read()

        self.analyze_diff()
        self.result.msg += '(%s)' % self.test_env['title']

    def tear_down(self):
        if self.global_env['options'].enable_coverage:

            src_path = os.path.abspath(
                os.path.join(self.global_env['root_dir'],
                             '..', 'src'))
            result = {}

            cov_objs = self.test_env.get('coverage_objectives', None)

            for gcda in ls(os.path.join(self.gcov_dir, '*.gcda')):
                gcov = Run(['gcov', gcda], cwd=self.gcov_dir)
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

                    # if we have coverage objectives consider only the listed
                    # sources.
                    if cov_objs is not None:
                        if os.path.basename(source) not in cov_objs.keys():
                            continue

                    if source not in result:
                        result[source] = {'lines': {},
                                          'subprogram': {},
                                          'line_count': 0}

                    cur = result[source]

                    with open(os.path.join(self.gcov_dir,
                                           gcov_file), 'rb') as fd:
                        content = fd.read()

                    cur_fun = ''

                    for l in content.splitlines():

                        status, line, content = l.split(':', 2)
                        status = status.strip()
                        line = int(line.strip())
                        if len(cur_fun) == 0:
                            m = re.match(
                                r' *(function|procedure) *([a-zA-Z0-9_]+)',
                                content)
                            if m:
                                cur_fun = m.group(2)
                        else:
                            m = re.match(r' *end *' + cur_fun, content)
                            if m:
                                cur_fun = ''

                        if line != 0:
                            cur['line_count'] += 1

                        if cov_objs and \
                                (not cur_fun or
                                 not re.match(
                                     cov_objs[os.path.basename(source)],
                                     cur_fun)):
                            continue

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
                rm(gcda)

            # Dump file to json. Note that we are not using yaml here for
            # performance issues.
            with open(os.path.join(self.global_env['output_dir'],
                                   self.test_env['test_name'] + '.cov.json'),
                      'wb') as fd:
                json.dump(result, fd)


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
        else:
            bin_dir = 'bin'
        os.environ['SHELL'] = os.path.join(self.main.options.with_gsh,
                                           bin_dir, 'gsh')

        Env().add_path(os.path.join(self.main.options.with_gsh, 'bin'))
        Env().add_path(os.path.dirname(os.environ['SHELL']))
