#!/usr/bin/env python
"""
./testsuite.py [OPTIONS] [tests_name]

Driver for gnatpython testsuite
"""

from gnatpython.env import Env
from gnatpython.fileutils import mkdir, rm, which, ls, split_file
from gnatpython.main import Main
from gnatpython.mainloop import (MainLoop, add_mainloop_options,
                                 generate_collect_result,
                                 generate_run_testcase)
from gnatpython.testdriver import add_run_test_options
from gnatpython.reports import ReportDiff
from gnatpython.ex import Run
from glob import glob

import logging
import os
import sys
import rest
import re
logger = logging.getLogger('gnatpython.testsuite')


class Artifact(object):
    def __init__(self, dir, recurse=True):
        logger.info("create artifact %s" % dir)
        self.dir = dir
        self.type = 'req'
        self.id = self.dir.replace('/', '_').replace('\\', '_')
        if os.path.isfile(os.path.join(self.dir, 'name')):
            self.name = split_file(os.path.join(self.dir, 'name'))[0]
        else:
            self.name = self.id

        if os.path.isfile(os.path.join(self.dir, 'description')):
            self.description = "\n".join(split_file(os.path.join(self.dir, 'description')))
            self.type = 'test'

        if self.type == 'req':
            subdirs = ls(os.path.join(self.dir, '*'))
            subdirs = [ k for k in subdirs if os.path.isdir(k) ]
            self.children = [ Artifact(k) for k in subdirs ]
            if os.path.isfile(os.path.join(self.dir, 'requirement.rst')):
                self.requirement = "\n".join(split_file(os.path.join(self.dir, 'requirement.rst')))
            else:
                self.requirement = ''
        else:
            self.children = []

    def file_content(self, filename, default=''):
        result = default
        if os.path.isfile(filename):
            fd = open(filename, 'r')
            result = fd.read()
            fd.close()
        return result

    def get_test_result(self):
        if self.type == 'req':
            
            self.ok = 0
            self.failed = 0
            for k in self.children:
                ok, failed = k.get_test_result()
                self.ok += ok
                self.failed += failed
            return (self.ok, self.failed)
        else:
            result_prefix = os.path.join('out', self.dir)
            self.result = self.file_content(result_prefix + '.result', 'unknown').split(':')[0]
            self.diff = self.file_content(result_prefix + '.diff')
            self.report = self.file_content(result_prefix + '.report')
            for goal in self.coverage_goals:
                self.check_coverage_goal(goal)

            if self.result == 'OK':
                return (1, 0)
            else:
                return (0, 1)

    def rest(self, output_dir):
        fd = open(os.path.join(output_dir, self.id + '.rst'), 'w')
        fd.write('.. _' + self.id + ':\n\n')
        fd.write(rest.chapter(self.name))
        if self.type == 'test':
           fd.write(rest.section('Description'))
           fd.write(self.description + '\n')
           fd.write(rest.section('Test Status'))
           fd.write(self.result + '\n')

           if len(self.diff) > 0:
               fd.write(rest.section('Logs'))
               fd.write(rest.code_block(self.diff, 'diff'))
           if len(self.report) > 0:
               fd.write(rest.section('Output'))
               fd.write(rest.parsed_literal(self.report))
        else:
           fd.write(self.requirement + '\n')

        fd.write(rest.toctree([k.id for k in self.children], 6, True))

        subreqs = [ k for k in self.children if k.type == 'req' ]
        if len(subreqs) > 0:
            fd.write(rest.section('SubRequirements'))
            fd.write('.. csv-table::\n')
            fd.write('   :header: "name", "passed tests", "failed tests"\n\n')
            for r in subreqs:
                fd.write('   ":ref:`%s`", "%s", "%s"\n' % (r.id, r.ok, r.failed))
            fd.write('\n')

        tests = [ k for k in self.children if k.type == 'test' ]
        if len(tests) > 0:
            fd.write(rest.section('Tests'))
            fd.write('.. csv-table::\n')
            fd.write('   :header: "name", "description", "status"\n\n')
            for r in tests:
                fd.write('   ":ref:`%s`", "%s", "%s"\n' % (r.id, r.description.splitlines()[0], r.result))
            fd.write('\n')
        fd.close()
        for k in self.children:
            k.rest(output_dir)

    def get_test_list(self):
        if self.type == 'test':
            return [self.dir]
        else:
            result = []
            for k in self.children:
                result += k.get_test_list()
        return result

    def __str__(self):
        result="name: %s\ndir: %s\nchildren: %s" % (self.name, self.dir, ", ".join([k.name for k in self.children]))
        return result

    
def main():
    """Run gnatpython testsuite"""
    m = Main()
    add_mainloop_options(m)
    add_run_test_options(m)
    m.add_option("--diffs", dest="view_diffs", action="store_true",
                 default=False, help="show diffs on stdout")
    m.add_option("--old-result-dir", type="string", default=None,
                 help="Old result dir (to generate the report)")
    m.add_option("--report-only", dest="report_only", action="store_true",
                 default=False, help="only regenerate the report")

    m.parse_args()

    # Various files needed or created by the testsuite
    results_file = m.options.output_dir + '/results'
    report_file = m.options.output_dir + '/report'

    if not m.options.failed_only and not m.options.report_only:
        rm(m.options.output_dir, True)
        mkdir(m.options.output_dir)

    t=Artifact('root', recurse=False)
    t.name = 'root'
    for sub in ('shell_language', 'special-builtins', 'builtins'):
        suba=Artifact(sub)
        t.children.append(suba)

    if m.args:
        test_list = [t.strip('/') for t in m.args]
    else:
        test_list = t.get_test_list()        

    if os.path.dirname(__file__):
        os.chdir(os.path.dirname(__file__))

    env = Env()
    env.add_search_path('PYTHONPATH', os.getcwd())

    discs = [env.target.platform]

    if m.options.discs:
        discs += m.options.discs.split(',')

    collect_result = generate_collect_result(
        m.options.output_dir, results_file, m.options.view_diffs, use_basename=False)
    run_testcase = generate_run_testcase('run-test', discs, m.options)

    if not m.options.report_only:
        MainLoop(test_list, run_testcase, collect_result, m.options.mainloop_jobs)

        # Generate the report file
        ReportDiff(m.options.output_dir,
                   m.options.old_result_dir).txt_image(report_file)


    t.get_test_result()
    t.rest('doc/source') 
if __name__ == "__main__":
    main()
