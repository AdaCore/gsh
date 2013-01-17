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

    if m.args:
        test_list = [t.strip('/') for t in m.args]
    else:
        test_list = ls('tests/*') 

    if os.path.dirname(__file__):
        os.chdir(os.path.dirname(__file__))

    env = Env()
    env.add_search_path('PYTHONPATH', os.getcwd())

    discs = [env.target.platform]

    if m.options.discs:
        discs += m.options.discs.split(',')

    collect_result = generate_collect_result(
        m.options.output_dir, results_file, m.options.view_diffs, use_basename=True)
    run_testcase = generate_run_testcase('run-test', discs, m.options)

    if not m.options.report_only:
        MainLoop(test_list, run_testcase, collect_result, m.options.mainloop_jobs)

        # Generate the report file
        ReportDiff(m.options.output_dir,
                   m.options.old_result_dir).txt_image(report_file)

if __name__ == "__main__":
    main()
