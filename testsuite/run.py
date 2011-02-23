#!/usr/bin/env python
import sys
import os.path
import os
from gnatpython.fileutils import echo_to_file, diff, cp
from gnatpython.ex import Run, STDOUT
from gnatpython.testdriver import *

class MyRunner(TestRunner):

if __name__ == "__main__":
   test_name = os.path.basename(sys.argv[1])
   test_dir  = sys.argv[1]
   mode = "regular"
   if len(sys.argv) > 2:
      mode=sys.argv[2]
 
   output_dir='output'
   output_file = '%s/%s.out' % (output_dir, test_name)
   expected_file = '%s/test.out' % test_dir

   shell=os.environ['GSH']
   shell_path = os.path.dirname(shell)
   os.environ['PATH'] = shell_path + ';' + os.environ['PATH']
   
   if mode == "regular":
       
       p = Run ([shell, test_dir + '/test.sh'], output=output_file, error=STDOUT)
       d = diff (expected_file, output_file)

       if len(d) == 0:
           echo_to_file ('output/%s.result' % test_name, 'PASSED')
       else:
           echo_to_file ('output/%s.result' % test_name, 'FAILED')
           cp (expected_file, 'output/%s.expected' % test_name)
       cp (test_dir + '/description', 'output/%s.desc' % test_name)

   elif mode == 'mem':
       p = Run([shell, "-c", "for i in `seq 1 2`; do . %s/test.sh; done" % test_dir])
       p = Run(['gnatmem', '4', shell], output="%s/%s.gmem" % (output_dir, test_name))
       p = Run([shell, "-c", "for i in `seq 1 20`; do . %s/test.sh; done" % test_dir])
       p = Run(['gnatmem', '4', shell], output="%s/%s.gmem10" % (output_dir, test_name))
      


