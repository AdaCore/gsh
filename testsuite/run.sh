#!/bin/bash

if [ "$1" = "cov" ]; then
   export GSH="`pwd`/../obj/dev/cov/no-gmem/gsh"
   rm -f `pwd`/../obj/dev/cov/no-gmem/*.gcda
   jobs=1
else
   export GSH="`pwd`/../obj/dev/no-cov/no-gmem/gsh"
   jobs=1
fi

# delete prvious outputs
rm out/*

# First get the list of of tests
ls */*/*/test.sh | sed -e "s;/test.sh;;g" > out/test.list

# Run regular testsuite
(export PATH=`pwd`/../gnutools/bin:$PATH
mainloop --jobs $jobs -o `pwd`/out out/test.list "python;./run-test;%(name)s")


# Run memory test
#echo "Running memory tests"
#export GSH=`pwd`/../obj/dev/no-cov/gmem/gsh
#jobs=1
#mainloop --jobs $jobs -o `pwd`/output output/test.list "python;./run.py;%(name)s;mem"

(IFS=':'
 while read name result; do
    printf "%-8s %-62s %s\n" "$name" "`cat out/$name.desc`" "$result"
 done
) < output/results

cat output/status
